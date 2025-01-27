export type show<t> = {
	[k in keyof t]: t[k];
} & unknown;

type Token =
	| { type: "number"; value: number }
	| { type: "symbol"; value: string }
	| { type: "string"; value: string }
	| { type: "boolean"; value: boolean }
	| { type: "list"; value: Token[] };

type State<
	unscanned extends string,
	current extends Token[] = Token[],
	stack extends Token[][] = Token[][],
> = {
	unscanned: unscanned;
	current: current;
	stack: stack;
};

type initialState<T extends string> = State<T, [], []>;

type trimLeft<s extends string> =
	s extends `${" " | "\n" | "\t"}${infer rest}`
		? trimLeft<rest>
		: s;

type shiftNumber<s extends State<any>> =
	s["unscanned"] extends `${infer digit extends number}${infer rest}`
		? State<
				trimLeft<rest>,
				[...s["current"], { type: "number"; value: digit }],
				s["stack"]
			>
		: s;

type isDelimiter<char extends string> = char extends
	| " "
	| "("
	| ")"
	| '"'
	? true
	: false;

type accumulateSymbol<
	str extends string,
	acc extends string = "",
> = str extends `${infer char}${infer rest}`
	? isDelimiter<char> extends true
		? [acc, str]
		: accumulateSymbol<rest, `${acc}${char}`>
	: [acc, str];

type shiftSymbol<s extends State<any>> =
	s["unscanned"] extends `${infer char}${infer rest}`
		? isDelimiter<char> extends true
			? s
			: accumulateSymbol<s["unscanned"]> extends [
						infer sym extends string,
						infer remaining extends string,
					]
				? State<
						trimLeft<remaining>,
						[
							...s["current"],
							{ type: "symbol"; value: sym },
						],
						s["stack"]
					>
				: s
		: s;

type shiftString<s extends State<any>> =
	s["unscanned"] extends `"${infer content}"${infer rest}`
		? State<
				trimLeft<rest>,
				[
					...s["current"],
					{ type: "string"; value: content },
				],
				s["stack"]
			>
		: s;

type consumeChar<
	s extends State<any>,
	ch extends string | number,
> = trimLeft<
	s["unscanned"] extends `${ch}${infer rest}`
		? rest
		: s["unscanned"]
>;

type pushStack<s extends State<any>> = State<
	consumeChar<s, "(">,
	[],
	[...s["stack"], s["current"]]
>;

type popStack<s extends State<any>> = s["stack"] extends [
	...infer stack extends Token[][],
	infer tail extends Token[],
]
	? State<
			consumeChar<s, ")" | " ">,
			[...tail, { type: "list"; value: s["current"] }],
			stack
		>
	: never;

type parse<s extends State<any>> = s["unscanned"] extends ""
	? s["current"][0] extends { type: "list" }
		? s["current"]
		: [{ type: "list"; value: s["current"] }]
	: s["unscanned"] extends `(${string}`
		? parse<pushStack<s>>
		: s["unscanned"] extends `${")" | " "}${string}`
			? parse<popStack<s>>
			: s["unscanned"] extends `"${string}`
				? parse<shiftString<s>>
				: s["unscanned"] extends `${number}${string}`
					? parse<shiftNumber<s>>
					: parse<shiftSymbol<s>>;

type result1 = parse<initialState<"(add (add 2 3) 5)">>;
type result2 = parse<initialState<'(concat "hello" true)'>>;

type test1 = show<result1>;
type test2 = show<result2>;
