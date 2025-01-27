export type show<t> = { [k in keyof t]: t[k] } & unknown;

type State<
	unscanned extends string,
	current extends unknown[] = unknown[],
	stack extends unknown[][] = unknown[][],
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
				[...s["current"], digit],
				s["stack"]
			>
		: s;

type shiftChar<
	s extends State<any>,
	ch extends string,
> = trimLeft<
	s["unscanned"] extends `${ch}${infer rest}`
		? rest
		: s["unscanned"]
>;

type reduce<s extends State<any>> = State<
	s["unscanned"],
	s["current"],
	s["stack"]
>;

type parseSymbol<s extends State<any>> =
	s["unscanned"] extends `${infer char}${infer rest}`
		? char extends " " | "(" | ")"
			? s
			: State<
					trimLeft<rest>,
					[...s["current"], char],
					s["stack"]
				> & {
					symbol: char;
				}
		: s;

type pushStack<s extends State<any>> = State<
	trimLeft<shiftChar<s, "(">>,
	[],
	[...s["stack"], s["current"]]
>;

type popStack<s extends State<any>> = s["stack"] extends [
	...infer prevStack extends unknown[][],
	infer lastItem extends unknown[],
]
	? State<
			shiftChar<s, ")">,
			[...lastItem, s["current"]],
			prevStack
		>
	: never;

type parse<s extends State<any>> = s["unscanned"] extends ""
	? s["current"]
	: s["unscanned"] extends `(${string}`
		? parse<pushStack<s>>
		: s["unscanned"] extends `)${string}`
			? parse<popStack<s>>
			: s["unscanned"] extends `${number}${string}`
				? parse<shiftNumber<s>>
				: parse<parseSymbol<s>>;

type result1 = parse<initialState<`(add (add 12 2) 3)`>>;
