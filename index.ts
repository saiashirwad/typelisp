export type show<t> = { [k in keyof t]: t[k] } & unknown;

type State<
	unscanned extends string,
	current extends unknown[] = unknown[],
	groups extends unknown[][] = unknown[][],
> = {
	unscanned: unscanned;
	current: current;
	groups: groups;
};

type initialState<T extends string> = State<T, [], []>;

type trimLeft<s extends string> =
	s extends `${" " | "\n" | "\t"}${infer rest}`
		? trimLeft<rest>
		: s;

type pushNumber<s extends State<any>> =
	s["unscanned"] extends `${infer digit extends number}${infer rest}`
		? State<
				trimLeft<rest>,
				[...s["current"], digit],
				s["groups"]
			>
		: s;

type pushChar<
	s extends State<any>,
	ch extends string,
> = trimLeft<
	s["unscanned"] extends `${ch}${infer rest}`
		? rest
		: s["unscanned"]
>;

type parseSymbol<s extends State<any>> =
	s["unscanned"] extends `${infer char}${infer rest}`
		? char extends " " | "(" | ")"
			? s
			: State<
					trimLeft<rest>,
					[...s["current"], char],
					s["groups"]
				> & {
					symbol: char;
				}
		: s;

type pushGroup<s extends State<any>> = State<
	trimLeft<pushChar<s, "(">>,
	[],
	[...s["groups"], s["current"]]
>;

type popGroup<s extends State<any>> = s["groups"] extends [
	...infer prevGroups extends unknown[][],
	infer lastGroup extends unknown[],
]
	? State<
			pushChar<s, ")">,
			[...lastGroup, s["current"]],
			prevGroups
		>
	: never;

type parse<s extends State<any>> = s["unscanned"] extends ""
	? s["current"]
	: s["unscanned"] extends `(${string}`
		? parse<pushGroup<s>>
		: s["unscanned"] extends `)${string}`
			? parse<popGroup<s>>
			: s["unscanned"] extends `${number}${string}`
				? parse<pushNumber<s>>
				: parse<parseSymbol<s>>;

type result1 = parse<initialState<`(add (add 12 2) 3)`>>;
