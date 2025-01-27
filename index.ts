export type show<t> = { [k in keyof t]: t[k] } & unknown;

type BranchState = {
	current: unknown[];
	groups: unknown[][];
};

type State<
	unscanned extends string,
	branches extends BranchState = BranchState,
> = {
	unscanned: unscanned;
	branches: branches;
};

type emptyBranches = {
	current: [];
	groups: [];
};

type initialState<T extends string> = State<
	T,
	emptyBranches
>;

type trimLeft<s extends string> =
	s extends `${" " | "\n" | "\t"}${infer rest}`
		? trimLeft<rest>
		: s;

type appendCurrent<s extends State<any>, value> = show<{
	groups: s["branches"]["groups"];
	current: [...s["branches"]["current"], value];
}>;

type parseNumber<s extends State<any>> =
	s["unscanned"] extends `${infer digit extends number}${infer rest}`
		? State<trimLeft<rest>, appendCurrent<s, digit>>
		: s;

type scanChar<
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
			: State<trimLeft<rest>, appendCurrent<s, char>> & {
					symbol: char;
				}
		: s;

type pushGroup<s extends State<any>> = State<
	trimLeft<scanChar<s, "(">>,
	{
		current: [];
		groups: [
			...s["branches"]["groups"],
			s["branches"]["current"],
		];
	}
>;

type popGroup<s extends State<any>> =
	s["branches"]["groups"] extends [
		...infer prevGroups extends unknown[][],
		infer lastGroup extends unknown[],
	]
		? State<
				scanChar<s, ")">,
				{
					current: [...lastGroup, s["branches"]["current"]];
					groups: prevGroups;
				}
			>
		: never;

type parse<s extends State<any>> = s["unscanned"] extends ""
	? s["branches"]["current"]
	: s["unscanned"] extends `(${string}`
		? parse<pushGroup<s>>
		: s["unscanned"] extends `)${string}`
			? parse<popGroup<s>>
			: s["unscanned"] extends `${number}${string}`
				? parse<parseNumber<s>>
				: parse<parseSymbol<s>>;

type result1 = parse<initialState<"(add (add 1 2) 3)">>;
