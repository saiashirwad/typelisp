export type show<t> = {
	[k in keyof t]: t[k];
} & unknown;

type Token =
	| { type: "number"; value: number }
	| { type: "symbol"; value: string }
	| { type: "string"; value: string }
	| { type: "boolean"; value: boolean }
	| { type: "list"; value: Token[] }
	| { type: "definition"; name: Token; value: Token }
	| {
			type: "let";
			bindings: { name: Token; value: Token }[];
			body: Token;
	  };

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

type isSymbol<
	T extends Token,
	V extends string,
> = T extends {
	type: "symbol";
	value: V;
}
	? true
	: false;

type transformDefinition<T extends Token> = T extends {
	type: "list";
	value: [
		infer First extends Token,
		infer Name extends Token,
		infer Value extends Token,
	];
}
	? isSymbol<First, "define"> extends true
		? { type: "definition"; name: Name; value: Value }
		: T
	: T;

type extractBindings<T extends Token[]> = T extends [
	{
		type: "list";
		value: [
			infer name extends Token,
			infer value extends Token,
		];
	},
	...infer rest extends Token[],
]
	? [{ name: name; value: value }, ...extractBindings<rest>]
	: [];

type transformLet<t extends Token> = t extends {
	type: "list";
	value: [
		infer first extends Token,
		{ type: "list"; value: infer bindings extends Token[] },
		infer body extends Token,
	];
}
	? isSymbol<first, "let"> extends true
		? {
				type: "let";
				bindings: extractBindings<bindings>;
				body: body;
			}
		: t
	: t;

type transformSpecialForms<T extends Token> = transformLet<
	transformDefinition<T>
>;

type popStack<s extends State<any>> = s["stack"] extends [
	...infer stack extends Token[][],
	infer tail extends Token[],
]
	? State<
			consumeChar<s, ")" | " ">,
			[
				...tail,
				transformSpecialForms<{
					type: "list";
					value: s["current"];
				}>,
			],
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

type parseResult = parse<initialState<"(define x 5)">>;

type parseLetResult = parse<
	initialState<"(let ((x 1) (y 2)) (+ x y))">
>;

type Environment = {
	bindings: Record<string, Token>;
	parent: Environment | null;
};

type emptyEnv = {
	bindings: {};
	parent: null;
};

type lookupEnv<
	env extends Environment,
	key extends string,
> = env["bindings"][key] extends Token
	? env["bindings"][key]
	: env["parent"] extends Environment
		? lookupEnv<env["parent"], key>
		: never;

type createScopedEnv<
	env extends Environment,
	key extends string,
	value extends Token,
> = {
	bindings: {
		[k in key]: value;
	} & env["bindings"];
	parent: env;
};

type extendEnv<
	env extends Environment,
	key extends string,
	value extends Token,
> = {
	bindings: show<
		{
			[k in key]: value;
		} & env["bindings"]
	>;
	parent: env["parent"];
};

type buildTuple<
	n extends number,
	acc extends any[] = [],
> = acc["length"] extends n
	? acc
	: buildTuple<n, [...acc, 0]>;

type add<a extends number, b extends number> = [
	...buildTuple<a>,
	...buildTuple<b>,
]["length"];

type subtract<
	a extends number,
	b extends number,
> = buildTuple<a> extends [...buildTuple<b>, ...infer rest]
	? rest["length"]
	: never;

type eval<
	tok extends Token,
	env extends Environment,
> = tok extends {
	type: "number";
}
	? tok
	: tok extends {
				type: "symbol";
				value: infer sym extends string;
			}
		? lookupEnv<env, sym>
		: tok extends {
					type: "list";
					value: [
						infer first extends Token,
						...infer rest extends Token[],
					];
				}
			? evalApplication<first, rest, env>
			: tok extends {
						type: "definition";
						name: {
							type: "symbol";
							value: infer n extends string;
						};
						value: infer val extends Token;
					}
				? extendEnv<
						env,
						n,
						eval<val, env> extends Token
							? eval<val, env>
							: never
					>
				: tok extends {
							type: "let";
							bindings: infer bindings extends Array<{
								name: Token;
								value: Token;
							}>;
							body: infer body extends Token;
						}
					? evalLet<bindings, body, env>
					: never;

type evalApplication<
	fn extends Token,
	args extends Token[],
	env extends Environment,
> = fn extends { type: "symbol"; value: "add" }
	? args extends [
			infer a extends Token,
			infer b extends Token,
		]
		? eval<a, env> extends {
				type: "number";
				value: infer x extends number;
			}
			? eval<b, env> extends {
					type: "number";
					value: infer y extends number;
				}
				? { type: "number"; value: add<x, y> }
				: never
			: never
		: never
	: fn extends { type: "symbol"; value: "sub" }
		? args extends [
				infer a extends Token,
				infer b extends Token,
			]
			? eval<a, env> extends {
					type: "number";
					value: infer x extends number;
				}
				? eval<b, env> extends {
						type: "number";
						value: infer y extends number;
					}
					? { type: "number"; value: subtract<x, y> }
					: never
				: never
			: never
		: never;

type evalLet<
	bindings extends Array<{ name: Token; value: Token }>,
	body extends Token,
	env extends Environment,
> = bindings extends []
	? eval<body, env>
	: bindings extends [
				{
					name: {
						type: "symbol";
						value: infer n extends string;
					};
					value: infer val extends Token;
				},
				...infer rest extends Array<{
					name: Token;
					value: Token;
				}>,
			]
		? evalLet<
				rest,
				body,
				createScopedEnv<
					env,
					n,
					eval<val, env> extends Token
						? eval<val, env>
						: never
				>
			>
		: never;

type initialEnv = {
	bindings: {
		add: { type: "symbol"; value: "add" };
		sub: { type: "symbol"; value: "sub" };
	};
	parent: emptyEnv;
};

type result = eval<
	parse<initialState<"(add (sub 4 3) 4)">>[0],
	initialEnv
>;
