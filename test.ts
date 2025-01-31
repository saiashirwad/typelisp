const upgradeOptions: Record<
  string,
  (state: string[]) => string[]
> = {
  regular: (state) => [...state, "pro", "lifetime"],
  pro: (state) => [...state, "lifetime"],
  monthly: (state) => [...state, "annual"],
  noAi: (state) => [...state, "ai-monthly", "ai-annual"],
  "ai-monthly": (state) => [...state, "ai-annual"],
  overdue: () => ["pay-bill"],
  paused: () => ["resume"],
};

const flags = ["lifetime", "noAi"];
let state: string[] = [];
for (const k of Object.keys(upgradeOptions)) {
  if (flags.includes(k)) {
    state = upgradeOptions[k](state);
  }
}

state = [...new Set(state)];

console.log(state);
