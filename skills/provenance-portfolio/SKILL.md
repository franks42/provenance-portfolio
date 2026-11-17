---
name: provenance-portfolio
description: Read-only lookup of what Provenance Blockchain addresses hold, including Figure Markets balances, HASH staking and vesting, and Democratized Prime lending, valued in USD. Use when asked about holdings, balances, vesting, staking, Prime positions or Figure accounts for a pb1… address.
---

# Provenance portfolio lookup

The `bin/portfolio.cljs` script in the
[provenance-portfolio](https://github.com/franks42/provenance-portfolio) repo
reports what Provenance addresses hold. It uses the same code as the project's
web page (`src/portfolio/chain.cljs` and `tree.cljs`), so both always agree.

It is read-only: public REST endpoints and CosmWasm smart queries, no keys, no
signing. It cannot move funds, and you should not try to make it.

## Running it

Needs Node 18+ (nbb is fetched by `npx`). From the repo root:

```
npx nbb -cp src bin/portfolio.cljs [--format tree|json|edn] [--no-groups] [--no-prices] ADDRESS[~label] ...
```

- `tree` (default): indented text for people.
- `json` / `edn`: the same tree as data. Use this when you need to compute or compare.
- Group accounts that an address controls (Figure Markets accounts are x/group
  policy accounts) are found automatically; `--no-groups` turns that off.
- Endpoints, prices and asset classes come from `endpoints.edn`
  (`--config PATH` to use another file).
- A run makes roughly 50–150 HTTP requests and takes 10–30 s.

## Privacy

Addresses are the user's data. Pass them as arguments only. Don't write them
into files, commits or logs unless the user asks. Refer to accounts by the last
4 characters of the address (`…ab12`), which is how the output labels them.

## Reading the output

Tree: Portfolio > accounts (largest first) > asset classes > assets > breakdown,
then "All accounts" (the same holdings combined across accounts).

Asset classes: Stocks, Stable / fiat, Tradable crypto (has a Figure Markets
price), Untradable crypto (no Figure market), Prime lending.

Per asset:
- **delegated**: staked (for vesting accounts split into *unvested (locked)* and
  *vested*), unbonding with completion dates, and unclaimed rewards.
- **in wallet**:
  - *available*: can be sent or traded now (the chain's spendable balance).
  - *locked (unvested)*: vesting lock not covered by staked tokens.
  - *held*: on hold for other reasons.
  - *committed to Figure Markets*: the user's trading balance on Figure's
    exchange. It can be moved too, after releasing it, which cancels open
    orders on those funds.
  - *unreconciled*, if present: wallet ≠ available + locked + held. Report it;
    it means the model is missing something.
- **free to move** (overlay): available + committed + rewards, with the extra
  step each one needs. Answers "how much could I move today".
- **vesting schedule** (overlay): original grant, vested so far, unvested,
  per-day rate and end date. The chain applies the vesting lock to staked
  tokens first.

Overlays restate other numbers and are never added into totals.

Forms of one asset are grouped under one row with "N forms":
- *via IBC from <chain>*: the bridged token itself, e.g. USDC from noble-1.
- *Figure entitlement (SE)*: a claim on an asset Figure holds in custody
  (`*.figure.se`), not the coin itself.
- *Figure Markets cash*: `*.trading` USD.

Prime lending rows show the value of the receipt shares (shares × liquidity
index), the lender rate and utilization. A row flagged `!` (JSON
`"warning": true`) is above its rate-model kink: high rate, but little
unborrowed cash, so withdrawals may have to wait.

## How far to trust the values

- USD prices are Figure Markets last trades. YLDS and USD are fixed at $1.
- `est.` (JSON `price-estimated-from`): no Figure market. The price is an
  external spot price (Coinbase by default), e.g. for ETH/SOL/XRP/UNI
  entitlements Figure no longer trades.
- `assumed` (JSON `price-assumed`): valued at $1 by configuration, e.g.
  USDC/USDT without a market, and figureomni (Figure's tokenized USD deposit
  at an omnibus bank, in cents).
- `no market` / `"unpriced": true`: not in the USD totals.
- Data is unverified REST from public nodes, not light-client verified.
  `errors` on an account means some sections failed and are missing from its
  totals.
- Rounded USD values can differ by a few cents from the totals, which are
  computed before rounding.

## Not visible on chain

Claims kept only in Figure's own books (e.g. auction-rate/margin pool shares),
and what backs the SE tokens. Say so instead of guessing.
