# provenance-portfolio

A read-only, serverless browser view of what Provenance Blockchain addresses hold:
bank balances, Figure Markets holds and commitments, HASH staking/rewards/vesting,
Democratized Prime lending positions, and who else is authorized to act on the account.

**Live:** https://franks42.github.io/provenance-portfolio/

## What it does
- Takes **public addresses only** (`pb1…`). No keys, no signing, no wallet connection.
  Everything is a GET to public Provenance REST endpoints or a read-only CosmWasm query.
- Optionally finds the **group accounts** an address controls (Figure Markets accounts are
  `x/group` policy accounts whose member is your key account).
- Values **Democratized Prime** positions as receipt shares × the pool's live liquidity index,
  for every pool of the configured contract code (new pools are picked up automatically).
- Shows **USD values** from Figure Markets' last trades (HASH, YLDS, FGRS, stablecoins); assets
  without a market are listed as unpriced rather than guessed.
- Shows **authz grants and group membership**: who can move or commit funds on the account.
- Marks Figure security-entitlement tokens (`*.figure.se`) with an **SE** badge: they are
  claims held at Figure, not the native coins.

## Privacy
- Addresses are never in this repo. You choose where they live: this tab only, this
  browser's localStorage, or the page link's `#fragment` (never sent to any server).
- The chosen API endpoints (see `endpoints.edn`) do see which addresses you query.
- On `username.github.io`, every page on that host shares browser storage. Prefer the
  tab-only or link option if that matters to you.
- CDN scripts are pinned to exact versions with Subresource Integrity hashes.

## Not shown (not on chain)
- Claims kept only in Figure's books (e.g. auction-rate/margin pool shares).
- Whether SE tokens are backed by the underlying coins.
- History: browsers can't pin a block height on these endpoints, so it's always latest state.

## Data sources
Defaults in `endpoints.edn` (Polkachu, STAVR, with others listed but off). All run the same
Provenance software; they differ in retention, CORS and limits. "Data sources…" in the page
lets you reorder, add and **test** endpoints from your own browser.

## Development
Plain files, no build step. See `CLAUDE.md` for rules (cache busters, SRI, BigInt helpers).
```sh
python3 -m http.server 8000   # then open http://127.0.0.1:8000/
```
The data layer (`src/portfolio/chain.cljs`, `tree.cljs`) also runs under nbb.

## Command line / AI agents
The same view without a browser (Node 18+):
```sh
npx nbb -cp src bin/portfolio.cljs pb1…your-address~label            # indented text
npx nbb -cp src bin/portfolio.cljs --format json pb1…a pb1…b          # data (or edn)
```
[`skills/provenance-portfolio/SKILL.md`](skills/provenance-portfolio/SKILL.md) tells an
agent how to run it and what each row means. Read-only: no keys, no signing.

Data shown is unverified REST output from third-party nodes; check important figures
against Figure Markets or an explorer.
