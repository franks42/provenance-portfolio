# Provenance Portfolio Viewer

## Overview
Static, read-only browser app (Scittle + Reagent, served by GitHub Pages) that shows
what Provenance Blockchain addresses hold, including Figure Markets balances and
Democratized Prime lending positions. No server, no keys, no signing.

## Critical Rules
- **Read-only, always.** Only HTTP GETs and CosmWasm *smart queries*. Never add signing,
  key handling, or tx broadcast code.
- **No personal data in the repo.** No real wallet addresses, balances, or screenshots
  of them in code, tests, docs, or commit messages. Addresses live only in the user's
  browser (tab memory, localStorage, or URL fragment, by the user's choice).
- **Refer to accounts by the last 4 characters** of the address (`…ab12`) in UI and docs.
- **Cache busters:** ALWAYS bump `?v=X.Y.Z` on the `.cljs` script tags in `index.html`
  when changing ANY `.cljs` file. Browsers will serve stale code otherwise.
- **Pinned CDN scripts with SRI.** Exact versions + `integrity` hashes in `index.html`.
  When upgrading, recompute: `curl -s URL | openssl dgst -sha384 -binary | openssl base64 -A`
  and verify the CDN file matches the npm tarball.
- **BigInt for amounts.** Amounts are BigInt base units. Under SCI, `+ - * / < =` work on
  BigInt, but `quot`, `max`, `compare`, `zero?` do NOT (`(zero? 0n)` is false!). Use the
  helpers in `portfolio.chain` (`big-zero?`, `big-pos?`, `big-max`, `dec->big`, `big->dec`).
- **Don't hardcode endpoints.** They live in `endpoints.edn`; users can override in the page.
- **Never name a local `quote`** (or other special forms: `do`, `if`, `let`, `fn`, `def`, `var`, `new`, `set!`):
  `(quote m)` silently returns the symbol `m` instead of calling your function.

## Architecture
- `index.html`: pinned React 18 UMD + Scittle core/Reagent/Promesa plugins, then the app scripts.
- `src/portfolio/chain.cljs`: data layer. Runs unchanged in Scittle **and nbb** (uses only
  `js/fetch` + promesa). Endpoint fallback, 429 backoff, concurrency limiter, denom metadata,
  bank/hold/commitments/staking/rewards/vesting/authz/groups, Prime pool discovery + valuation.
- `src/portfolio/tree.cljs`: pure data. Turns reports into the tree (accounts, asset classes,
  wallet/delegation breakdown, vesting overlay, free to move, forms of one asset, Prime notes).
  Runs in Scittle and nbb. Domain knowledge belongs here or in chain.cljs, not in ui.cljs,
  so the CLI gets it too.
- `src/portfolio/ui.cljs`: Reagent UI, storage-mode handling, endpoint settings and self-test.
- `bin/portfolio.cljs`: nbb CLI for agents/scripts (`--format tree|json|edn`), same chain+tree code.
- `skills/provenance-portfolio/SKILL.md`: how an agent runs the CLI and reads its output.
  Update it when the meaning of a tree row changes.
- `endpoints.edn`: endpoints, Prime code ids, prices (fixed/assumed/estimated), asset classes
  (`:group-as`), probe pool for the self-test.

## Data sources
Public Provenance REST (LCD) endpoints that allow browser CORS access. The API is the same
everywhere (same chain software); operators differ in history retention, CORS, limits.
- Browsers can't send `x-cosmos-block-height` (custom-header preflight is refused), so the
  app always shows **latest** state.
- The Foundation endpoints (`api.provenance.io`, `rpc.provenance.io`) are CORS-allowlisted
  and therefore unusable from this page.
- USD prices: Figure Markets public markets API (`:prices` in `endpoints.edn`, CORS reflects any
  origin). `X-USD` markets price directly; `X-YLDS` via YLDS-USD. ETH/SOL/XRP/UNI entitlements have
  no market since Figure ended trading in them, so they're listed as unpriced.
- Prime pools: CosmWasm code id(s) in `:prime-code-ids`; receipt-token address is
  `get_state → contract.atca`; value = CW20 shares × `get_reserve → reserve.liquidity_index`.

## Testing
- Data layer under nbb (same SCI as Scittle):
  `npx nbb -cp src your-test.cljs` with `(require '[portfolio.chain :as c])`.
  Use public addresses (e.g. pool contracts, or holders from a receipt token's
  `{"all_accounts":{}}`), never personal ones.
- Browser: `python3 -m http.server` in the repo root, open `http://127.0.0.1:8000/`.
  The endpoint "Test" buttons check CORS for real.

## Debugging
- Browser console: `scittle.core.eval_string('(clj->js (:result @portfolio.ui/app))')`
