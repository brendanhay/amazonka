-- Module      : Gen.AST
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.AST where

import Control.Arrow
import Control.Error
import Control.Lens
import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Gen.AST.Cofree
import Gen.AST.Data
import Gen.AST.Override
import Gen.AST.Prefix
import Gen.AST.Subst
import Gen.Formatting hiding (replaced)
import Gen.Types

-- FIXME: Relations need to be updated by the solving step.

rewrite ::
  Versions ->
  Config ->
  Service Maybe (RefF ()) (ShapeF ()) (Waiter Id) ->
  Either Error Library
rewrite v cfg s' = do
  s <- rewriteService cfg (ignore cfg (deprecate s')) >>= renderShapes cfg
  Library v cfg s <$> serviceData (s ^. metadata) (s ^. retry)

deprecate :: Service f a b c -> Service f a b c
deprecate = operations %~ Map.filter (not . view opDeprecated)

ignore :: Config -> Service f a b c -> Service f a b c
ignore c srv =
  srv
    & waiters %~ Map.filterWithKey (const . validWaiter)
    & operations %~ Map.mapWithKey (\k v -> if validPager k then v else (opPager .~ Nothing) v)
  where
    validWaiter k = not $ Set.member k (c ^. ignoredWaiters)
    validPager k = not $ Set.member k (c ^. ignoredPaginators)

rewriteService ::
  Config ->
  Service Maybe (RefF ()) (ShapeF ()) (Waiter Id) ->
  Either Error (Service Identity (RefF ()) (Shape Related) (Waiter Id))
rewriteService cfg s = do
  -- Determine which direction (input, output, or both) shapes are used.
  rs <- relations (s ^. operations) (s ^. shapes)
  -- Elaborate the shape map into a comonadic strucutre for traversing.
  elaborate (s ^. shapes)
    -- Annotate the comonadic tree with the associated
    -- bi/unidirectional (input/output/both) relation for shapes.
    >>= traverse (pure . attach Related rs)
    -- Apply the override configuration to the service, and default any
    -- optional fields from the JSON where needed.
    >>= return . (\ss -> override (cfg ^. typeOverrides) (s {_shapes = ss}))
    -- Ensure no empty operation references exist, and that operation shapes
    -- are considered 'unique', so they can be lifted into the operation's
    -- module, separately from .Types.
    >>= substitute

renderShapes ::
  Config ->
  Service Identity (RefF ()) (Shape Related) (Waiter Id) ->
  Either Error (Service Identity SData SData WData)
renderShapes cfg svc = do
  -- Generate unique prefixes for struct (product) members and
  -- enum (sum) branches to avoid ambiguity.
  (x, y) <-
    prefixes (svc ^. shapes)
      -- Determine the appropriate Haskell AST type, auto deriveable instances,
      -- and fully rendered instances.
      >>= return . solve cfg
      -- Separate the operation input/output shapes from the .Types shapes.
      >>= separate (svc ^. operations)

  -- Prune anything that is an orphan, or not an exception
  let prune = Map.filter $ \s -> not (isOrphan s) || s ^. infoException

  -- Convert shape ASTs into a rendered Haskell AST declaration,
  xs <- traverse (operationData cfg svc) x
  ys <- kvTraverseMaybe (const (shapeData svc)) (prune y)
  zs <- Map.traverseWithKey (waiterData svc x) (svc ^. waiters)

  return
    $! svc
      { _operations = xs,
        _shapes = ys,
        _waiters = zs
      }

type MemoR = StateT (Map Id Relation, Set (Id, Direction, Id)) (Either Error)

-- | Determine the relation for operation payloads, both input and output.
--
-- /Note:/ This currently doesn't operate over the free AST, since it's also
-- used by 'setDefaults'.
relations ::
  Show a =>
  Map Id (Operation Maybe (RefF b) c) ->
  Map Id (ShapeF a) ->
  Either Error (Map Id Relation)
relations os ss = fst <$> execStateT (traverse go os) (mempty, mempty)
  where
    -- FIXME: opName here is incorrect as a parent.
    go :: Operation Maybe (RefF a) b -> MemoR ()
    go o =
      count Nothing Input (o ^? opInput . _Just . refShape)
        >> count Nothing Output (o ^? opOutput . _Just . refShape)
    count :: Maybe Id -> Direction -> Maybe Id -> MemoR ()
    count _ _ Nothing = pure ()
    count p d (Just n) = do
      _1 %= Map.insertWith (<>) n (mkRelation p d)
      check p d n $ do
        s <- lift (safe n)
        shape n d s

    shape :: Id -> Direction -> ShapeF a -> MemoR ()
    shape p d =
      mapM_ (count (Just p) d . Just . view refShape)
        . toListOf references

    -- Ensure cyclic dependencies are only checked once per direction/parent.
    check Nothing _ _ f = f
    check (Just p) d n f = do
      let k = (p, d, n)
      m <- uses _2 (Set.member k)
      if m
        then pure ()
        else _2 %= Set.insert k >> f

    safe n =
      note
        ( format
            ( "Missing shape " % iprimary
                % " when counting relations "
                % ", possible matches: "
                % partial
            )
            n
            (n, ss)
        )
        (Map.lookup n ss)

-- FIXME: Necessary to update the Relation?
solve ::
  Traversable t =>
  Config ->
  t (Shape Prefixed) ->
  t (Shape Solved)
solve cfg ss = evalState (go ss) (replaced typeOf cfg)
  where
    go = traverse (annotate Solved id (pure . typeOf))

    replaced :: (Replace -> a) -> Config -> Map Id a
    replaced f =
      Map.fromList
        . map (_replaceName &&& f)
        . Map.elems
        . vMapMaybe _replacedBy
        . _typeOverrides

type MemoS a = StateT (Map Id a) (Either Error)

-- | Filter the ids representing operation input/outputs from the supplied map,
-- and attach the associated shape to the appropriate operation.
separate ::
  (Show a, HasRelation a) =>
  Map Id (Operation Identity (RefF b) c) ->
  Map Id a ->
  Either
    Error
    ( Map Id (Operation Identity (RefF a) c), -- Operations.
      Map Id a -- Data Types.
    )
separate os = runStateT (traverse go os)
  where
    go ::
      (HasRelation b) =>
      Operation Identity (RefF a) c ->
      MemoS b (Operation Identity (RefF b) c)
    go o = do
      x <- remove Input (inputName o)
      y <- remove Output (outputName o)

      return
        $! o
          { _opInput = Identity (o ^. opInput . _Identity & refAnn .~ x),
            _opOutput = Identity (o ^. opOutput . _Identity & refAnn .~ y)
          }

    remove :: HasRelation a => Direction -> Id -> MemoS a a
    remove d n = do
      s <- get
      let m =
            "Failure separating operation wrapper " % iprimary
              % " from "
              % shown
      case Map.lookup n s of
        Nothing -> throwError $ format m n (Map.map (const ()) s)
        Just x -> do
          when (d == Input || not (isShared x)) $
            modify (Map.delete n)
          return x
