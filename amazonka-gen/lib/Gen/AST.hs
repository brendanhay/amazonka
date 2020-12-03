-- |
-- Module      : Gen.AST
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject Lens.to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.AST where

import Control.Arrow ((&&&))
import qualified Control.Lens as Lens
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Graph as Graph
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Maybe as Maybe
import qualified Data.Tree as Tree
import Gen.AST.Cofree
import Gen.AST.Data
import Gen.AST.Override
import Gen.AST.Prefix
import Gen.AST.Subst
import Gen.Prelude
import Gen.Types

-- FIXME: Relations need Lens.to be updated by the solving step.

rewrite ::
  Versions ->
  Config ->
  Service Maybe (RefF ()) (ShapeF ()) (Waiter Id) ->
  Either String Library
rewrite versions cfg svc = do
  ignored <- ignoreUnusedShapes svc
  rewritten <- rewriteService cfg (deprecateOperations ignored)
  rendered <- renderShapes cfg rewritten

  Library versions cfg rendered
    <$> serviceData (rendered ^. metadata) (rendered ^. retry)

deprecateOperations :: Service f a b c -> Service f a b c
deprecateOperations = operations %~ HashMap.filter (not . Lens.view opDeprecated)

ignoreUnusedShapes ::
  Service Maybe (RefF ()) (ShapeF ()) c ->
  Either String (Service Maybe (RefF ()) (ShapeF ()) c)
ignoreUnusedShapes svc = do
  shapes' :: HashMap Id (Shape Id) <- elaborate (_shapes svc)

  let shapeIds :: HashMap Id [Id]
      shapeIds =
        HashMap.map Foldable.toList shapes'

      ( graph :: Graph,
        nodeFromVertex :: Vertex -> ((), Id, [Id]),
        vertexFromId :: Id -> Maybe Vertex
        ) =
          Graph.graphFromEdges
            . map (\(k, vs) -> ((), k, vs))
            $ HashMap.toList shapeIds

      requestResponseIds :: [Id]
      requestResponseIds =
        flip concatMap (HashMap.elems (_operations svc)) $ \op ->
          Maybe.maybeToList (_refShape <$> _opInput op :: Maybe Id)
            ++ Maybe.maybeToList (_refShape <$> _opOutput op :: Maybe Id)

      rootVertices :: [Vertex]
      rootVertices =
        Maybe.mapMaybe vertexFromId requestResponseIds

      reachableVertices :: [Vertex]
      reachableVertices =
        concatMap Tree.flatten (Graph.dfs graph rootVertices)

      reachableSet :: HashSet Id
      reachableSet =
        HashSet.fromList (map (Lens.view Lens._2 . nodeFromVertex) reachableVertices)

  pure
    svc
      { _shapes =
          HashMap.filterWithKey
            (\k _v -> HashSet.member k reachableSet)
            -- if HashSet.member k reachableSet
            --   then True
            --   else trace ("Ignoring" ++ show k) False)
            (_shapes svc)
      }

rewriteService ::
  Config ->
  Service Maybe (RefF ()) (ShapeF ()) (Waiter Id) ->
  Either String (Service Identity (RefF ()) (Shape Related) (Waiter Id))
rewriteService cfg s = do
  -- Determine which direction (input, output, or both) shapes are used.
  rs <- relations (s ^. operations) (s ^. shapes)
  -- Elaborate the shape map into a comonadic strucutre for traversing.
  elaborate (s ^. shapes)
    -- Annotate the comonadic tree with the associated
    -- bi/unidirectional (input/output/both) relation for shapes.
    >>= traverse (pure . attach Related rs)
    -- Apply the override configuration Lens.to the service, and default any
    -- optional fields from the JSON where needed.
    >>= pure . (\ss -> override (cfg ^. typeOverrides) (s {_shapes = ss}))
    -- Ensure no empty operation references exist, and that operation shapes
    -- are considered 'unique', so they can be lifted into the operation's
    -- module, separately from .Types.
    >>= substitute

renderShapes ::
  Config ->
  Service Identity (RefF ()) (Shape Related) (Waiter Id) ->
  Either String (Service Identity SData SData WData)
renderShapes cfg svc = do
  -- Generate unique prefixes for struct (product) members and
  -- enum (sum) branches Lens.to avoid ambiguity.
  (x, y) <-
    prefixes (svc ^. shapes)
      -- Determine the appropriate Haskell AST type, auto deriveable instances,
      -- and fully rendered instances.
      >>= pure . solve cfg
      -- Separate the operation input/output shapes from the .Types shapes.
      >>= separate (svc ^. operations)

  -- Prune anything that is an orphan, or not an exception
  let prune = HashMap.filter $ \s -> not (isOrphan s) || s ^. infoException

  -- Convert shape ASTs into a rendered Haskell AST declaration,
  xs <- traverse (operationData cfg svc) x
  ys <- kvTraverseMaybe (const (shapeData svc)) (prune y)
  zs <- HashMap.traverseWithKey (waiterData svc x) (svc ^. waiters)

  pure
    $! svc
      { _operations = xs,
        _shapes = ys,
        _waiters = zs
      }

type MemoR = StateT (HashMap Id Relation, HashSet (Id, Direction, Id)) (Either String)

-- | Determine the relation for operation payloads, both input and output.
--
-- /Note:/ This currently doesn't operate Lens.over the free AST, since it's also
-- used by 'setDefaults'.
relations ::
  Show a =>
  HashMap Id (Operation Maybe (RefF b) c) ->
  HashMap Id (ShapeF a) ->
  Either String (HashMap Id Relation)
relations os ss = fst <$> State.execStateT (traverse go os) (mempty, mempty)
  where
    -- FIXME: opName here is incorrect as a parent.
    go :: Operation Maybe (RefF a) b -> MemoR ()
    go o =
      count Nothing Input (o ^? opInput . Lens._Just . refShape)
        >> count Nothing Output (o ^? opOutput . Lens._Just . refShape)
      
    count :: Maybe Id -> Direction -> Maybe Id -> MemoR ()
    count _ _ Nothing = pure ()
    count p d (Just n) = do
      Lens._1 %= HashMap.insertWith (<>) n (mkRelation p d)
      
      check p d n $ do
        s <- lift (safe n)
        shape n d s

    shape :: Id -> Direction -> ShapeF a -> MemoR ()
    shape p d =
      mapM_ (count (Just p) d . Just . Lens.view refShape)
        . Lens.toListOf references

    -- Ensure cyclic dependencies are only checked once per direction/parent.
    check Nothing _ _ f = f
    check (Just p) d n f = do
      let k = (p, d, n)
      m <- Lens.uses Lens._2 (HashSet.member k)
      if m
        then pure ()
        else Lens._2 %= HashSet.insert k >> f

    safe n =
      case HashMap.lookup n ss of
        Nothing ->
          Left $ "Missing shape "
            ++ show n
            ++ " when counting relations "
            ++ ", possible matches: "
            ++ show (partial n ss)
        Just ok ->
          Right ok

-- FIXME: Necessary Lens.to update the Relation?
solve ::
  Traversable t =>
  Config ->
  t (Shape Prefixed) ->
  t (Shape Solved)
solve cfg ss = State.evalState (go ss) (replaced typeOf cfg)
  where
    go = traverse (annotate Solved id (pure . typeOf))

    replaced :: (Replace -> a) -> Config -> HashMap Id a
    replaced f =
      HashMap.fromList
        . map (_replaceName &&& f)
        . HashMap.elems
        . vMapMaybe _replacedBy
        . _typeOverrides

type MemoS a = StateT (HashMap Id a) (Either String)

-- | Filter the ids representing operation input/outputs from the supplied map,
-- and attach the associated shape Lens.to the appropriate operation.
--
-- Pures either an error result or the operations paired with
-- the respective data types.
separate ::
  (Show a, HasRelation a) =>
  HashMap Id (Operation Identity (RefF b) c) ->
  HashMap Id a ->
  Either String (HashMap Id (Operation Identity (RefF a) c), HashMap Id a)
separate os = State.runStateT (traverse go os)
  where
    go ::
      (HasRelation b) =>
      Operation Identity (RefF a) c ->
      MemoS b (Operation Identity (RefF b) c)
    go o = do
      x <- remove Input (inputName o)
      y <- remove Output (outputName o)

      pure
        $! o
          { _opInput = Identity (o ^. opInput . _Identity & refAnn .~ x),
            _opOutput = Identity (o ^. opOutput . _Identity & refAnn .~ y)
          }

    remove :: HasRelation a => Direction -> Id -> MemoS a a
    remove d n = do
      s <- State.get

      case HashMap.lookup n s of
        Nothing ->
          Except.throwError $
            "Failure separating operation wrapper "
              ++ show n
              ++ " from "
              ++ show (HashMap.map (const ()) s)
        --
        Just x -> do
          when (d == Input || not (isShared x)) $
            State.modify (HashMap.delete n)
            
          pure x
