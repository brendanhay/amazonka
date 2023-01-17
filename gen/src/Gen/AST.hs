module Gen.AST where

import Control.Arrow ((&&&))
import qualified Control.Lens as Lens
import qualified Control.Monad.Except as Except
import Control.Monad.State.Strict (execState, modify)
import qualified Control.Monad.State.Strict as State
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.Text as Text
import Gen.AST.Cofree
import Gen.AST.Data
import Gen.AST.Override
import Gen.AST.Prefix
import Gen.AST.Subst
import Gen.Prelude
import Gen.Types

-- FIXME: Relations need to be updated by the solving step.

rewrite ::
  Versions ->
  Config ->
  Service Maybe (RefF ()) (ShapeF ()) (Waiter Id) ->
  Either String Library
rewrite _versions' _config' s' = do
  rewrittenService <- rewriteService _config' (ignore _config' (deprecate s'))
  _service' <- renderShapes _config' rewrittenService
  let nodes :: [Text]
      nodes =
        -- Select the type names from the rewritten service, so we
        -- skip over shape definitions that have no corresponding
        -- module (e.g., shapes which are mere lists of other shapes).
        rewrittenService ^.. shapes . traverse . importedTypes

      edges :: Text -> [Text]
      edges ty =
        case rewrittenService ^? shapes . Lens.at (mkId ty) . traverse of
          Nothing -> []
          Just (_ :< shape) -> case shape of
            Ptr {} -> [] -- A top-level lookup should never be a Ptr
            Struct StructF {_members} -> _members ^.. traverse . importedTypes
            List ListF {_listItem} -> _listItem ^.. importedTypes
            Map MapF {_mapKey, _mapValue} -> [_mapKey, _mapValue] ^.. traverse . importedTypes
            Enum {} -> []
            Lit {} -> []

      -- A 'Lens.Fold' over any type names that 't' will have to import.
      importedTypes :: TypeOf t => Lens.Fold t Text
      importedTypes = Lens.to (typeNames . typeOf) . traverse
        where
          typeNames = \case
            TType t _ -> [t]
            TLit {} -> []
            TNatural {} -> []
            TStream {} -> []
            TMaybe t -> typeNames t
            TSensitive t -> typeNames t
            TList t -> typeNames t
            TList1 t -> typeNames t
            TMap k v -> typeNames k ++ typeNames v

      -- Compute cuts that we will need to turn into @{-# SOURCE #-}@ imports.
      -- Ignore cuts from a type to itself; they don't cause circular imports.
      _cuts' = Set.filter (uncurry (/=)) $ breakLoops edges nodes

  _instance' <- serviceData (_service' ^. metadata) (_service' ^. retry)
  pure $ Library {_versions', _config', _service', _cuts', _instance'}

deprecate :: Service f a b c -> Service f a b c
deprecate = operations %~ HashMap.filter (not . Lens.view opDeprecated)

ignore :: Config -> Service f a b c -> Service f a b c
ignore c srv =
  srv
    & waiters %~ HashMap.filterWithKey (const . validWaiter)
    & operations %~ HashMap.mapWithKey (\k v -> if validPager k then v else (opPager .~ Nothing) v)
  where
    validWaiter k = not $ HashSet.member k (c ^. ignoredWaiters)
    validPager k = not $ HashSet.member k (c ^. ignoredPaginators)

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
    <&> fmap (attach Related rs)
    -- Apply the override configuration to the service, and default any
    -- optional fields from the JSON where needed.
    <&> (\ss -> override (cfg ^. typeOverrides) (s {_shapes = ss}))
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
  -- enum (sum) branches to avoid ambiguity.
  (x, y) <-
    prefixes (svc ^. shapes)
      -- Determine the appropriate Haskell AST type, auto deriveable instances,
      -- and fully rendered instances.
      <&> solve cfg
      -- Separate the operation input/output shapes from the .Types shapes.
      >>= separate (svc ^. operations)

  -- Prune anything that is an orphan, or not an exception
  let prune = HashMap.filter $ \s -> not (isOrphan s) || s ^. infoException

  -- Convert shape ASTs into a rendered Haskell AST declaration,
  xs <- traverse (operationData cfg svc) x
  ys <- kvTraverseMaybe (const (shapeData svc)) (prune y)
  zs <- HashMap.traverseWithKey (waiterData svc x) (svc ^. waiters)

  return
    $! svc
      { _operations = xs,
        _shapes = ys,
        _waiters = zs
      }

type MemoR = StateT (HashMap Id Relation, HashSet (Id, Direction, Id)) (Either String)

-- | Determine the relation for operation payloads, both input and output.
--
-- /Note:/ This currently doesn't operate over the free AST, since it's also
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

      unless m $ do
        Lens._2 %= HashSet.insert k
        f

    safe n =
      note
        ( "Missing shape " ++ Text.unpack (memberId n)
            ++ " when counting relations "
            ++ ", possible matches: "
            ++ partial n ss
        )
        (HashMap.lookup n ss)

-- FIXME: Necessary to update the Relation?
solve ::
  Traversable t =>
  Config ->
  t (Shape Prefixed) ->
  t (Shape Solved)
solve cfg ss = State.evalState (go ss) types
  where
    go = traverse (annotate Solved id (pure . typeOf))

    types :: HashMap Id TType
    types =
      HashMap.fromList
        . map (_replaceName &&& typeOf)
        . HashMap.elems
        . vMapMaybe _replacedBy
        $ _typeOverrides cfg

type MemoS a = StateT (HashMap Id a) (Either String)

-- | Filter the ids representing operation input/outputs from the supplied map,
-- and attach the associated shape to the appropriate operation.
separate ::
  (Show a, HasRelation a) =>
  HashMap Id (Operation Identity (RefF b) c) ->
  HashMap Id a ->
  Either
    String
    ( HashMap Id (Operation Identity (RefF a) c), -- Operations.
      HashMap Id a -- Data Types.
    )
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
            "Failure separating operation wrapper " ++ Text.unpack (memberId n)
              ++ " from "
              ++ show (HashMap.map (const ()) s)
        --
        Just x -> do
          when (d == Input || not (isShared x)) $
            State.modify' (HashMap.delete n)

          pure x

breakLoops ::
  forall node.
  Ord node =>
  -- | Edge relation
  (node -> [node]) ->
  -- | Set of nodes to explore from
  [node] ->
  Set (node, node)
breakLoops edgesFrom = (`execState` mempty) . traverse (exploreNode mempty)
  where
    exploreNode :: Set node -> node -> State (Set (node, node)) ()
    exploreNode history node = for_ (edgesFrom node) $ \n ->
      if n `elem` history
        then do
          -- We've seen this node before. Record a loop-breaker and
          -- do not recurse further.
          modify (Set.insert (node, n))
        else exploreNode (Set.insert node history) n
