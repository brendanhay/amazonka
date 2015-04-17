{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- Module      : Gen.Override
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Override where

import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Data.Bifunctor
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import           Data.Default.Class
import           Data.Foldable              (foldl')
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as Set
import           Data.List                  (intercalate)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Manipulate
import           Debug.Trace
import           Gen.Model                  hiding (Name, State)
import           Gen.OrdMap                 (OrdMap)
import qualified Gen.OrdMap                 as OrdMap
import           Gen.Text                   (safeHead)
import           Gen.Types                  hiding (override)

type PS = HashMap (CI Text) (HashSet (CI Text))

-- -- | Replace operation input/output references with their respective shapes,
-- -- removing the shape from the service if they are not shared.
-- subst :: TextSet
--       -> Service (Typed Shape) (Untyped Ref)
--       -> Service (Typed Shape) (Typed Shape)
-- subst sh s =
--     let (x, y) = runState (Map.traverseWithKey go os) ss
--      in s & svcOperations .~ x & svcShapes .~ y
--   where
--     os = s ^. svcOperations
--     ss = s ^. svcShapes

--     go :: MonadError String m
--        => Text
--        -> Operation (Untyped Ref)
--        -> StateT (TextMap (Typed Shape)) m (Operation (Typed Shape))
--     go n o = do
--         rq <- update n                 (o ^. operInput)
--         rs <- update (n <> "Response") (o ^. operOutput)
--         return $! o
--             & operInput  .~ rq
--             & operOutput .~ rs

--     update :: MonadError String m
--            => Text
--            -> Untyped Ref
--            -> StateT (TextMap (Typed Shape)) m (Typed Shape)
--     update n r = do
--         let k = r ^. refShape
--         s <- gets (Map.lookup k)
--         case s of
--             Just x | Set.member k sh -> copy n   r s
--             Just x                   -> move n k r s
--             _                        ->
--                 throwError $ "Unable to subst " ++ show n

--     -- 1. if not shared, then rename the shape and delete it from the state.
--     -- 2. otherwise, copy the shape
--     -- 3. adjust the shape to suit being a request/response

--     -- move :: Text
--     --      -> Text
--     --      -> Ref
--     --      -> Typed Shape
--     --      -> StateT (HashMap Text (Typed Shape)) m (Maybe Ref)
--     move n k r s = modify (Map.delete k) >> copy n r d

--     -- copy :: Text
--     --      -> Ref
--     --      -> Typed Shape
--     --      -> State (HashMap Text Data) (Maybe Ref)
--     copy n r s = undefined -- do
--         -- modify (Map.insert n (dataRename n d))
--         -- return (Just (r & refShape .~ n))

-- | Apply the override rulset to shapes and their respective fields.
rules :: TextMap Rules -> TextMap (Untyped Shape) -> TextMap (Untyped Shape)
rules o = Map.foldlWithKey' go mempty
  where
    go acc n = shape (fromMaybe def (Map.lookup n o)) acc n

     -- FIXME: Renaming should additionally operate over
     -- the operation input/output.

    shape :: Rules
          -> TextMap (Untyped Shape)
          -> Text
          -> (Untyped Shape)
          -> TextMap (Untyped Shape)
    shape rs acc n s
        | Map.member n replacedBy          = acc
        | Just x <- Map.lookup n renamedTo = shape rs acc x s
        | otherwise                        = Map.insert n (rules s) acc
      where
        rules = requireFields
              . optionalFields
              . renameFields
              . retypeFields
              . prefixEnum
              . appendEnum

        requireFields :: Untyped Shape -> Untyped Shape
        requireFields = _SStruct . structRequired
            %~ (<> _ruleRequired rs)

        optionalFields = _SStruct . structRequired
            %~ (`Set.difference` _ruleOptional rs)

        renameFields :: Untyped Shape -> Untyped Shape
        renameFields = _SStruct . structMembers %~ first f
          where
            f k = fromMaybe k $ do
                k' <- Map.lookup (CI.mk (k ^. memOriginal)) (_ruleRenamed rs)
                return (k & memName .~ k')

        retypeFields :: Untyped Shape -> Untyped Shape
        retypeFields = references %~ f replacedBy . f renamedTo
          where
            f m v = maybe v (\x -> v & refShape .~ x)
                $ Map.lookup (v ^. refShape) m

        prefixEnum :: Untyped Shape -> Untyped Shape
        prefixEnum = _SEnum . enumValues %~ f
          where
            f vs = fromMaybe vs $ do
                p <- _ruleEnumPrefix rs
                return $! first (memPrefix ?~ p) vs

        appendEnum :: Untyped Shape -> Untyped Shape
        appendEnum = _SEnum . enumValues <>~ _ruleEnumValues rs

    renamedTo :: TextMap Text
    renamedTo = buildMapping _ruleRenameTo

    replacedBy :: TextMap Text
    replacedBy = buildMapping _ruleReplacedBy

    buildMapping :: (Rules -> Maybe Text) -> TextMap Text
    buildMapping f = Map.fromList $
        mapMaybe (\(k, v) -> (k,) <$> f v) (Map.toList o)

-- | Assign unique prefixes to 'Enum' and 'Struct' shapes.
prefix :: (Functor m, MonadError String m)
       => TextMap (Shape a)
       -> m (TextMap (Shape a))
prefix ss = evalStateT (Map.traverseWithKey go ss) (mempty, mempty)
  where
    go :: (Functor m, MonadError String m)
       => Text
       -> Shape a
       -> StateT (PS, PS) m (Shape a)
    go n = \case
        SStruct x -> SStruct <$> uniq (heuristics n) _1 structMembers x
        SEnum   x -> SEnum   <$> uniq (mempty : heuristics n) _2 enumValues x
        s         -> pure s
      where
        uniq :: (Functor m, MonadError String m)
             => [CI Text]
             -> Lens' (PS, PS) PS
             -> Lens' a (OrdMap Member v)
             -> a
             -> StateT (PS, PS) m a
        uniq hs f g x = do
            p <- next n f hs (keys (x ^. g))
            pure (x & g %~ first (memPrefix ?~ p))

        keys :: OrdMap Member v -> HashSet (CI Text)
        keys = Set.fromList . map (CI.mk . _memName) . OrdMap.keys

    next :: (Functor m, MonadError String m)
         => Text
         -> Lens' (PS, PS) PS
         -> [CI Text]
         -> HashSet (CI Text)
         -> StateT (PS, PS) m Text
    next k l (x:xs) ks = do
        m <- use l
        case Map.lookup x m of
            Just js | not (Set.null (Set.intersection js ks))
                -> next k l xs ks
            _   -> l %= Map.insertWith (<>) x ks >> pure (CI.original x)
    next k l _      ks = do
        m <- use l
        throwError . intercalate "\n" $
              ("Error selecting prefix for: " <> Text.unpack k)
            : ("Fields: " <> show ks)
            : map (\h -> show h <> " => " <> show (Map.lookup h m)) (heuristics k)

    heuristics :: Text -> [CI Text]
    heuristics n = rules ++ ordinal
      where
        -- Acronym preference list.
        rules = map CI.mk $ catMaybes [r1, r2, r3, r4]

        -- SomeTestTType -> STT
        r1 = toAcronym n

        -- SomeTestTType -> S
        r3 = Text.toUpper <$> safeHead n

        -- Some -> Some || SomeTestTType -> Some
        r2 | Text.length n <= 3 = Just n
           | otherwise          = Just (Text.take 3 n)

        -- SomeTestTType -> Som
        r4 = upperHead <$> listToMaybe (splitWords n)

        -- Append an ordinal to the generated acronyms.
        ordinal = concatMap (\i -> map (\x -> mappend x (num i)) rules) [1..3]

        num :: Int -> CI Text
        num = CI.mk . Text.pack . show

-- | Determine the usage of operation input/output shapes.
--
-- A shape is considered 'shared' if it is used as a field of another shape,
-- as opposed to only being referenced by the operation itself.
--
-- Returns a set of shapes that are _not_ shared.
shared :: TextMap (Operation (Untyped Ref))
       -> TextMap (Untyped Shape)
       -> TextSet
shared oo ss = occur (execState check mempty)
  where
    -- FIXME: Need to correctly count a shape being used as a ref as shared.
    occur :: TextMap Int -> TextSet
    occur = Set.fromList . Map.keys . Map.filter (> 1)

    check :: State (TextMap Int) ()
    check = forM_ (Map.elems oo) $ \o -> do
        ref (o ^. operInput  . _Just . refShape)
        ref (o ^. operOutput . _Just . refShape)

    ref :: Text -> State (TextMap Int) ()
    ref n = count n >> maybe (pure ()) shape (Map.lookup n ss)

    shape :: Untyped Shape -> State (TextMap Int) ()
    shape = mapM_ (count . view refShape) . toListOf references

    count :: Text -> State (TextMap Int) ()
    count n  = modify (Map.insertWith (+) n 1)

constraints' :: (Functor m, MonadError String m)
             => TextMap (Typed Shape)
             -> m (TextMap (Derived Shape))
constraints' ss = execStateT (traverse go (Map.keys ss)) mempty
  where
    go :: (Functor m, MonadError String m)
       => Text
       -> StateT (TextMap (Derived Shape)) m (Derived Shape)
    go n = do
        m <- gets (Map.lookup n)
        case m of
            Just s -> return s
            Nothing ->
                case Map.lookup n ss of
                    Nothing -> throwError $ "Missing Shape " ++ Text.unpack n
                    Just  s -> do
                        d <- derive s
                        modify (Map.insert n d)
                        return d

    derive :: (Functor m, MonadError String m)
           => Typed Shape
           -> StateT (TextMap (Derived Shape)) m (Derived Shape)
    derive s = Derived s <$> case s of
        SList   _ -> pure (base <> list)
        SMap    _ -> pure (base <> list)
        SStruct _ -> complex (base <> [COrd, CIsString]) `always` [CGeneric]
        _         -> pure (constraints s)
      where
        list = [CMonoid, CSemigroup]
        base = [CEq, CRead, CShow, CGeneric]

        always f xs = f >>= pure . mappend xs

        complex = liftA2 (Set.intersection) go' . pure
          where
            go' :: (Functor m, MonadError String m)
                => StateT (TextMap (Derived Shape)) m (HashSet Constraint)
            go' = do
                    m <- traverse ref (toListOf references s)
                    return $! case m of
                        x:xs -> foldl' Set.intersection x xs
                        _    -> mempty

    ref :: (Functor m, MonadError String m)
        => Typed Ref
        -> StateT (TextMap (Derived Shape)) m (HashSet Constraint)
    ref r = _derConst <$> go (r ^. refShape)

-- | Replace the untyped 'Text' references with actual Haskell 'Type's.
types :: (Functor m, MonadError String m)
      => Protocol
      -> TextMap (Untyped Shape)
      -> m (TextMap (Typed Shape))
types proto ss = evalStateT (traverse (id references ref) ss) mempty
  where
    ref :: MonadError String m
        => Untyped Ref
        -> StateT (TextMap TType) m (Typed Ref)
    ref r = flip (set refAnn) r `liftM` memo (_refShape r)

    memo :: MonadError String m
         => Text
         -> StateT (TextMap TType) m TType
    memo n = do
        m <- gets (Map.lookup n)
        case m of
            Just t -> return t
            Nothing -> do
                t <- case Map.lookup n ss of
                    Just !x -> solve n x
                    Nothing -> return (TType n)
                modify (Map.insert n t)
                return t

    solve :: MonadError String m
          => Text
          -> Untyped Shape
          -> StateT (TextMap TType) m TType
    solve n = \case
        SStruct _ -> return (TType n)
        SEnum   _ -> return (TType n)
        SString _ -> return (TType "Text")
        SBool   _ -> return (TType "Bool")
        SDouble _ -> return (TType "Double")
        SInt    x -> return (number x "Int")
        SLong   x -> return (number x "Integer")
        STime   x -> return (time x)
        SBlob   x -> return (stream x)
        SList   x -> list x
        SMap    x -> hmap x
      where
        hmap x = do
            k <- ref (x ^. mapKey)
            v <- ref (x ^. mapValue)
            return $! flatten (TEMap e i j (k ^. refAnn) (v ^. refAnn))
          where
            e = fromMaybe "entry" Nothing -- (r ^. refLocationName)
            i = fromMaybe "key"   (x ^. mapKey   . refLocationName)
            j = fromMaybe "value" (x ^. mapValue . refLocationName)

            flatten
                | x ^. mapFlattened = TFlat
                | otherwise         = id

        list x = do
            t <- ref (x ^. listMember)
            return $! flatten (nonEmpty (t ^. refAnn))
          where
            -- flatten (TyApp (TyApp nonEmpty member) )
            flatten
                | x ^. listFlattened = TFlat
                | otherwise          = id

            nonEmpty
                | (x ^. listMin) > 0 = TList1 member
                | otherwise          = TList  member

            member = fromMaybe "member" (x ^. listMember . refLocationName)

            -- defMember
            --     | proto == Query = "member"
            --     | proto == EC2   = "member"
            --     | otherwise      = error $ "Unable to get locationName: " ++ show x

        stream = const (TType "Stream") -- figure out streaming or not

        time = TPrim
             . PTime
             . fromMaybe RFC822 -- This should take into account different protocol defaults
             . view timeTimestampFormat

        number x
            | x ^. numMin > Just 0 = const (TType "Natural")
            | otherwise            = TType

service :: (Functor m, MonadError String m)
        => Service (Untyped Shape) (Untyped Ref)
        -> m (Service (Derived Shape) (Untyped Ref))
service s = do
    -- 1. Override rules are applied to the raw AST.
    let os = rules (s ^. ovOverrides) (s ^. svcShapes)

    -- 2. Shape's members/fields are given a unique prefix.
    ps <- prefix os

    -- 3. Shapes which are specified as operation inputs/outputs are checked
    --    for sharing/commonality.
    -- let sh = shared (s ^. svcOperations) ps

    -- 4. The textual references in operations and shapes are replaced
    --    with actual Haskell types.
    ts <- types (s ^. metaProtocol) ps

    -- 5. Solve the constraints for the annotated types.
    cs <- constraints' ts

    -- 6. Substitution is done to replace the operation's input/output
    --    references with actual shapes, and any non-shared shapes
    --    are then removed from the service's shape map.
--    return $! subst sh (s & svcShapes .~ ts)

    return (s { _svcShapes = cs })
