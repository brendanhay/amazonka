module Gen.AST.Solve
  ( solve,
  )
where

import qualified Control.Comonad as Comonad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import Gen.Prelude
import Gen.Types

-- | Annotate every shape with its Haskell type and the classes it can derive.
--
-- A shape's type is a function of its 'Id' alone, so every occurrence of a
-- shape gets the same annotation, wherever 'Ptr' loop-breakers sit in the
-- trees and in whatever order the trees are visited.
--
-- A struct can derive a class when all of its members can. For recursive
-- structs that rule is solved as a greatest fixed point: every struct starts
-- with 'derivingBase' and loses classes until no struct changes, so structs
-- on a cycle keep exactly the classes that nothing reachable from them rules
-- out.
--
-- The first argument holds the types that overrides substitute for shapes,
-- keyed by the substituted name. A shape with one of those names takes that
-- type in place of a solved one.
solve ::
  (Traversable t) =>
  HashMap Id TType ->
  t (Shape Prefixed) ->
  Either String (t (Shape Solved))
solve seed ss = do
  ds <- fixpoint start
  traverse (sequenceA . Comonad.extend (annotate ds)) ss
  where
    annotate ds x@(a :< _) = Solved a <$> typeAt ds mempty x

    -- One occurrence of every shape, preferring a root. Loop-breakers are
    -- left out, so looking up a loop-breaker's 'Id' finds its target.
    nodes :: HashMap Id (Shape Prefixed)
    nodes = HashMap.union (index roots) (index (concatMap subtrees roots))
      where
        roots = foldr (:) [] ss
        subtrees = foldr (:) [] . Comonad.duplicate
        index xs = HashMap.fromList [(identifier x, x) | x@(_ :< s) <- xs, not (isPtr s)]

        isPtr = \case
          Ptr {} -> True
          _ -> False

    structs :: HashMap Id (StructF (Shape Prefixed))
    structs = HashMap.mapMaybe struct nodes `HashMap.difference` seed
      where
        struct = \case
          _ :< Struct st -> Just st
          _ -> Nothing

    start :: HashMap Id [Derive]
    start = structs <&> \st -> if isStreaming st then streaming else derivingBase

    step :: HashMap Id [Derive] -> Either String (HashMap Id [Derive])
    step ds =
      for structs $ \st ->
        if isStreaming st
          then pure streaming
          else
            foldr (List.intersect . derivingOf) derivingBase
              <$> traverse (refType ds mempty) (st ^.. references)

    -- Each round can only remove classes, so this terminates.
    fixpoint :: HashMap Id [Derive] -> Either String (HashMap Id [Derive])
    fixpoint ds = do
      ds' <- step ds
      if ds' == ds then pure ds else fixpoint ds'

    streaming = derivingOf (TStream :: TType)

    refType :: HashMap Id [Derive] -> Set Id -> RefF (Shape Prefixed) -> Either String TType
    refType ds followed r
      | isStreaming r = pure TStream
      | otherwise = typeAt ds followed (r ^. refAnn)

    -- The second argument is the set of loop-breakers followed since the last
    -- struct. Meeting one of them again means the cycle has no struct on it,
    -- so the type would be infinite.
    typeAt :: HashMap Id [Derive] -> Set Id -> Shape Prefixed -> Either String TType
    typeAt ds followed (a :< s) =
      case s of
        -- The target's own 'Info' decides whether the type is sensitive. The
        -- loop-breaker carries a copy of it, which must not wrap the type again.
        Ptr {}
          | Set.member n followed -> Left (failure "Recursive shape with no struct on its cycle")
          | otherwise ->
              note (failure "Loop-breaker for a missing shape") (HashMap.lookup n nodes)
                >>= typeAt ds (Set.insert n followed)
        -- A replacement carries its own type, which can differ from that of
        -- another replacement with the same name.
        Replaced _ t -> pure (wrap t)
        _ | Just t <- HashMap.lookup n seed -> pure (wrap t)
        Struct {} ->
          wrap . TType (typeId n)
            <$> note (failure "No derivings solved for struct") (HashMap.lookup n ds)
        Enum {} -> pure (wrap (TType (typeId n) (DOrd : derivingBase)))
        List (ListF i e)
          | nonEmpty i -> wrap . TList1 <$> ref e
          | otherwise -> wrap . TList <$> ref e
        Map (MapF _ k v) -> wrap <$> (TMap . key <$> ref k <*> ref v)
        Lit i l -> pure (wrap (literal i l))
      where
        n = identifier a
        ref = refType ds followed

        wrap t
          | s ^. infoSensitive = TSensitive t
          | otherwise = t

        key = \case
          TSensitive t -> t
          t -> t

        failure msg = msg ++ ": " ++ Text.unpack (memberId n)

literal :: Info -> Lit -> TType
literal i = \case
  Int -> natural (TLit Int)
  Long -> natural (TLit Long)
  Base64 | isStreaming i -> TStream
  Bytes | isStreaming i -> TStream
  l -> TLit l
  where
    natural t
      | Just m <- i ^. infoMin, m >= 0 = TNatural
      | otherwise = t
