{-# LANGUAGE TemplateHaskell #-}

module Gen.AST.Override
  ( override,
  )
where

import qualified Control.Comonad as Comonad
import qualified Control.Lens as Lens
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Gen.Prelude
import Gen.Types

data Env = Env
  { _renamed :: Map Id Id,
    _replaced :: Map Id Replace,
    _memo :: Map Id (Shape Related)
  }

$(Lens.makeLenses ''Env)

-- | Apply the override rules to shapes and their respective fields.
override ::
  Functor f =>
  Map Id Override ->
  Service f (RefF a) (Shape Related) b ->
  Service f (RefF a) (Shape Related) b
override ovs svc =
  svc
    & operations . Lens.each %~ operation
    & shapes .~ State.evalState ss (Env rename replace mempty)
  where
    ss =
      fmap Map.fromList
        . traverse (uncurry (overrideShape ovs))
        . Map.toList
        $ svc ^. shapes

    operation :: Functor f => Operation f (RefF a) b -> Operation f (RefF a) b
    operation o =
      o
        { _opInput = ref <$> _opInput o,
          _opOutput = ref <$> _opOutput o
        }

    ref :: RefF a -> RefF a
    ref r
      | Just x <- Map.lookup ptr rename = r & refShape .~ x
      | Just x <- Map.lookup ptr replace = r & refShape .~ x ^. replaceName
      | otherwise = r
      where
        ptr = r ^. refShape

    rename :: Map Id Id
    rename = vMapMaybe _renamedTo ovs

    replace :: Map Id Replace
    replace = vMapMaybe _replacedBy ovs

type MemoS = State Env

overrideShape ::
  Map Id Override ->
  Id ->
  Shape Related ->
  MemoS (Id, Shape Related)
overrideShape ovs n c@(_ :< s) = go -- env memo n >>= maybe go (return . (n,))
  where
    go = do
      rp <- env replaced n
      rn <- env renamed n

      case (rp, rn) of
        (Nothing, Nothing) -> (n,) <$> shape
        (Just x, _) -> (n,) <$> pointer x
        (_, Just x)
          | x == n -> (n,) <$> shape
          | otherwise -> overrideShape ovs x c

    Override {..} = fromMaybe defaultOverride (Map.lookup n ovs)

    pointer :: Replace -> MemoS (Shape Related)
    pointer r =
      save $
        (Comonad.extract c & annId .~ n) :< Ptr (s ^. info) (typeOf r)

    shape :: MemoS (Shape Related)
    shape = do
      let a = Comonad.extract c & annId .~ n

      Lens.traverseOf references ref s
        >>= rules
        >>= save . (a :<)

    ref :: RefF (Shape Related) -> MemoS (RefF (Shape Related))
    ref r =
      flip (Lens.set refAnn) r . snd
        <$> overrideShape ovs (r ^. refShape) (r ^. refAnn)

    rules :: ShapeF a -> MemoS (ShapeF a)
    rules = retype . fields . require . optional

    require, optional :: ShapeF a -> ShapeF a
    require = setRequired (<> _requiredFields)
    optional = setRequired (List.\\ _optionalFields)

    fields :: ShapeF a -> ShapeF a
    fields = _Struct . members . kvTraversal %~ first f
      where
        f k = maybe k (replaceId k) (Map.lookup k _renamedFields)

    retype :: ShapeF a -> MemoS (ShapeF a)
    retype x = do
      rp <- Lens.use replaced
      rn <- Lens.use renamed

      let f g m v =
            maybe
              v
              (flip (Lens.set refShape) v . g)
              (Map.lookup (v ^. refShape) m)

      pure $! x
        & references
        %~ f _replaceName rp . f id rn

env :: MonadState Env m => Getter Env (Map Id a) -> Id -> m (Maybe a)
env l n = Lens.uses l (Map.lookup n)

save :: Shape Related -> MemoS (Shape Related)
save x = x <$ (memo %= Map.insert (identifier x) x)
