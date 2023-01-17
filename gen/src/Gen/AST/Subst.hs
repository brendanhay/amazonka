{-# LANGUAGE TemplateHaskell #-}

module Gen.AST.Subst
  ( substitute,
  )
where

import qualified Control.Lens as Lens
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text
import Gen.AST.Override
import Gen.Prelude
import Gen.Types

data Env a = Env
  { _overrides :: HashMap Id Override,
    _memo :: HashMap Id (Shape a)
  }

$(Lens.makeLenses ''Env)

type MemoS a = StateT (Env a) (Either String)

-- | Set some appropriate defaults where needed for later stages,
-- and ensure there are no vacant references to input/output shapes
-- by either adding any empty request or response shapes,
-- or creating a wrapper type for the request/response pointing to a
-- potentially shared shape.
substitute ::
  Service Maybe (RefF ()) (Shape Related) a ->
  Either String (Service Identity (RefF ()) (Shape Related) a)
substitute svc@Service {..} = do
  (os, e) <- State.runStateT (traverse operation _operations) (Env mempty _shapes)
  pure $! override (e ^. overrides) $
    svc
      { _metadata' = meta _metadata',
        _operations = os,
        _shapes = e ^. memo
      }
  where
    meta :: Metadata Maybe -> Metadata Identity
    meta m@Metadata {..} =
      m { _checksumFormat = _checksumFormat .! SHA256 }

    operation ::
      Operation Maybe (RefF ()) a ->
      MemoS Related (Operation Identity (RefF ()) a)
    operation o@Operation {..} = do
      inp <- subst Input (name Input _opName) _opInput
      out <- subst Output (name Output _opName) _opOutput
      pure
        $! o
          { _opDocumentation = _opDocumentation .! "-- | Undocumented operation.",
            _opInput = Identity inp,
            _opOutput = Identity out
          }

    -- This ensures the Response type has a unique name
    -- even in the presence of sharing. A check is performed to ensure that
    -- a response type isn't inserted before we get to another top-level
    -- operation with the same name.
    name :: Direction -> Id -> Id
    name Input n
      | HashMap.member n _shapes = mkId (typeId (appendId n "'"))
      | otherwise = n
    name Output n
      | HashMap.member rs _operations = mkId (typeId (appendId n "Response'"))
      | otherwise = rs
      where
        rs = mkId (typeId (appendId n "Response"))

    -- Fill out missing Refs with a default Ref pointing to an empty Shape,
    -- which is also inserted into the resulting Shape universe.
    --
    -- For shared Shapes, perform a copy of the destination Shape to a new Shape.
    subst ::
      Direction ->
      Id ->
      Maybe (RefF ()) ->
      MemoS Related (RefF ())

    -- FIXME: this could be a shared empty shape for void types which succeeds
    -- on de/serialisation for any protocol, and takes into account a successful
    -- status code on responses.
    subst d n Nothing = do
      verify n "Failure attempting to substitute fresh shape"
      -- No Ref exists, safely insert an empty shape and pure a related Ref.
      save n (Related n (mkRelation Nothing d) :< emptyStruct)
      pure (emptyRef n)
    subst d n (Just r) = do
      let k = r ^. refShape
      x :< s <- lift (safe k _shapes)
      if
          | isShared x,
            d == Input -> do
            -- Check that the desired name is not in use
            -- to prevent accidental override.
            verify n "Failed attempting to copy existing shape"
            -- Copy the shape by saving it under the desired name.
            save n ((x & annId .~ n) :< s)
            -- Update the Ref to point to the new wrapper.
            pure (r & refShape .~ n)
          | isShared x -> pure r
          | otherwise -> do
            -- Ref exists, and is not referred to by any other Shape.
            -- Insert override to rename the Ref/Shape to the desired name.
            -- Ensure the annotation is updated.
            --
            -- Also adds a required status code field to any
            -- non-shared response.
            save k (Related k (_annRelation x) :< addStatus d k s)
            rename k n
            pure r

addStatus :: Direction -> Id -> ShapeF (Shape Related) -> ShapeF (Shape Related)
addStatus Input _k = id
addStatus Output _k = go
  where
    go = \case
      Struct st -> Struct (maybe missing exists mstatus)
        where
          mstatus =
            List.find ((Just StatusCode ==) . Lens.view refLocation . snd) $
              HashMap.toList (st ^. members)

          missing =
            st & required' %~ Lens.cons n & members %~ HashMap.insert n ref

          exists (name, _) =
            st & required' %~ Lens.cons name

          ref =
            emptyRef n
              & refLocation ?~ StatusCode
              & refDocumentation ?~ "The response\'s http status code."
              & refAnn .~ Related n mempty :< Lit emptyInfo Int

          n = mkId "HttpStatus"
      --
      other ->
        other

save :: Id -> Shape a -> MemoS a ()
save n s = memo %= HashMap.insert n s

rename :: Id -> Id -> MemoS a ()
rename x y = overrides %= HashMap.insert x (defaultOverride & renamedTo ?~ y)

safe :: Show a => Id -> HashMap Id a -> Either String a
safe n ss =
  note
    ( "Missing shape " ++ Text.unpack (memberId n)
        ++ ", possible matches: "
        ++ partial n ss
    )
    (HashMap.lookup n ss)

verify ::
  (MonadState (Env a) m, MonadError String m) =>
  Id ->
  String ->
  m ()
verify n msg = do
  p <- Lens.uses memo (HashMap.member n)

  when p . Except.throwError $
    msg ++ " for " ++ Text.unpack (memberId n)

infixl 7 .!

(.!) :: Maybe a -> a -> Identity a
m .! x = maybe (Identity x) Identity m

emptyStruct :: ShapeF a
emptyStruct = Struct (StructF emptyInfo mempty mempty Nothing)

emptyInfo :: Info
emptyInfo =
  Info
    { _infoDocumentation = Nothing,
      _infoMin = Nothing,
      _infoMax = Nothing,
      _infoFlattened = False,
      _infoSensitive = False,
      _infoStreaming = False,
      _infoException = False,
      _infoError = Nothing
    }

emptyRef :: Id -> RefF ()
emptyRef n =
  RefF
    { _refAnn = (),
      _refShape = n,
      _refDocumentation = Nothing,
      _refLocation = Nothing,
      _refLocationName = Nothing,
      _refResultWrapper = Nothing,
      _refQueryName = Nothing,
      _refStreaming = False,
      _refXMLAttribute = False,
      _refXMLNamespace = Nothing
    }
