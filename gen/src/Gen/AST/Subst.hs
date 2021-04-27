{-# LANGUAGE TemplateHaskell #-}

-- Module      : Gen.AST.Subst
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.AST.Subst
  ( substitute,
  )
where

import Control.Comonad.Cofree
import Control.Error
import Control.Lens hiding ((:<))
import Control.Monad.Except
import Control.Monad.State
import Data.HashMap.Strict qualified as Map
import Data.List (find)
import Data.Text.Lazy qualified as LText
import Gen.AST.Override
import Gen.Formatting
import Gen.Types

data Env a = Env
  { _overrides :: Map Id Override,
    _memo :: Map Id (Shape a)
  }

makeLenses ''Env

type MemoS a = StateT (Env a) (Either Error)

-- | Set some appropriate defaults where needed for later stages,
-- and ensure there are no vacant references to input/output shapes
-- by either adding any empty request or response shapes,
-- or creating a wrapper type for the request/response pointing to a
-- potentially shared shape.
substitute ::
  Service Maybe (RefF ()) (Shape Related) a ->
  Either Error (Service Identity (RefF ()) (Shape Related) a)
substitute svc@Service {..} = do
  (os, e) <- runStateT (traverse operation _operations) (Env mempty _shapes)
  return $! override (e ^. overrides) $
    svc
      { _metadata' = meta _metadata',
        _operations = os,
        _shapes = e ^. memo
      }
  where
    meta :: Metadata Maybe -> Metadata Identity
    meta m@Metadata {..} =
      m
        { _timestampFormat = Identity ts,
          _checksumFormat = _checksumFormat .! SHA256
        }

    ts :: Timestamp
    ts = fromMaybe (timestamp (svc ^. protocol)) (svc ^. timestampFormat)

    operation ::
      Operation Maybe (RefF ()) a ->
      MemoS Related (Operation Identity (RefF ()) a)
    operation o@Operation {..} = do
      inp <- subst Input (name Input _opName) _opInput
      out <- subst Output (name Output _opName) _opOutput
      return
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
      | Map.member n _shapes = mkId (typeId (appendId n "'"))
      | otherwise = n
    name Output n
      | Map.member rs _operations = mkId (typeId (appendId n "Response'"))
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
      -- No Ref exists, safely insert an empty shape and return a related Ref.
      save n (Related n (mkRelation Nothing d) :< emptyStruct)
      return (emptyRef n)
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
            return (r & refShape .~ n)
          | isShared x -> return r
          | otherwise -> do
            -- Ref exists, and is not referred to by any other Shape.
            -- Insert override to rename the Ref/Shape to the desired name.
            -- Ensure the annotation is updated.
            --
            -- Also adds a required status code field to any
            -- non-shared response.
            save k (Related k (_annRelation x) :< addStatus d k s)
            rename k n
            return r

addStatus :: Direction -> Id -> ShapeF (Shape Related) -> ShapeF (Shape Related)
addStatus Input _k = id
addStatus Output _k = go
  where
    go = \case
      Struct st -> Struct (maybe missing exists mstatus)
        where
          mstatus =
            find ((Just StatusCode ==) . view refLocation . snd) $
              Map.toList (st ^. members)

          missing =
            st & required' %~ cons n & members %~ Map.insert n ref

          exists (name, _) =
            st & required' %~ cons name

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
save n s = memo %= Map.insert n s

rename :: Id -> Id -> MemoS a ()
rename x y = overrides %= Map.insert x (defaultOverride & renamedTo ?~ y)

safe :: Show a => Id -> Map Id a -> Either Error a
safe n ss =
  note
    ( format
        ( "Missing shape " % iprimary
            % ", possible matches: "
            % partial
        )
        n
        (n, ss)
    )
    (Map.lookup n ss)

verify ::
  (MonadState (Env a) m, MonadError e m) =>
  Id ->
  Format (Id -> LText.Text) (Id -> e) ->
  m ()
verify n msg = do
  p <- uses memo (Map.member n)
  when p . throwError $
    format (msg % " for " % iprimary) n

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
