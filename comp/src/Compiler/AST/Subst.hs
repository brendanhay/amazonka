{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.AST.Subst
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Subst
    ( substitute
    ) where

import           Compiler.AST.Cofree
import           Compiler.AST.Data
import           Compiler.AST.Override
import           Compiler.AST.Prefix
import           Compiler.AST.Solve
import           Compiler.Formatting
import           Compiler.Protocol
import           Compiler.Types
import           Control.Comonad.Cofree
import           Control.Error
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.HashMap.Strict    as Map
import qualified Data.HashSet           as Set
import           Data.List              (sort)
import           Data.Monoid
import qualified Data.Text.Lazy         as LText
import           Debug.Trace

data Env a = Env
    { _overrides :: Map Id Override
    , _memo      :: Map Id (Shape a)
    }

makeLenses ''Env

type MemoS a = StateT (Env a) (Either Error)

-- | Set some appropriate defaults where needed for later stages,
-- and ensure there are no vacant references to input/output shapes
-- by either adding any empty request or response shapes,
-- or creating a wrapper type for the request/response pointing to a
-- potentially shared shape.
substitute :: Service Maybe (RefF ()) (Shape Related)
           -> Either Error (Service Identity (RefF ()) (Shape Related))
substitute svc@Service{..} = do
    (os, e) <- runStateT (traverse operation _operations) $
        Env mempty (Map.map shape _shapes)
    return $! override (e ^. overrides) $ svc
        { _metadata'  = meta _metadata'
        , _operations = os
        , _shapes     = e ^. memo
        }
  where
    meta :: Metadata Maybe -> Metadata Identity
    meta m@Metadata{..} = m
        { _timestampFormat = Identity ts
        , _checksumFormat  = _checksumFormat .! SHA256
        }

    ts :: Timestamp
    ts = fromMaybe (timestamp (svc ^. protocol)) (svc ^. timestampFormat)

    operation :: Operation Maybe (RefF ())
              -> MemoS Related (Operation Identity (RefF ()))
    operation o@Operation{..} = do
        let h = http _opHTTP

        inp <- subst Input  (name Input  _opName) h _opInput
        out <- subst Output (name Output _opName) h _opOutput
        return $! o
            { _opDocumentation =
                _opDocumentation .! "FIXME: Undocumented operation."
            , _opHTTP          = h
            , _opInput         = inp
            , _opOutput        = out
            }

    http :: HTTP Maybe -> HTTP Identity
    http h = h
        { _responseCode = _responseCode h .! 200
        }

    shape :: Shape a -> Shape a
    shape (n :< s) = (n :<) $
        case s of
            Lit i (Time Nothing) -> Lit i . Time $ Just ts
            _                    -> s

    -- Fill out missing Refs with a default Ref pointing to an empty Shape,
    -- which is also inserted into the resulting Shape universe.
    --
    -- For shared Shapes, perform a copy of the destination Shape to a new Shape.
    subst :: Direction
          -> Id
          -> HTTP Identity
          -> Maybe (RefF ())
          -> MemoS Related (Identity (RefF ()))

    -- FIXME: this could be a shared empty shape for void types which succeeds
    -- on de/serialisation for any protocol, and takes into account a successful
    -- status code on responses.
    subst d n h Nothing  = do
        verify n "Failure attempting to substitute fresh shape"
        -- No Ref exists, safely insert an empty shape and return a related Ref.
        save n (mkRelOp d n h (mkRelation mempty d) :< emptyStruct)
        return $! Identity (emptyRef n)

    subst d n h (Just r) = do
        let k = r ^. refShape
        x :< s <- lift (safe k _shapes)
        if not (isShared x)
            -- Ref exists, and is not referred to by any other Shape.
            -- Insert override to rename the Ref/Shape to the desired name.
            then do
                -- Ensure the annotation is updated.
                save k (mkRelOp d k h x :< s)
                rename k n >> return (Identity r)
            -- Ref exists and is referred to by other shapes.
            else do
                -- Check that the desired name is not in use
                -- to prevent accidental override.
                verify n "Failed attempting to copy existing shape"
                -- Copy the shape by saving it under the desired name.
                save n (mkRelOp d n h x :< s)
                memo %= Map.delete k
                -- Update the Ref to point to the new wrapper.
                return $! Identity (r & refShape .~ n)

save :: Id -> Shape a -> MemoS a ()
save n s = memo %= Map.insert n s

rename :: Id -> Id -> MemoS a ()
rename x y = overrides %= Map.insert x (defaultOverride & renamedTo ?~ y)

safe :: Show a => Id -> Map Id a -> Either Error a
safe n ss = note
    (format ("Missing shape "      % iprimary %
            ", possible matches: " % partial)
            n (n, ss))
    (Map.lookup n ss)

verify :: (MonadState (Env a) m, MonadError e m)
       => Id
       -> Format (Id -> LText.Text) (Id -> e)
       -> m ()
verify n msg = do
    p <- uses memo (Map.member n)
    when p . throwError $
        format (msg % " for " % iprimary) n

name :: Direction -> Id -> Id
name Input  n = mkId (n ^. typeId)
name Output n = mkId (appendId n "Response" ^. typeId)

infixl 7 .!

(.!) :: Maybe a -> a -> Identity a
m .! x = maybe (Identity x) Identity m

emptyStruct :: ShapeF a
emptyStruct = Struct (StructF i mempty mempty Nothing)
  where
    i = Info
        { _infoDocumentation = Nothing
        , _infoMin           = Nothing
        , _infoMax           = Nothing
        , _infoFlattened     = False
        , _infoSensitive     = False
        , _infoStreaming     = False
        , _infoException     = False
        }

emptyRef :: Id -> RefF ()
emptyRef n = RefF
    { _refAnn           = ()
    , _refShape         = n
    , _refDocumentation = Nothing
    , _refLocation      = Nothing
    , _refLocationName  = Nothing
    , _refResultWrapper = Nothing
    , _refQueryName     = Nothing
    , _refStreaming     = False
    , _refXMLAttribute  = False
    , _refXMLNamespace  = Nothing
    }
