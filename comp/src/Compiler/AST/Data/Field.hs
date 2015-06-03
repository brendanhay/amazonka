{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

-- Module      : Compiler.AST.Data.Field
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Data.Field where

import           Compiler.AST.TypeOf
import           Compiler.Types
import           Control.Applicative
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Lens
import           Data.Function                (on)
import qualified Data.HashSet                 as Set
import           Data.List                    (sortBy)
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Manipulate
import           Language.Haskell.Exts.Syntax (Name (..))

-- | Convenience type to package up some information from the struct with the
-- related field, namely the memberId and the (Set.member required).
data Field = Field
    { _fieldOrdinal   :: !Int
    , _fieldId        :: Id    -- ^ The memberId from the struct members map.
    , _fieldRef       :: Ref   -- ^ The original struct member reference.
    , _fieldRequired  :: !Bool -- ^ Does the struct have this member in the required set.
    , _fieldPayload   :: !Bool -- ^ Does the struct have this memeber marked as the payload.
    , _fieldPrefix    :: Maybe Text
    , _fieldNamespace :: Maybe Text
    }

makeLenses ''Field

instance IsStreaming Field where
    streaming = streaming . _fieldRef

instance TypeOf Field where
    typeOf f = canDefault (f ^. fieldRequired) (typeOf (f ^. fieldRef))
      where
        canDefault :: Bool -> TType -> TType
        canDefault True  t  = t -- This field is required.
        canDefault False t
              -- This field is not required, and can be defaulted using mempty/Nothing.
            | typeDefault t = t
              -- This field is not required, but the TType can't be defaulted sensibly.
            | otherwise     = TMaybe t

instance HasInfo Field where
    info = fieldAnn . info

-- FIXME: Can just add the metadata to field as well since
-- the protocol/timestamp are passed in everywhere in the .Syntax module.
mkFields :: HasMetadata a f
         => a
         -> Maybe Text
         -> StructF (Shape Solved)
         -> [Field]
mkFields m p st = sortFields $ zipWith mk [1..] (st ^. members)
  where
    mk :: Int -> (Id, Ref) -> Field
    mk i (k, v) = Field i k v req pay p ns
      where
        req = Set.member k (getRequired st)
        pay = Just k == st ^. payload

        ns  = (view xmlUri <$> v ^. refXMLNamespace)
          <|> (m ^. xmlNamespace)

-- | Ensures that streaming fields appear last in the parameter ordering,
-- but doesn't affect the rest of the order which is determined by parsing
-- of the JSON service definition.
sortFields :: [Field] -> [Field]
sortFields = id -- zipWith (set fieldOrdinal) [1..] . sortBy (on compare streaming)

fieldLens, fieldAccessor :: Getter Field Text
fieldLens     = to (\f -> f ^. fieldId . lensId     (f ^. fieldPrefix))
fieldAccessor = to (\f -> f ^. fieldId . accessorId (f ^. fieldPrefix))

-- | Parameter to a constructor function.
fieldParam :: Getter Field Name
fieldParam = fieldId
    . lensId Nothing
    . to (Ident . Text.unpack . Text.cons 'p' . upperHead)

fieldHelp :: Getter Field Help
fieldHelp = fieldRef
    . refDocumentation
    . to (fromMaybe "FIXME: Undocumented member.")

fieldLocation :: Getter Field (Maybe Location)
fieldLocation = fieldRef . refLocation

fieldMonoid :: Getter Field Bool
fieldMonoid = fieldAnn . to (elem DMonoid . view annDerive)

fieldStream :: Getter Field Bool
fieldStream = to f
  where
    f x = x ^. fieldRef . refStreaming
       || x ^. fieldAnn . infoStreaming

fieldList1, fieldList, fieldMap :: Field -> Bool
fieldList1 f = fieldList f && nonEmpty f
fieldList    = not . isn't _List . unwrap . view fieldAnn
fieldMap     = not . isn't _Map  . unwrap . view fieldAnn

fieldAnn :: Lens' Field (Shape Solved)
fieldAnn = fieldRef . refAnn
