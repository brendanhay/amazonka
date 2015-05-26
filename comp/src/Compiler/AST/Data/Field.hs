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

module Compiler.AST.Data.Field
    ( Field
    , mkFields

    -- * Ids
    , fieldId
    , fieldLens
    , fieldAccessor
    , fieldParam

    -- * Nested Lenses
    , fieldRef
    , fieldRequired
    , fieldHelp
    , fieldLocation
    , fieldMonoid
    , fieldPayload
    ) where

import           Compiler.AST.TypeOf
import           Compiler.Types
import           Control.Lens
import           Data.Function                (on)
import qualified Data.HashSet                 as Set
import           Data.List                    (sort)
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.Haskell.Exts.Syntax (Name (..))

-- | Convenience type to package up some information from the struct with the
-- related field, namely the memberId and the (Set.member required).
data Field = Field
    { _fieldId       :: Id    -- ^ The memberId from the struct members map.
    , _fieldRef      :: Ref   -- ^ The original struct member reference.
    , _fieldRequired :: !Bool -- ^ Does the struct have this member in the required set.
    , _fieldPayload  :: !Bool -- ^ Does the struct have this memeber marked as the payload.
    , _fieldPrefix   :: Maybe Text
    } deriving (Show)

makeLenses ''Field

instance Eq Field where
    (==) = on (==) _fieldId

-- | Ensures that streaming fields appear last in the parameter ordering,
-- but doesn't affect the rest of the order which is determined by parsing
-- of the JSON service definition.
instance Ord Field where
    compare = on compare streaming

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

mkFields :: Maybe Text -> StructF (Shape Solved) -> [Field]
mkFields p st = sort $ map mk (st ^. members)
  where
    mk :: (Id, Ref) -> Field
    mk (k, v) = Field k v (Set.member k (getRequired st)) (Just k == st ^. payload) p

fieldLens, fieldAccessor :: Getter Field Text
fieldLens     = to (\f -> f ^. fieldId . lensId     (f ^. fieldPrefix))
fieldAccessor = to (\f -> f ^. fieldId . accessorId (f ^. fieldPrefix))

-- | Parameter to a constructor function.
fieldParam :: Getter Field Name
fieldParam = fieldId . paramId . to (Ident . Text.unpack)

fieldHelp :: Getter Field Help
fieldHelp = fieldRef
    . refDocumentation
    . to (fromMaybe "FIXME: Undocumented member.")

fieldLocation :: Getter Field (Maybe Location)
fieldLocation = fieldRef . refLocation

fieldMonoid :: Getter Field Bool
fieldMonoid = fieldRef . refAnn . to (elem DMonoid . view annDerive)
