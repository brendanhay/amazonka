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
    , fieldLens
    , fieldAccessor
    , fieldParam

    -- * Nested Lenses
    , fieldRef
    , fieldRequired
    , fieldHelp
    , fieldLocation
    , fieldMonoid
    ) where

import           Compiler.AST.TypeOf
import           Compiler.Types
import           Control.Comonad
import           Control.Lens
import qualified Data.HashMap.Strict          as Map
import qualified Data.HashSet                 as Set
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
    } deriving (Show)

makeLenses ''Field

instance TypeOf Field where
    typeOf f = canDefault (f ^. fieldRequired) (typeOf (f ^. fieldRef))
      where
        canDefault :: Bool -> TType -> TType
        canDefault True  t   = t -- This field is required.
        canDefault False t
              -- This field is not required, but the TType can't be defaulted sensibly.
            | typeRequired t = TMaybe t
              -- This field is not required, and can be defaulted using mempty/Nothing.
            | otherwise      = t

mkFields :: StructF (Shape (Id ::: (Maybe Text ::: (Relation ::: Solved))))
         -> [Field]
mkFields st = map mk . Map.toList $ st ^. members
  where
    mk :: (Id, Ref) -> Field
    mk (k, v) = Field k v (Set.member k (st ^. required))

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
fieldMonoid = fieldRef . refAnn . to (f . extract)
  where
    f (_ ::: _ ::: _ ::: _ ::: ds ::: _) = DMonoid `elem` ds

fieldPrefix :: Getter Field (Maybe Text)
fieldPrefix = fieldRef . refAnn . to (f . extract)
  where
    f (_ ::: p ::: _ ::: _ ::: _ ::: _) = p
