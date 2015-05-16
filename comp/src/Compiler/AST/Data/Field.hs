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
import           Control.Comonad
import           Control.Lens
import           Data.Maybe
import qualified Data.Text                    as Text
import           Language.Haskell.Exts.Syntax (Name (..))

-- | Convenience type to package up some information from the struct with the
-- related field, namely the memberId and the (Set.member required).
data Field = Field
    { _fieldId       :: Id    -- ^ The memberId from the struct members map.
    , _fieldRef      :: Ref   -- ^ The original struct member reference.
    , _fieldRequired :: !Bool -- ^ Does the struct have this member in the required set.
    } deriving (Show)

   -- let _ ::: _ ::: _ ::: rt ::: rds ::: _ = extract (v ^. refAnn)
   --  in Field
   --     { fieldId      = k
   --     , fieldType    = memberType k (strf ^. required) rt

   --     , fieldDerive  = rds
   --     , fieldHelp    =
   --         fromMaybe "FIXME: Undocumented member."
   --             (v ^. refDocumentation)
   --     }

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

-- | Parameter to a constructor function.
fieldParam :: Getter Field Name
fieldParam = fieldId . paramId . to (Ident . Text.unpack)

fieldHelp :: Getter Field Help
fieldHelp = fieldRef
    . refDocumentation
    . to (fromMaybe "FIXME: Undocumented member.")

fieldMonoid :: Getter Field Bool
fieldMonoid = fieldRef . refAnn . to (f . extract)
  where
    f (_ ::: _ ::: _ ::: _ ::: ds ::: _) = DMonoid `elem` ds

--    , fieldLocation :: !Location    Also the name? Maybe just the ref here or what?

fieldLocation :: Getter Field (Maybe Location)
fieldLocation = fieldRef . refLocation

-- fieldLocationName :: Getter Field Text
-- fieldLocationName = undefined

-- This can perhaps call out to the Protocol module and provide
-- correction 'locationName' lenses for the field.
