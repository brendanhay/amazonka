{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- Module      : Compiler.Protocol
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Protocol where

import           Compiler.Types
import           Control.Lens
import           Data.List            (nub)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import           Data.Text.Manipulate

-- FIXME: Go through the other SDK's tests to ensure correctness.
memberName :: Protocol
           -> Direction
           -> Id     -- ^ The member id.
           -> RefF a -- ^ The member reference.
           -> Text
memberName p d n r = go p d
  where
    go EC2 Input = upperHead $ fromMaybe key (r ^. refQueryName)
    go _   _     = key

    -- Use the locationName on the struct member if present,
    -- otherwise the struct member id.
    key = fromMaybe (n ^. memberId) (r ^. refLocationName)

listName :: Protocol
         -> Direction
         -> Id        -- ^ The member id.
         -> RefF  a   -- ^ The member reference.
         -> ListF a   -- ^ The list shape pointed to by the member reference.
         -> (Text, Maybe Text)
listName p d n r l = (memberName p d n r, go p d (l ^. infoFlattened))
  where
    go :: Protocol
       -> Direction
       -> Bool -- ^ Flattened?
       -> Maybe Text

    go Query    _      True  = Nothing
    go Query    _      False = Just item

    go EC2      _      True  = Nothing
    go EC2      _      False = Just item

    go JSON     _      _     = Nothing
    go RestJSON _      _     = Nothing

    go RestXML  _      True  = Nothing
    go RestXML  _      False = Just item

    -- input XML       True  = (parent, Nothing) -
    -- input XML       False = (parent, Just element)

    -- Use the locationName on the actual list element pointed
    -- to by the struct member reference if present,
    -- otherwise default to 'member'.
    item = fromMaybe "member" (l ^. listItem . refLocationName)
