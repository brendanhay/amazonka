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
import           Control.Applicative
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Lens
import           Data.List              (nub)
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Manipulate

protocolSuffix :: Protocol -> Text
protocolSuffix = \case
    JSON     -> "JSON"
    RestJSON -> "JSON"
    RestXML  -> "XML"
    Query    -> "XML"
    EC2      -> "XML"

mapNames :: Protocol
         -> Direction
         -> Id
         -> RefF a
         -> MapF a
         -> (Maybe Text, Text, Text, Text)
mapNames p d n r m
    | m ^. infoFlattened = (Nothing,     member, key, value)
    | otherwise          = (Just member, entry,  key, value)
  where
    member = memberName p d n r

    entry  = "entry"

    key    = fromMaybe "key"   (m ^. mapKey   . refLocationName)
    value  = fromMaybe "value" (m ^. mapValue . refLocationName)

    -- Member, [entry, key, value]

    -- Non-flattened, no locationNames:
    -- <Map><entry><key>qux</key><value><foo>bar</foo></value></entry></Map>

    -- Non-flattened, locationNames:
    -- <Map><entry><foo>qux</foo><bar>bar</bar></entry></Map>

    -- Nothing, [Member, key, value]

    -- Flattened, no locationNames:
    -- <Map><key>qux</key><value>bar</value></Map><Map><key>baz</key><value>bam</value></Map>

    -- Flattened, locationNames:
    -- <Map><foo>qux</foo><bar>qux</var></Map>

    -- Query, input:
    -- MapArg.entry.1.key=key1&MapArg.entry.1.value=val1

listNames :: Protocol
          -> Direction
          -> Id
          -> RefF  a
          -> ListF a
          -> (Maybe Text, Text)
listNames p d n r l
     | l ^. infoFlattened || p == EC2 = flat
     | otherwise                      = nest
  where
    flat =
        ( Nothing
        , fromMaybe mem (ref <|> item)
        )

    nest =
        ( Just $ fromMaybe mem ref
        , fromMaybe "member" item
        )

    mem  = n ^. memberId
    ref  = r ^. refLocationName
    item = l ^. listItem . refLocationName

     -- Member, [item]

     -- Non-flattened, no locationName:
     -- <ListParam><member>one</member><member>two</member><member>...

     -- Non-flattened, alternate memberName and itemName:
     -- <AlternateName><NotMember>one</NotMember><NotMember>...

     -- [Member]

     -- Flattened, no locationName:
     -- <ListParam>one</ListParam><ListParam>two</ListParam>...

    -- go Query    True  = Nothing
    -- go Query    False = Just item

    -- go EC2      True  = Nothing
    -- go EC2      False = Just item

    -- go JSON     _     = Nothing
    -- go RestJSON _     = Nothing

    -- go RestXML  True  = Nothing
    -- go RestXML  False = Just item

-- FIXME: Go through the other SDK's tests to ensure correctness.
memberName :: Protocol
           -> Direction
           -> Id
           -> RefF a
           -> Text
memberName p d n r = go p d
  where
    go EC2 Input = upperHead $ fromMaybe key (r ^. refQueryName)
    go _   _     = key

    -- Use the locationName on the struct member if present,
    -- otherwise the struct member id.
    key = fromMaybe (n ^. memberId) (r ^. refLocationName)
