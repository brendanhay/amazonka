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

module Compiler.Protocol
    ( Names(..)
    , memberName
    , nestedNames
    , suffix
    ) where

import           Compiler.Types
import           Control.Comonad.Cofree
import           Control.Lens
import           Data.Maybe
import           Data.Text              (Text)
import           Data.Text.Manipulate

suffix :: Protocol -> Text
suffix = \case
    JSON     -> "JSON"
    RestJSON -> "JSON"
    RestXML  -> "XML"
    Query    -> "XML"
    EC2      -> "XML"

data Names
    = NMap  (Maybe Text) Text Text Text
    | NList (Maybe Text) Text
    | NName Text

memberName :: Protocol -> Direction -> Id -> RefF (Shape a) -> Text
memberName p d n r =
    case nestedNames p d n r of
        NMap  mn e _ _ -> fromMaybe e mn
        NList mn i     -> fromMaybe i mn
        NName x        -> x

nestedNames :: Protocol -> Direction -> Id -> RefF (Shape a) -> Names
nestedNames p d n r =
    case unwrap (r ^. refAnn) of
        Map  m -> mapNames  p d n r m
        List l -> listNames p d n r l
        _      -> NName (name p d n r)

mapNames :: Protocol -> Direction -> Id -> RefF a -> MapF a -> Names
mapNames p d n r m
    | flatten p m = NMap Nothing   mn      kn vn
    | otherwise   = NMap (Just mn) "entry" kn vn
  where
    mn = name p d n r
    kn = fromMaybe "key"   (m ^. mapKey   . refLocationName)
    vn = fromMaybe "value" (m ^. mapValue . refLocationName)

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

listNames :: Protocol -> Direction -> Id -> RefF  a -> ListF a -> Names
listNames p d n r l
     | flatten p l = NList Nothing   (fromMaybe mn ln)
     | otherwise   = NList (Just mn) (fromMaybe "member" ln)
  where
    mn = name p d n r
    ln = l ^. listItem . refLocationName

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
name :: Protocol -> Direction -> Id -> RefF a -> Text
name p d n r = go p d
  where
    go EC2 Input = upperHead $ fromMaybe key (r ^. refQueryName)
    go _   _     = key

    -- Use the locationName on the struct member if present,
    -- otherwise the struct member id.
    key = fromMaybe (n ^. memberId) (r ^. refLocationName)

flatten :: HasInfo a => Protocol -> a -> Bool
flatten EC2 _ = True
flatten _   i = i ^. infoFlattened
