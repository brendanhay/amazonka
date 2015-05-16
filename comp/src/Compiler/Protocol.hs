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
import           Data.Maybe
import           Data.Text            (Text)
import           Data.Text.Manipulate

timestamp :: Protocol -> Timestamp
timestamp = \case
    JSON     -> POSIX
    RestJSON -> POSIX
    XML      -> ISO8601
    RestXML  -> ISO8601
    Query    -> ISO8601
    EC2      -> ISO8601

instances :: Protocol -> Relation -> [Instance]
instances p = \case
    Bi         -> [inp, out]
    Uni Input  -> [inp]
    Uni Output -> [out]
  where
    inp = case p of
        JSON     -> ToJSON
        RestJSON -> ToJSON
        XML      -> ToXML
        RestXML  -> ToXML
        Query    -> ToQuery
        EC2      -> ToQuery

    out = case p of
        JSON     -> FromJSON
        RestJSON -> FromJSON
        XML      -> FromXML
        RestXML  -> FromXML
        Query    -> FromXML
        EC2      -> FromXML

direction :: Instance -> Direction
direction = \case
    ToJSON   -> Input
    ToXML    -> Input
    ToQuery  -> Input
    FromJSON -> Output
    FromXML  -> Output

satisfies :: Instance -> (a -> Maybe Location) -> [a] -> [a]
satisfies i f = filter (match i . f)
  where
    -- Protocol classes (total)
    match FromJSON  = const True
    match FromXML   = const True
    match FromBody  = const True

    match ToJSON    = discard
    match ToXML     = discard
    match ToQuery   = discard

    -- Request classes (partial)
    match ToBody    = (== Just Body)
    match ToHeaders = flip elem [Just Headers, Just Header]
    match ToPath    = flip elem [Just URI, Just Querystring]

    discard x = not $
           match ToPath x
        || match ToBody x
        || match ToHeaders x

listName :: Protocol  -- ^ Service protocol.
         -> Direction -- ^ The serialisation direction.
         -> Id        -- ^ The member id.
         -> RefF  a   -- ^ The member reference.
         -> ListF a   -- ^ The list shape pointed to by the member reference.
         -> (Text, Maybe Text)
listName p d n r l = go p d (l ^. infoFlattened)
  where
    go :: Protocol
       -> Direction
       -> Bool -- ^ Flattened?
       -> (Text, Maybe Text)

    go Query    _      True  = (key, Nothing)
    go Query    _      False = (key, Just item)

    go EC2      Input  _     = (upperHead $ fromMaybe key (r ^. refQueryName), Nothing)
    go EC2      Output True  = (key, Nothing)
    go EC2      Output False = (key, Just item)

    go JSON     _      _     = (key, Nothing)

    go RestJSON _      _     = (key, Nothing)

    go RestXML  _      True  = (key, Nothing)
    go RestXML  _      False = (key, Just item)

    -- input XML       True  = (parent, Nothing) -
    -- input XML       False = (parent, Just element)

    -- Use the locationName on the struct member if present,
    -- otherwise the struct member id.
    key = fromMaybe (n ^. memberId) (r ^. refLocationName)

    -- Use the locationName on the actual list element pointed
    -- to by the struct member reference if present,
    -- otherwise default to 'member'.
    item = fromMaybe "member" (l ^. listItem . refLocationName)
