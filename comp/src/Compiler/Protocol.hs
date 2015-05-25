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
    Bi  _        -> [inp, out]
    Uni _ Input  -> [inp]
    Uni _ Output -> [out]
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
    FromJSON -> Output
    FromXML  -> Output
    ToJSON   -> Input
    ToXML    -> Input
    ToQuery  -> Input

-- FIXME: this needs to take into account location?
--   perhaps constraint a to Info?
--   what about the StructF's 'payload' field?

--   Make sense to use the fromjson instance of the struct
--   to find the parsed member payload and set it's location to body?

satisfies :: Instance -> (a -> Maybe Location) -> [a] -> [a]
satisfies i f = filter (match i . f)
  where
    -- Protocol classes (total)
    match FromJSON  = const True
    match FromXML   = const True
    match FromBody  = const True

    -- FIXME: Is it a streaming request?
    -- If so Then it shouldn't have tojson/toxml instances.

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

operator :: Instance
         -> Bool -- ^ Is the field required?
         -> Text
operator = go
  where
    go FromJSON True  = ".:"
    go FromJSON False = ".:?"
    go FromXML  True  = ".@"
    go FromXML  False = ".@?"
    go ToJSON   _     = ".="
    go ToXML    _     = "=@"
    go ToQuery  _     = "=?"

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

    go EC2      Input  _     = Nothing
    go EC2      Output True  = Nothing
    go EC2      Output False = Just item

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
