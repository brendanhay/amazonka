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

-- memberName :: Protocol -> Uni -> Id -> Ref -> Text
-- memberName = undefined

-- itemName :: Protocol -> Uni -> Ref -> Shape -> Maybe Text
-- itemName = undefined

-- -- keyName
-- -- valueName

-- Just focus on getting the types rendered and worry about request/response later.

-- Render the `member op location` and vice versa lines for each Inst

-- instanceOp :: Inst -> Id -> Text -> Exp
-- instanceOp i l m = infixapp () (qop (op i)) ()

--     This is the function that needs to call listName, keyName, valueName

-- -- FIXME: Parameterize Ref over a, which can be swapped out for the actual shape.

-- instanceOp :: Protocol -> Inst -> Id -> Ref -> Shape -> Exp
-- instanceOp p i k v = \case
--     List {} ->
--     Map  {} ->
--     _ ->
--   where
--     name = memberName p dir k v
--     item = itemName p dir v v'
--     key  = itemName p dir v v'
--     val  = itemName p dir v v'

--     op =
--         case i of
--             ToJSON -> ".="

--     dir =
--         case i of
--             ToJSON -> Input

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

-- listName :: Protocol    -- ^ Service protocol
--          -> (Id,   Ref) -- ^ The struct member Id and Ref
--          -> (Info, Ref) -- ^ The actual list type item's member ref
--          -> ((Text, Maybe Text), (Text, Maybe Text))
-- listName p (k, v) (i, e) = ((inp, input p flat), (out, output p flat))
--   where
--     flat = i ^. infoFlattened

--     -- Use the locationName on the struct member if present,
--     -- otherwise the struct member id.
--     (inp, out) = memberName p (k, v)

--     -- Use the locationName on list element if present,
--     -- otherwise defualt to 'member'.
--     element = fromMaybe "member" (e ^. refLocationName)

--     -- input XML       True  = (parent, Nothing) -
--     -- input XML       False = (parent, Just element)

--     input Query     True  = Nothing
--     input Query     False = Just element
--     input EC2       _     = Nothing
--     input JSON      _     = Nothing
--     input RestJSON  _     = Nothing
--     input RestXML   True  = Nothing
--     input RestXML   False = Just element

--     output Query    True  = Nothing
--     output Query    False = Just element
--     output EC2      True  = Nothing
--     output EC2      False = Just element
--     output JSON     _     = Nothing
--     output RestJSON _     = Nothing
--     output RestXML  True  = Nothing
--     output RestXML  False = Just element

-- listMember :: Protocol -- ^ Service protocol
--            -> Bool     -- ^ Request
--            -> Id       -- ^ The member id
--            -> Ref      -- ^ The member ref
--            -> Text

-- inputMember p k = fromMaybe (k ^. memberId) . view refLocationName

-- inputListMember p k v
--     | EC2 <- p  = upperHead $ fromMaybe parent (v ^. refQueryName)
--     | otherwise = parent
--   where
--     -- Use the locationName on the struct member if present,
--     -- otherwise the struct member id.
--     parent = fromMaybe (k ^. memberId) (v ^. refLocationName)

-- outputListMember k v = fromMaybe (k ^. memberId) (v ^. refLocationName)

-- listItem :: Info -> Ref -> Text
-- listItem = fromMaybe "member" . view refLocationName


-- memberName :: Protocol -> (Id, Ref) -> (Text, Text)
-- memberName p (k, v) = (input p, member)
--   where
--     input EC2 = upperHead $ fromMaybe member (v ^. refQueryName)
--     input _   = member

    -- member = fromMaybe (k ^. memberId) (v ^. refLocationName)
