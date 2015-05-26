{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Compiler.AST.Data.Instance
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Data.Instance where

import           Compiler.AST.Data.Field
import           Compiler.Types
import           Control.Lens
import           Data.Aeson
import           Data.Maybe
import           Data.Text               (Text)
import qualified Data.Text               as Text

data Instance
    = FromXML   [Field]
    | FromJSON  [Field]
    | ToXML     [Field]
    | ToJSON    [Field]
    | ToQuery   [Field]

      -- headers, status, deserialise or body
    | FromRes   [Field] (Maybe Field) (Either Instance Field)

      -- headers, path, query, serialise or body
    | ToReq     [Field] [Either Field Text] [Either Field Text] (Either Instance Field)
      deriving (Eq, Show)

instToText :: Instance -> Text
instToText = \case
    FromJSON  {} -> "FromJSON"
    FromXML   {} -> "FromXML"
    ToJSON    {} -> "ToJSON"
    ToXML     {} -> "ToXML"
    ToQuery   {} -> "ToQuery"
    FromRes   {} -> "FromRes"
    ToReq     {} -> "ToReq"

instance ToJSON Instance where
    toJSON = toJSON . instToText

-- derive :: HasRelated a => Protocol -> a -> [Field] -> Either Error [Instance]
-- derive p (view related -> r) fs =
--   -- where
--   --   satisfy (`elem` )

sumInstances :: HasRelation a => Protocol -> a -> [Text]
sumInstances p r = map instToText base
  where
    base = case r ^. relMode of
        Bi         -> [inp [], out []]
        Uni Input  -> [inp []]
        Uni Output -> [out []]

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

prodInstances :: HasRelated a
              => Protocol
              -> a
              -> [Field]
              -> Either Error [Instance]
prodInstances p r =
    case r ^. related of
        RReq _ _ h -> request  p h
        RRes    {} -> response p
        RShape  {} -> pure . shape p

-- nub [ToBody, ToHeaders, ToPath, ToQuery, inp]
    -- base :: [[Field] -> Instance]
    -- base = case r ^. relMode of
    --     Bi         -> [inp, out]
    --     Uni Input  -> [inp]
    --     Uni Output -> [out]

    -- inp :: [Field] -> Instance
    -- inp = case p of
    --     JSON     -> ToJSON
    --     RestJSON -> ToJSON
    --     XML      -> ToXML
    --     RestXML  -> ToXML
    --     Query    -> ToQuery
    --     EC2      -> ToQuery

    -- out :: [Field] -> Instance
    -- out = case p of
    --     JSON     -> FromJSON
    --     RestJSON -> FromJSON
    --     XML      -> FromXML
    --     RestXML  -> FromXML
    --     Query    -> FromXML
    --     EC2      -> FromXML

request :: Protocol -> HTTP Identity -> [Field] -> Either Error [Instance]
request = undefined

response :: Protocol -> [Field] -> Either Error [Instance]
response = undefined

shape :: Protocol -> [Field] -> [Instance]
shape = undefined

satisfies :: [Location] -> [Field] -> [Field]
satisfies xs = satisfy (`elem` xs)

satisfy :: (Location -> Bool) -> [Field] -> [Field]
satisfy f = filter (maybe False f . view fieldLocation)

--     -- Protocol classes (total)
--     match FromJSON  = const True
--     match FromXML   = const True
--     match FromBody  = const True

--     -- FIXME: Is it a streaming request?
--     -- If so Then it shouldn't have tojson/toxml instances.

--     match ToJSON    = discard
--     match ToXML     = discard
--     match ToQuery
-- --        | op        = (== Just Querystring)
--         | otherwise = discard

--     -- Request classes (partial)
--     match ToBody    = (== Just Body)
--     match ToHeaders = flip elem [Just Headers, Just Header]
--     match ToPath    = (== Just URI)

--     discard = flip notElem
--         [ Just Headers
--         , Just Header
--         , Just URI
--         , Just Querystring
--         ]

-- placement :: Instance -> Direction
-- placement = \case
--     FromJSON  -> Output
--     FromXML   -> Output
--     ToJSON    -> Input
--     ToXML     -> Input
--     ToQuery   -> Input
--     ToBody    -> Input
--     ToHeaders -> Input
--     ToPath    -> Input

-- FIXME: this needs to take into account location?
--   perhaps constraint a to Info?
--   what about the StructF's 'payload' field?

--   Make sense to use the fromjson instance of the struct
--   to find the parsed member payload and set it's location to body?

-- operator :: Instance
--          -> Bool -- ^ Is the field required?
--          -> Text
-- operator = go
--   where
--     go FromJSON True  = ".:"
--     go FromJSON False = ".:?"
--     go FromXML  True  = ".@"
--     go FromXML  False = ".@?"
--     go ToJSON    _    = ".="
--     go ToXML     _    = "=@"
--     go ToQuery   _    = "=?"
--     go ToBody    _    = "???"
--     go ToHeaders _    = "=:"
--     go ToPath    _    = "<>"
