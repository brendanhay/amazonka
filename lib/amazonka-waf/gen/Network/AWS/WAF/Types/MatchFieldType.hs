{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.MatchFieldType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.MatchFieldType where

import Network.AWS.Prelude

data MatchFieldType
  = AllQueryArgs
  | Body
  | Header
  | Method
  | QueryString
  | SingleQueryArg
  | URI
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText MatchFieldType where
  parser =
    takeLowerText >>= \case
      "all_query_args" -> pure AllQueryArgs
      "body" -> pure Body
      "header" -> pure Header
      "method" -> pure Method
      "query_string" -> pure QueryString
      "single_query_arg" -> pure SingleQueryArg
      "uri" -> pure URI
      e ->
        fromTextError $
          "Failure parsing MatchFieldType from value: '" <> e
            <> "'. Accepted values: all_query_args, body, header, method, query_string, single_query_arg, uri"

instance ToText MatchFieldType where
  toText = \case
    AllQueryArgs -> "ALL_QUERY_ARGS"
    Body -> "BODY"
    Header -> "HEADER"
    Method -> "METHOD"
    QueryString -> "QUERY_STRING"
    SingleQueryArg -> "SINGLE_QUERY_ARG"
    URI -> "URI"

instance Hashable MatchFieldType

instance NFData MatchFieldType

instance ToByteString MatchFieldType

instance ToQuery MatchFieldType

instance ToHeader MatchFieldType

instance ToJSON MatchFieldType where
  toJSON = toJSONText

instance FromJSON MatchFieldType where
  parseJSON = parseJSONText "MatchFieldType"
