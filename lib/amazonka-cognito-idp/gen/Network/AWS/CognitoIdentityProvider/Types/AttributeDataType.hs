{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AttributeDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AttributeDataType where

import Network.AWS.Prelude

data AttributeDataType
  = Boolean
  | DateTime
  | Number
  | String
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

instance FromText AttributeDataType where
  parser =
    takeLowerText >>= \case
      "boolean" -> pure Boolean
      "datetime" -> pure DateTime
      "number" -> pure Number
      "string" -> pure String
      e ->
        fromTextError $
          "Failure parsing AttributeDataType from value: '" <> e
            <> "'. Accepted values: boolean, datetime, number, string"

instance ToText AttributeDataType where
  toText = \case
    Boolean -> "Boolean"
    DateTime -> "DateTime"
    Number -> "Number"
    String -> "String"

instance Hashable AttributeDataType

instance NFData AttributeDataType

instance ToByteString AttributeDataType

instance ToQuery AttributeDataType

instance ToHeader AttributeDataType

instance ToJSON AttributeDataType where
  toJSON = toJSONText

instance FromJSON AttributeDataType where
  parseJSON = parseJSONText "AttributeDataType"
