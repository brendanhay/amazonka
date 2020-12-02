{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.FieldType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.FieldType where

import Network.AWS.Prelude

data FieldType
  = Boolean
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

instance FromText FieldType where
  parser =
    takeLowerText >>= \case
      "boolean" -> pure Boolean
      "number" -> pure Number
      "string" -> pure String
      e ->
        fromTextError $
          "Failure parsing FieldType from value: '" <> e
            <> "'. Accepted values: boolean, number, string"

instance ToText FieldType where
  toText = \case
    Boolean -> "Boolean"
    Number -> "Number"
    String -> "String"

instance Hashable FieldType

instance NFData FieldType

instance ToByteString FieldType

instance ToQuery FieldType

instance ToHeader FieldType

instance ToJSON FieldType where
  toJSON = toJSONText

instance FromJSON FieldType where
  parseJSON = parseJSONText "FieldType"
