{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType where

import Network.AWS.Prelude

data ActionConfigurationPropertyType
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

instance FromText ActionConfigurationPropertyType where
  parser =
    takeLowerText >>= \case
      "boolean" -> pure Boolean
      "number" -> pure Number
      "string" -> pure String
      e ->
        fromTextError $
          "Failure parsing ActionConfigurationPropertyType from value: '" <> e
            <> "'. Accepted values: boolean, number, string"

instance ToText ActionConfigurationPropertyType where
  toText = \case
    Boolean -> "Boolean"
    Number -> "Number"
    String -> "String"

instance Hashable ActionConfigurationPropertyType

instance NFData ActionConfigurationPropertyType

instance ToByteString ActionConfigurationPropertyType

instance ToQuery ActionConfigurationPropertyType

instance ToHeader ActionConfigurationPropertyType

instance ToJSON ActionConfigurationPropertyType where
  toJSON = toJSONText

instance FromJSON ActionConfigurationPropertyType where
  parseJSON = parseJSONText "ActionConfigurationPropertyType"
