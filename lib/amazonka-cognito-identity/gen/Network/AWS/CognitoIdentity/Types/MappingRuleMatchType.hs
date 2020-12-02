{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.MappingRuleMatchType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.MappingRuleMatchType where

import Network.AWS.Prelude

data MappingRuleMatchType
  = Contains
  | Equals
  | NotEqual
  | StartsWith
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

instance FromText MappingRuleMatchType where
  parser =
    takeLowerText >>= \case
      "contains" -> pure Contains
      "equals" -> pure Equals
      "notequal" -> pure NotEqual
      "startswith" -> pure StartsWith
      e ->
        fromTextError $
          "Failure parsing MappingRuleMatchType from value: '" <> e
            <> "'. Accepted values: contains, equals, notequal, startswith"

instance ToText MappingRuleMatchType where
  toText = \case
    Contains -> "Contains"
    Equals -> "Equals"
    NotEqual -> "NotEqual"
    StartsWith -> "StartsWith"

instance Hashable MappingRuleMatchType

instance NFData MappingRuleMatchType

instance ToByteString MappingRuleMatchType

instance ToQuery MappingRuleMatchType

instance ToHeader MappingRuleMatchType

instance ToJSON MappingRuleMatchType where
  toJSON = toJSONText

instance FromJSON MappingRuleMatchType where
  parseJSON = parseJSONText "MappingRuleMatchType"
