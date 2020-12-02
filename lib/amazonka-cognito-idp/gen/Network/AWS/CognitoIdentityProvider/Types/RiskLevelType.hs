{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskLevelType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RiskLevelType where

import Network.AWS.Prelude

data RiskLevelType
  = High
  | Low
  | Medium
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

instance FromText RiskLevelType where
  parser =
    takeLowerText >>= \case
      "high" -> pure High
      "low" -> pure Low
      "medium" -> pure Medium
      e ->
        fromTextError $
          "Failure parsing RiskLevelType from value: '" <> e
            <> "'. Accepted values: high, low, medium"

instance ToText RiskLevelType where
  toText = \case
    High -> "High"
    Low -> "Low"
    Medium -> "Medium"

instance Hashable RiskLevelType

instance NFData RiskLevelType

instance ToByteString RiskLevelType

instance ToQuery RiskLevelType

instance ToHeader RiskLevelType

instance FromJSON RiskLevelType where
  parseJSON = parseJSONText "RiskLevelType"
