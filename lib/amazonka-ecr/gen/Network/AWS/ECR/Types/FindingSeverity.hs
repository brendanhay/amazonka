{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.FindingSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.FindingSeverity where

import Network.AWS.Prelude

data FindingSeverity
  = Critical
  | High
  | Informational
  | Low
  | Medium
  | Undefined
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

instance FromText FindingSeverity where
  parser =
    takeLowerText >>= \case
      "critical" -> pure Critical
      "high" -> pure High
      "informational" -> pure Informational
      "low" -> pure Low
      "medium" -> pure Medium
      "undefined" -> pure Undefined
      e ->
        fromTextError $
          "Failure parsing FindingSeverity from value: '" <> e
            <> "'. Accepted values: critical, high, informational, low, medium, undefined"

instance ToText FindingSeverity where
  toText = \case
    Critical -> "CRITICAL"
    High -> "HIGH"
    Informational -> "INFORMATIONAL"
    Low -> "LOW"
    Medium -> "MEDIUM"
    Undefined -> "UNDEFINED"

instance Hashable FindingSeverity

instance NFData FindingSeverity

instance ToByteString FindingSeverity

instance ToQuery FindingSeverity

instance ToHeader FindingSeverity

instance FromJSON FindingSeverity where
  parseJSON = parseJSONText "FindingSeverity"
