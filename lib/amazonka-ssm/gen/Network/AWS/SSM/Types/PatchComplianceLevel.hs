{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchComplianceLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchComplianceLevel where

import Network.AWS.Prelude

data PatchComplianceLevel
  = Critical
  | High
  | Informational
  | Low
  | Medium
  | Unspecified
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

instance FromText PatchComplianceLevel where
  parser =
    takeLowerText >>= \case
      "critical" -> pure Critical
      "high" -> pure High
      "informational" -> pure Informational
      "low" -> pure Low
      "medium" -> pure Medium
      "unspecified" -> pure Unspecified
      e ->
        fromTextError $
          "Failure parsing PatchComplianceLevel from value: '" <> e
            <> "'. Accepted values: critical, high, informational, low, medium, unspecified"

instance ToText PatchComplianceLevel where
  toText = \case
    Critical -> "CRITICAL"
    High -> "HIGH"
    Informational -> "INFORMATIONAL"
    Low -> "LOW"
    Medium -> "MEDIUM"
    Unspecified -> "UNSPECIFIED"

instance Hashable PatchComplianceLevel

instance NFData PatchComplianceLevel

instance ToByteString PatchComplianceLevel

instance ToQuery PatchComplianceLevel

instance ToHeader PatchComplianceLevel

instance ToJSON PatchComplianceLevel where
  toJSON = toJSONText

instance FromJSON PatchComplianceLevel where
  parseJSON = parseJSONText "PatchComplianceLevel"
