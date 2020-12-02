{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditFindingSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditFindingSeverity where

import Network.AWS.Prelude

data AuditFindingSeverity
  = Critical
  | High
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

instance FromText AuditFindingSeverity where
  parser =
    takeLowerText >>= \case
      "critical" -> pure Critical
      "high" -> pure High
      "low" -> pure Low
      "medium" -> pure Medium
      e ->
        fromTextError $
          "Failure parsing AuditFindingSeverity from value: '" <> e
            <> "'. Accepted values: critical, high, low, medium"

instance ToText AuditFindingSeverity where
  toText = \case
    Critical -> "CRITICAL"
    High -> "HIGH"
    Low -> "LOW"
    Medium -> "MEDIUM"

instance Hashable AuditFindingSeverity

instance NFData AuditFindingSeverity

instance ToByteString AuditFindingSeverity

instance ToQuery AuditFindingSeverity

instance ToHeader AuditFindingSeverity

instance FromJSON AuditFindingSeverity where
  parseJSON = parseJSONText "AuditFindingSeverity"
