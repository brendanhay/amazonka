{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditFrequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditFrequency where

import Network.AWS.Prelude

data AuditFrequency
  = Biweekly
  | Daily
  | Monthly
  | Weekly
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

instance FromText AuditFrequency where
  parser =
    takeLowerText >>= \case
      "biweekly" -> pure Biweekly
      "daily" -> pure Daily
      "monthly" -> pure Monthly
      "weekly" -> pure Weekly
      e ->
        fromTextError $
          "Failure parsing AuditFrequency from value: '" <> e
            <> "'. Accepted values: biweekly, daily, monthly, weekly"

instance ToText AuditFrequency where
  toText = \case
    Biweekly -> "BIWEEKLY"
    Daily -> "DAILY"
    Monthly -> "MONTHLY"
    Weekly -> "WEEKLY"

instance Hashable AuditFrequency

instance NFData AuditFrequency

instance ToByteString AuditFrequency

instance ToQuery AuditFrequency

instance ToHeader AuditFrequency

instance ToJSON AuditFrequency where
  toJSON = toJSONText

instance FromJSON AuditFrequency where
  parseJSON = parseJSONText "AuditFrequency"
