{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleDestinationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleDestinationStatus where

import Network.AWS.Prelude

data TopicRuleDestinationStatus
  = TRDSDisabled
  | TRDSEnabled
  | TRDSError'
  | TRDSInProgress
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

instance FromText TopicRuleDestinationStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure TRDSDisabled
      "enabled" -> pure TRDSEnabled
      "error" -> pure TRDSError'
      "in_progress" -> pure TRDSInProgress
      e ->
        fromTextError $
          "Failure parsing TopicRuleDestinationStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled, error, in_progress"

instance ToText TopicRuleDestinationStatus where
  toText = \case
    TRDSDisabled -> "DISABLED"
    TRDSEnabled -> "ENABLED"
    TRDSError' -> "ERROR"
    TRDSInProgress -> "IN_PROGRESS"

instance Hashable TopicRuleDestinationStatus

instance NFData TopicRuleDestinationStatus

instance ToByteString TopicRuleDestinationStatus

instance ToQuery TopicRuleDestinationStatus

instance ToHeader TopicRuleDestinationStatus

instance ToJSON TopicRuleDestinationStatus where
  toJSON = toJSONText

instance FromJSON TopicRuleDestinationStatus where
  parseJSON = parseJSONText "TopicRuleDestinationStatus"
