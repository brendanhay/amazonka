{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.MessageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.MessageType where

import Network.AWS.Prelude

data MessageType
  = ConfigurationItemChangeNotification
  | ConfigurationSnapshotDeliveryCompleted
  | OversizedConfigurationItemChangeNotification
  | ScheduledNotification
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

instance FromText MessageType where
  parser =
    takeLowerText >>= \case
      "configurationitemchangenotification" -> pure ConfigurationItemChangeNotification
      "configurationsnapshotdeliverycompleted" -> pure ConfigurationSnapshotDeliveryCompleted
      "oversizedconfigurationitemchangenotification" -> pure OversizedConfigurationItemChangeNotification
      "schedulednotification" -> pure ScheduledNotification
      e ->
        fromTextError $
          "Failure parsing MessageType from value: '" <> e
            <> "'. Accepted values: configurationitemchangenotification, configurationsnapshotdeliverycompleted, oversizedconfigurationitemchangenotification, schedulednotification"

instance ToText MessageType where
  toText = \case
    ConfigurationItemChangeNotification -> "ConfigurationItemChangeNotification"
    ConfigurationSnapshotDeliveryCompleted -> "ConfigurationSnapshotDeliveryCompleted"
    OversizedConfigurationItemChangeNotification -> "OversizedConfigurationItemChangeNotification"
    ScheduledNotification -> "ScheduledNotification"

instance Hashable MessageType

instance NFData MessageType

instance ToByteString MessageType

instance ToQuery MessageType

instance ToHeader MessageType

instance ToJSON MessageType where
  toJSON = toJSONText

instance FromJSON MessageType where
  parseJSON = parseJSONText "MessageType"
