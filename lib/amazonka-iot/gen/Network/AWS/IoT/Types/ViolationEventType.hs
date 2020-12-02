{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ViolationEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ViolationEventType where

import Network.AWS.Prelude

data ViolationEventType
  = AlarmCleared
  | AlarmInvalidated
  | InAlarm
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

instance FromText ViolationEventType where
  parser =
    takeLowerText >>= \case
      "alarm-cleared" -> pure AlarmCleared
      "alarm-invalidated" -> pure AlarmInvalidated
      "in-alarm" -> pure InAlarm
      e ->
        fromTextError $
          "Failure parsing ViolationEventType from value: '" <> e
            <> "'. Accepted values: alarm-cleared, alarm-invalidated, in-alarm"

instance ToText ViolationEventType where
  toText = \case
    AlarmCleared -> "alarm-cleared"
    AlarmInvalidated -> "alarm-invalidated"
    InAlarm -> "in-alarm"

instance Hashable ViolationEventType

instance NFData ViolationEventType

instance ToByteString ViolationEventType

instance ToQuery ViolationEventType

instance ToHeader ViolationEventType

instance FromJSON ViolationEventType where
  parseJSON = parseJSONText "ViolationEventType"
