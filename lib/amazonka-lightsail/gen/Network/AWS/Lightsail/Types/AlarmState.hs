{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AlarmState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AlarmState where

import Network.AWS.Prelude

data AlarmState
  = Alarm
  | InsufficientData
  | OK
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

instance FromText AlarmState where
  parser =
    takeLowerText >>= \case
      "alarm" -> pure Alarm
      "insufficient_data" -> pure InsufficientData
      "ok" -> pure OK
      e ->
        fromTextError $
          "Failure parsing AlarmState from value: '" <> e
            <> "'. Accepted values: alarm, insufficient_data, ok"

instance ToText AlarmState where
  toText = \case
    Alarm -> "ALARM"
    InsufficientData -> "INSUFFICIENT_DATA"
    OK -> "OK"

instance Hashable AlarmState

instance NFData AlarmState

instance ToByteString AlarmState

instance ToQuery AlarmState

instance ToHeader AlarmState

instance ToJSON AlarmState where
  toJSON = toJSONText

instance FromJSON AlarmState where
  parseJSON = parseJSONText "AlarmState"
