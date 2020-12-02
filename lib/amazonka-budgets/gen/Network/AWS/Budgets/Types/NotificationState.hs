{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.NotificationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.NotificationState where

import Network.AWS.Prelude

data NotificationState
  = Alarm
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

instance FromText NotificationState where
  parser =
    takeLowerText >>= \case
      "alarm" -> pure Alarm
      "ok" -> pure OK
      e ->
        fromTextError $
          "Failure parsing NotificationState from value: '" <> e
            <> "'. Accepted values: alarm, ok"

instance ToText NotificationState where
  toText = \case
    Alarm -> "ALARM"
    OK -> "OK"

instance Hashable NotificationState

instance NFData NotificationState

instance ToByteString NotificationState

instance ToQuery NotificationState

instance ToHeader NotificationState

instance ToJSON NotificationState where
  toJSON = toJSONText

instance FromJSON NotificationState where
  parseJSON = parseJSONText "NotificationState"
