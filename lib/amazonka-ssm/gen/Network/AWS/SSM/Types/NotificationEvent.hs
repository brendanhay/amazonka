{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.NotificationEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.NotificationEvent where

import Network.AWS.Prelude

data NotificationEvent
  = NEAll
  | NECancelled
  | NEFailed
  | NEInProgress
  | NESuccess
  | NETimedOut
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

instance FromText NotificationEvent where
  parser =
    takeLowerText >>= \case
      "all" -> pure NEAll
      "cancelled" -> pure NECancelled
      "failed" -> pure NEFailed
      "inprogress" -> pure NEInProgress
      "success" -> pure NESuccess
      "timedout" -> pure NETimedOut
      e ->
        fromTextError $
          "Failure parsing NotificationEvent from value: '" <> e
            <> "'. Accepted values: all, cancelled, failed, inprogress, success, timedout"

instance ToText NotificationEvent where
  toText = \case
    NEAll -> "All"
    NECancelled -> "Cancelled"
    NEFailed -> "Failed"
    NEInProgress -> "InProgress"
    NESuccess -> "Success"
    NETimedOut -> "TimedOut"

instance Hashable NotificationEvent

instance NFData NotificationEvent

instance ToByteString NotificationEvent

instance ToQuery NotificationEvent

instance ToHeader NotificationEvent

instance ToJSON NotificationEvent where
  toJSON = toJSONText

instance FromJSON NotificationEvent where
  parseJSON = parseJSONText "NotificationEvent"
