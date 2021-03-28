{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.NotificationEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.NotificationEvent
  ( NotificationEvent
    ( NotificationEvent'
    , NotificationEventAll
    , NotificationEventInProgress
    , NotificationEventSuccess
    , NotificationEventTimedOut
    , NotificationEventCancelled
    , NotificationEventFailed
    , fromNotificationEvent
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype NotificationEvent = NotificationEvent'{fromNotificationEvent
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern NotificationEventAll :: NotificationEvent
pattern NotificationEventAll = NotificationEvent' "All"

pattern NotificationEventInProgress :: NotificationEvent
pattern NotificationEventInProgress = NotificationEvent' "InProgress"

pattern NotificationEventSuccess :: NotificationEvent
pattern NotificationEventSuccess = NotificationEvent' "Success"

pattern NotificationEventTimedOut :: NotificationEvent
pattern NotificationEventTimedOut = NotificationEvent' "TimedOut"

pattern NotificationEventCancelled :: NotificationEvent
pattern NotificationEventCancelled = NotificationEvent' "Cancelled"

pattern NotificationEventFailed :: NotificationEvent
pattern NotificationEventFailed = NotificationEvent' "Failed"

{-# COMPLETE 
  NotificationEventAll,

  NotificationEventInProgress,

  NotificationEventSuccess,

  NotificationEventTimedOut,

  NotificationEventCancelled,

  NotificationEventFailed,
  NotificationEvent'
  #-}
