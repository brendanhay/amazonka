{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskTimeoutType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskTimeoutType
  ( ActivityTaskTimeoutType
      ( ActivityTaskTimeoutType',
        ATTTStartToClose,
        ATTTScheduleToStart,
        ATTTScheduleToClose,
        ATTTHeartbeat
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActivityTaskTimeoutType = ActivityTaskTimeoutType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ATTTStartToClose :: ActivityTaskTimeoutType
pattern ATTTStartToClose = ActivityTaskTimeoutType' "START_TO_CLOSE"

pattern ATTTScheduleToStart :: ActivityTaskTimeoutType
pattern ATTTScheduleToStart = ActivityTaskTimeoutType' "SCHEDULE_TO_START"

pattern ATTTScheduleToClose :: ActivityTaskTimeoutType
pattern ATTTScheduleToClose = ActivityTaskTimeoutType' "SCHEDULE_TO_CLOSE"

pattern ATTTHeartbeat :: ActivityTaskTimeoutType
pattern ATTTHeartbeat = ActivityTaskTimeoutType' "HEARTBEAT"

{-# COMPLETE
  ATTTStartToClose,
  ATTTScheduleToStart,
  ATTTScheduleToClose,
  ATTTHeartbeat,
  ActivityTaskTimeoutType'
  #-}
