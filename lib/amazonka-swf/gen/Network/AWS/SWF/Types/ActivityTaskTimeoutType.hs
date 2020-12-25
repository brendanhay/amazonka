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
        ActivityTaskTimeoutTypeStartToClose,
        ActivityTaskTimeoutTypeScheduleToStart,
        ActivityTaskTimeoutTypeScheduleToClose,
        ActivityTaskTimeoutTypeHeartbeat,
        fromActivityTaskTimeoutType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ActivityTaskTimeoutType = ActivityTaskTimeoutType'
  { fromActivityTaskTimeoutType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ActivityTaskTimeoutTypeStartToClose :: ActivityTaskTimeoutType
pattern ActivityTaskTimeoutTypeStartToClose = ActivityTaskTimeoutType' "START_TO_CLOSE"

pattern ActivityTaskTimeoutTypeScheduleToStart :: ActivityTaskTimeoutType
pattern ActivityTaskTimeoutTypeScheduleToStart = ActivityTaskTimeoutType' "SCHEDULE_TO_START"

pattern ActivityTaskTimeoutTypeScheduleToClose :: ActivityTaskTimeoutType
pattern ActivityTaskTimeoutTypeScheduleToClose = ActivityTaskTimeoutType' "SCHEDULE_TO_CLOSE"

pattern ActivityTaskTimeoutTypeHeartbeat :: ActivityTaskTimeoutType
pattern ActivityTaskTimeoutTypeHeartbeat = ActivityTaskTimeoutType' "HEARTBEAT"

{-# COMPLETE
  ActivityTaskTimeoutTypeStartToClose,
  ActivityTaskTimeoutTypeScheduleToStart,
  ActivityTaskTimeoutTypeScheduleToClose,
  ActivityTaskTimeoutTypeHeartbeat,
  ActivityTaskTimeoutType'
  #-}
