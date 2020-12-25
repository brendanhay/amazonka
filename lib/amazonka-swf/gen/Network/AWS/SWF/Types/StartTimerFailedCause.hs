{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartTimerFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartTimerFailedCause
  ( StartTimerFailedCause
      ( StartTimerFailedCause',
        StartTimerFailedCauseTimerIdAlreadyInUse,
        StartTimerFailedCauseOpenTimersLimitExceeded,
        StartTimerFailedCauseTimerCreationRateExceeded,
        StartTimerFailedCauseOperationNotPermitted,
        fromStartTimerFailedCause
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype StartTimerFailedCause = StartTimerFailedCause'
  { fromStartTimerFailedCause ::
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

pattern StartTimerFailedCauseTimerIdAlreadyInUse :: StartTimerFailedCause
pattern StartTimerFailedCauseTimerIdAlreadyInUse = StartTimerFailedCause' "TIMER_ID_ALREADY_IN_USE"

pattern StartTimerFailedCauseOpenTimersLimitExceeded :: StartTimerFailedCause
pattern StartTimerFailedCauseOpenTimersLimitExceeded = StartTimerFailedCause' "OPEN_TIMERS_LIMIT_EXCEEDED"

pattern StartTimerFailedCauseTimerCreationRateExceeded :: StartTimerFailedCause
pattern StartTimerFailedCauseTimerCreationRateExceeded = StartTimerFailedCause' "TIMER_CREATION_RATE_EXCEEDED"

pattern StartTimerFailedCauseOperationNotPermitted :: StartTimerFailedCause
pattern StartTimerFailedCauseOperationNotPermitted = StartTimerFailedCause' "OPERATION_NOT_PERMITTED"

{-# COMPLETE
  StartTimerFailedCauseTimerIdAlreadyInUse,
  StartTimerFailedCauseOpenTimersLimitExceeded,
  StartTimerFailedCauseTimerCreationRateExceeded,
  StartTimerFailedCauseOperationNotPermitted,
  StartTimerFailedCause'
  #-}
