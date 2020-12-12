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
        STFCOpenTimersLimitExceeded,
        STFCOperationNotPermitted,
        STFCTimerCreationRateExceeded,
        STFCTimerIdAlreadyInUse
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StartTimerFailedCause = StartTimerFailedCause' Lude.Text
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

pattern STFCOpenTimersLimitExceeded :: StartTimerFailedCause
pattern STFCOpenTimersLimitExceeded = StartTimerFailedCause' "OPEN_TIMERS_LIMIT_EXCEEDED"

pattern STFCOperationNotPermitted :: StartTimerFailedCause
pattern STFCOperationNotPermitted = StartTimerFailedCause' "OPERATION_NOT_PERMITTED"

pattern STFCTimerCreationRateExceeded :: StartTimerFailedCause
pattern STFCTimerCreationRateExceeded = StartTimerFailedCause' "TIMER_CREATION_RATE_EXCEEDED"

pattern STFCTimerIdAlreadyInUse :: StartTimerFailedCause
pattern STFCTimerIdAlreadyInUse = StartTimerFailedCause' "TIMER_ID_ALREADY_IN_USE"

{-# COMPLETE
  STFCOpenTimersLimitExceeded,
  STFCOperationNotPermitted,
  STFCTimerCreationRateExceeded,
  STFCTimerIdAlreadyInUse,
  StartTimerFailedCause'
  #-}
