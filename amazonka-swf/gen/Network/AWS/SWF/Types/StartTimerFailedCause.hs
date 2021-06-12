{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartTimerFailedCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartTimerFailedCause
  ( StartTimerFailedCause
      ( ..,
        StartTimerFailedCause_OPEN_TIMERS_LIMIT_EXCEEDED,
        StartTimerFailedCause_OPERATION_NOT_PERMITTED,
        StartTimerFailedCause_TIMER_CREATION_RATE_EXCEEDED,
        StartTimerFailedCause_TIMER_ID_ALREADY_IN_USE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype StartTimerFailedCause = StartTimerFailedCause'
  { fromStartTimerFailedCause ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern StartTimerFailedCause_OPEN_TIMERS_LIMIT_EXCEEDED :: StartTimerFailedCause
pattern StartTimerFailedCause_OPEN_TIMERS_LIMIT_EXCEEDED = StartTimerFailedCause' "OPEN_TIMERS_LIMIT_EXCEEDED"

pattern StartTimerFailedCause_OPERATION_NOT_PERMITTED :: StartTimerFailedCause
pattern StartTimerFailedCause_OPERATION_NOT_PERMITTED = StartTimerFailedCause' "OPERATION_NOT_PERMITTED"

pattern StartTimerFailedCause_TIMER_CREATION_RATE_EXCEEDED :: StartTimerFailedCause
pattern StartTimerFailedCause_TIMER_CREATION_RATE_EXCEEDED = StartTimerFailedCause' "TIMER_CREATION_RATE_EXCEEDED"

pattern StartTimerFailedCause_TIMER_ID_ALREADY_IN_USE :: StartTimerFailedCause
pattern StartTimerFailedCause_TIMER_ID_ALREADY_IN_USE = StartTimerFailedCause' "TIMER_ID_ALREADY_IN_USE"

{-# COMPLETE
  StartTimerFailedCause_OPEN_TIMERS_LIMIT_EXCEEDED,
  StartTimerFailedCause_OPERATION_NOT_PERMITTED,
  StartTimerFailedCause_TIMER_CREATION_RATE_EXCEEDED,
  StartTimerFailedCause_TIMER_ID_ALREADY_IN_USE,
  StartTimerFailedCause'
  #-}
