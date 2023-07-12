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
-- Module      : Amazonka.SWF.Types.StartTimerFailedCause
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.StartTimerFailedCause
  ( StartTimerFailedCause
      ( ..,
        StartTimerFailedCause_OPEN_TIMERS_LIMIT_EXCEEDED,
        StartTimerFailedCause_OPERATION_NOT_PERMITTED,
        StartTimerFailedCause_TIMER_CREATION_RATE_EXCEEDED,
        StartTimerFailedCause_TIMER_ID_ALREADY_IN_USE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StartTimerFailedCause = StartTimerFailedCause'
  { fromStartTimerFailedCause ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
