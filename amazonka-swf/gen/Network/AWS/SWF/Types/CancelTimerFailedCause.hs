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
-- Module      : Network.AWS.SWF.Types.CancelTimerFailedCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CancelTimerFailedCause
  ( CancelTimerFailedCause
      ( ..,
        CancelTimerFailedCause_OPERATION_NOT_PERMITTED,
        CancelTimerFailedCause_TIMER_ID_UNKNOWN
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CancelTimerFailedCause = CancelTimerFailedCause'
  { fromCancelTimerFailedCause ::
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

pattern CancelTimerFailedCause_OPERATION_NOT_PERMITTED :: CancelTimerFailedCause
pattern CancelTimerFailedCause_OPERATION_NOT_PERMITTED = CancelTimerFailedCause' "OPERATION_NOT_PERMITTED"

pattern CancelTimerFailedCause_TIMER_ID_UNKNOWN :: CancelTimerFailedCause
pattern CancelTimerFailedCause_TIMER_ID_UNKNOWN = CancelTimerFailedCause' "TIMER_ID_UNKNOWN"

{-# COMPLETE
  CancelTimerFailedCause_OPERATION_NOT_PERMITTED,
  CancelTimerFailedCause_TIMER_ID_UNKNOWN,
  CancelTimerFailedCause'
  #-}
