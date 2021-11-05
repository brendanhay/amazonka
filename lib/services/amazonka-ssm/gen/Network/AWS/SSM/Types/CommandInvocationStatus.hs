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
-- Module      : Amazonka.SSM.Types.CommandInvocationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.CommandInvocationStatus
  ( CommandInvocationStatus
      ( ..,
        CommandInvocationStatus_Cancelled,
        CommandInvocationStatus_Cancelling,
        CommandInvocationStatus_Delayed,
        CommandInvocationStatus_Failed,
        CommandInvocationStatus_InProgress,
        CommandInvocationStatus_Pending,
        CommandInvocationStatus_Success,
        CommandInvocationStatus_TimedOut
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CommandInvocationStatus = CommandInvocationStatus'
  { fromCommandInvocationStatus ::
      Core.Text
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

pattern CommandInvocationStatus_Cancelled :: CommandInvocationStatus
pattern CommandInvocationStatus_Cancelled = CommandInvocationStatus' "Cancelled"

pattern CommandInvocationStatus_Cancelling :: CommandInvocationStatus
pattern CommandInvocationStatus_Cancelling = CommandInvocationStatus' "Cancelling"

pattern CommandInvocationStatus_Delayed :: CommandInvocationStatus
pattern CommandInvocationStatus_Delayed = CommandInvocationStatus' "Delayed"

pattern CommandInvocationStatus_Failed :: CommandInvocationStatus
pattern CommandInvocationStatus_Failed = CommandInvocationStatus' "Failed"

pattern CommandInvocationStatus_InProgress :: CommandInvocationStatus
pattern CommandInvocationStatus_InProgress = CommandInvocationStatus' "InProgress"

pattern CommandInvocationStatus_Pending :: CommandInvocationStatus
pattern CommandInvocationStatus_Pending = CommandInvocationStatus' "Pending"

pattern CommandInvocationStatus_Success :: CommandInvocationStatus
pattern CommandInvocationStatus_Success = CommandInvocationStatus' "Success"

pattern CommandInvocationStatus_TimedOut :: CommandInvocationStatus
pattern CommandInvocationStatus_TimedOut = CommandInvocationStatus' "TimedOut"

{-# COMPLETE
  CommandInvocationStatus_Cancelled,
  CommandInvocationStatus_Cancelling,
  CommandInvocationStatus_Delayed,
  CommandInvocationStatus_Failed,
  CommandInvocationStatus_InProgress,
  CommandInvocationStatus_Pending,
  CommandInvocationStatus_Success,
  CommandInvocationStatus_TimedOut,
  CommandInvocationStatus'
  #-}
