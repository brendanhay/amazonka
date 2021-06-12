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
-- Module      : Network.AWS.StepFunctions.Types.SyncExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.SyncExecutionStatus
  ( SyncExecutionStatus
      ( ..,
        SyncExecutionStatus_FAILED,
        SyncExecutionStatus_SUCCEEDED,
        SyncExecutionStatus_TIMED_OUT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SyncExecutionStatus = SyncExecutionStatus'
  { fromSyncExecutionStatus ::
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

pattern SyncExecutionStatus_FAILED :: SyncExecutionStatus
pattern SyncExecutionStatus_FAILED = SyncExecutionStatus' "FAILED"

pattern SyncExecutionStatus_SUCCEEDED :: SyncExecutionStatus
pattern SyncExecutionStatus_SUCCEEDED = SyncExecutionStatus' "SUCCEEDED"

pattern SyncExecutionStatus_TIMED_OUT :: SyncExecutionStatus
pattern SyncExecutionStatus_TIMED_OUT = SyncExecutionStatus' "TIMED_OUT"

{-# COMPLETE
  SyncExecutionStatus_FAILED,
  SyncExecutionStatus_SUCCEEDED,
  SyncExecutionStatus_TIMED_OUT,
  SyncExecutionStatus'
  #-}
