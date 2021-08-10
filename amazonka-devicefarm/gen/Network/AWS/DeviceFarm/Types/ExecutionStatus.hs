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
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ExecutionStatus
  ( ExecutionStatus
      ( ..,
        ExecutionStatus_COMPLETED,
        ExecutionStatus_PENDING,
        ExecutionStatus_PENDING_CONCURRENCY,
        ExecutionStatus_PENDING_DEVICE,
        ExecutionStatus_PREPARING,
        ExecutionStatus_PROCESSING,
        ExecutionStatus_RUNNING,
        ExecutionStatus_SCHEDULING,
        ExecutionStatus_STOPPING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ExecutionStatus = ExecutionStatus'
  { fromExecutionStatus ::
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

pattern ExecutionStatus_COMPLETED :: ExecutionStatus
pattern ExecutionStatus_COMPLETED = ExecutionStatus' "COMPLETED"

pattern ExecutionStatus_PENDING :: ExecutionStatus
pattern ExecutionStatus_PENDING = ExecutionStatus' "PENDING"

pattern ExecutionStatus_PENDING_CONCURRENCY :: ExecutionStatus
pattern ExecutionStatus_PENDING_CONCURRENCY = ExecutionStatus' "PENDING_CONCURRENCY"

pattern ExecutionStatus_PENDING_DEVICE :: ExecutionStatus
pattern ExecutionStatus_PENDING_DEVICE = ExecutionStatus' "PENDING_DEVICE"

pattern ExecutionStatus_PREPARING :: ExecutionStatus
pattern ExecutionStatus_PREPARING = ExecutionStatus' "PREPARING"

pattern ExecutionStatus_PROCESSING :: ExecutionStatus
pattern ExecutionStatus_PROCESSING = ExecutionStatus' "PROCESSING"

pattern ExecutionStatus_RUNNING :: ExecutionStatus
pattern ExecutionStatus_RUNNING = ExecutionStatus' "RUNNING"

pattern ExecutionStatus_SCHEDULING :: ExecutionStatus
pattern ExecutionStatus_SCHEDULING = ExecutionStatus' "SCHEDULING"

pattern ExecutionStatus_STOPPING :: ExecutionStatus
pattern ExecutionStatus_STOPPING = ExecutionStatus' "STOPPING"

{-# COMPLETE
  ExecutionStatus_COMPLETED,
  ExecutionStatus_PENDING,
  ExecutionStatus_PENDING_CONCURRENCY,
  ExecutionStatus_PENDING_DEVICE,
  ExecutionStatus_PREPARING,
  ExecutionStatus_PROCESSING,
  ExecutionStatus_RUNNING,
  ExecutionStatus_SCHEDULING,
  ExecutionStatus_STOPPING,
  ExecutionStatus'
  #-}
