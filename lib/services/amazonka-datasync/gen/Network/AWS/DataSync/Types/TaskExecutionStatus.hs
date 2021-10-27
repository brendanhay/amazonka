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
-- Module      : Network.AWS.DataSync.Types.TaskExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataSync.Types.TaskExecutionStatus
  ( TaskExecutionStatus
      ( ..,
        TaskExecutionStatus_ERROR,
        TaskExecutionStatus_LAUNCHING,
        TaskExecutionStatus_PREPARING,
        TaskExecutionStatus_QUEUED,
        TaskExecutionStatus_SUCCESS,
        TaskExecutionStatus_TRANSFERRING,
        TaskExecutionStatus_VERIFYING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype TaskExecutionStatus = TaskExecutionStatus'
  { fromTaskExecutionStatus ::
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

pattern TaskExecutionStatus_ERROR :: TaskExecutionStatus
pattern TaskExecutionStatus_ERROR = TaskExecutionStatus' "ERROR"

pattern TaskExecutionStatus_LAUNCHING :: TaskExecutionStatus
pattern TaskExecutionStatus_LAUNCHING = TaskExecutionStatus' "LAUNCHING"

pattern TaskExecutionStatus_PREPARING :: TaskExecutionStatus
pattern TaskExecutionStatus_PREPARING = TaskExecutionStatus' "PREPARING"

pattern TaskExecutionStatus_QUEUED :: TaskExecutionStatus
pattern TaskExecutionStatus_QUEUED = TaskExecutionStatus' "QUEUED"

pattern TaskExecutionStatus_SUCCESS :: TaskExecutionStatus
pattern TaskExecutionStatus_SUCCESS = TaskExecutionStatus' "SUCCESS"

pattern TaskExecutionStatus_TRANSFERRING :: TaskExecutionStatus
pattern TaskExecutionStatus_TRANSFERRING = TaskExecutionStatus' "TRANSFERRING"

pattern TaskExecutionStatus_VERIFYING :: TaskExecutionStatus
pattern TaskExecutionStatus_VERIFYING = TaskExecutionStatus' "VERIFYING"

{-# COMPLETE
  TaskExecutionStatus_ERROR,
  TaskExecutionStatus_LAUNCHING,
  TaskExecutionStatus_PREPARING,
  TaskExecutionStatus_QUEUED,
  TaskExecutionStatus_SUCCESS,
  TaskExecutionStatus_TRANSFERRING,
  TaskExecutionStatus_VERIFYING,
  TaskExecutionStatus'
  #-}
