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
-- Module      : Amazonka.DataSync.Types.TaskExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.TaskExecutionStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TaskExecutionStatus = TaskExecutionStatus'
  { fromTaskExecutionStatus ::
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
