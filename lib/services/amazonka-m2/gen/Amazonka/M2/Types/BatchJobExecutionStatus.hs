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
-- Module      : Amazonka.M2.Types.BatchJobExecutionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.BatchJobExecutionStatus
  ( BatchJobExecutionStatus
      ( ..,
        BatchJobExecutionStatus_Cancelled,
        BatchJobExecutionStatus_Cancelling,
        BatchJobExecutionStatus_Dispatching,
        BatchJobExecutionStatus_Failed,
        BatchJobExecutionStatus_Holding,
        BatchJobExecutionStatus_Running,
        BatchJobExecutionStatus_Submitting,
        BatchJobExecutionStatus_Succeeded,
        BatchJobExecutionStatus_Succeeded_With_Warning
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BatchJobExecutionStatus = BatchJobExecutionStatus'
  { fromBatchJobExecutionStatus ::
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

pattern BatchJobExecutionStatus_Cancelled :: BatchJobExecutionStatus
pattern BatchJobExecutionStatus_Cancelled = BatchJobExecutionStatus' "Cancelled"

pattern BatchJobExecutionStatus_Cancelling :: BatchJobExecutionStatus
pattern BatchJobExecutionStatus_Cancelling = BatchJobExecutionStatus' "Cancelling"

pattern BatchJobExecutionStatus_Dispatching :: BatchJobExecutionStatus
pattern BatchJobExecutionStatus_Dispatching = BatchJobExecutionStatus' "Dispatching"

pattern BatchJobExecutionStatus_Failed :: BatchJobExecutionStatus
pattern BatchJobExecutionStatus_Failed = BatchJobExecutionStatus' "Failed"

pattern BatchJobExecutionStatus_Holding :: BatchJobExecutionStatus
pattern BatchJobExecutionStatus_Holding = BatchJobExecutionStatus' "Holding"

pattern BatchJobExecutionStatus_Running :: BatchJobExecutionStatus
pattern BatchJobExecutionStatus_Running = BatchJobExecutionStatus' "Running"

pattern BatchJobExecutionStatus_Submitting :: BatchJobExecutionStatus
pattern BatchJobExecutionStatus_Submitting = BatchJobExecutionStatus' "Submitting"

pattern BatchJobExecutionStatus_Succeeded :: BatchJobExecutionStatus
pattern BatchJobExecutionStatus_Succeeded = BatchJobExecutionStatus' "Succeeded"

pattern BatchJobExecutionStatus_Succeeded_With_Warning :: BatchJobExecutionStatus
pattern BatchJobExecutionStatus_Succeeded_With_Warning = BatchJobExecutionStatus' "Succeeded With Warning"

{-# COMPLETE
  BatchJobExecutionStatus_Cancelled,
  BatchJobExecutionStatus_Cancelling,
  BatchJobExecutionStatus_Dispatching,
  BatchJobExecutionStatus_Failed,
  BatchJobExecutionStatus_Holding,
  BatchJobExecutionStatus_Running,
  BatchJobExecutionStatus_Submitting,
  BatchJobExecutionStatus_Succeeded,
  BatchJobExecutionStatus_Succeeded_With_Warning,
  BatchJobExecutionStatus'
  #-}
