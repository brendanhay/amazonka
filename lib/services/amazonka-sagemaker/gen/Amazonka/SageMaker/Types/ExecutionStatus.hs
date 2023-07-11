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
-- Module      : Amazonka.SageMaker.Types.ExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ExecutionStatus
  ( ExecutionStatus
      ( ..,
        ExecutionStatus_Completed,
        ExecutionStatus_CompletedWithViolations,
        ExecutionStatus_Failed,
        ExecutionStatus_InProgress,
        ExecutionStatus_Pending,
        ExecutionStatus_Stopped,
        ExecutionStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExecutionStatus = ExecutionStatus'
  { fromExecutionStatus ::
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

pattern ExecutionStatus_Completed :: ExecutionStatus
pattern ExecutionStatus_Completed = ExecutionStatus' "Completed"

pattern ExecutionStatus_CompletedWithViolations :: ExecutionStatus
pattern ExecutionStatus_CompletedWithViolations = ExecutionStatus' "CompletedWithViolations"

pattern ExecutionStatus_Failed :: ExecutionStatus
pattern ExecutionStatus_Failed = ExecutionStatus' "Failed"

pattern ExecutionStatus_InProgress :: ExecutionStatus
pattern ExecutionStatus_InProgress = ExecutionStatus' "InProgress"

pattern ExecutionStatus_Pending :: ExecutionStatus
pattern ExecutionStatus_Pending = ExecutionStatus' "Pending"

pattern ExecutionStatus_Stopped :: ExecutionStatus
pattern ExecutionStatus_Stopped = ExecutionStatus' "Stopped"

pattern ExecutionStatus_Stopping :: ExecutionStatus
pattern ExecutionStatus_Stopping = ExecutionStatus' "Stopping"

{-# COMPLETE
  ExecutionStatus_Completed,
  ExecutionStatus_CompletedWithViolations,
  ExecutionStatus_Failed,
  ExecutionStatus_InProgress,
  ExecutionStatus_Pending,
  ExecutionStatus_Stopped,
  ExecutionStatus_Stopping,
  ExecutionStatus'
  #-}
