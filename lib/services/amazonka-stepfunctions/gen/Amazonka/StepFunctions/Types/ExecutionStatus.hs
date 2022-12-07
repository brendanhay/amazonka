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
-- Module      : Amazonka.StepFunctions.Types.ExecutionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ExecutionStatus
  ( ExecutionStatus
      ( ..,
        ExecutionStatus_ABORTED,
        ExecutionStatus_FAILED,
        ExecutionStatus_RUNNING,
        ExecutionStatus_SUCCEEDED,
        ExecutionStatus_TIMED_OUT
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

pattern ExecutionStatus_ABORTED :: ExecutionStatus
pattern ExecutionStatus_ABORTED = ExecutionStatus' "ABORTED"

pattern ExecutionStatus_FAILED :: ExecutionStatus
pattern ExecutionStatus_FAILED = ExecutionStatus' "FAILED"

pattern ExecutionStatus_RUNNING :: ExecutionStatus
pattern ExecutionStatus_RUNNING = ExecutionStatus' "RUNNING"

pattern ExecutionStatus_SUCCEEDED :: ExecutionStatus
pattern ExecutionStatus_SUCCEEDED = ExecutionStatus' "SUCCEEDED"

pattern ExecutionStatus_TIMED_OUT :: ExecutionStatus
pattern ExecutionStatus_TIMED_OUT = ExecutionStatus' "TIMED_OUT"

{-# COMPLETE
  ExecutionStatus_ABORTED,
  ExecutionStatus_FAILED,
  ExecutionStatus_RUNNING,
  ExecutionStatus_SUCCEEDED,
  ExecutionStatus_TIMED_OUT,
  ExecutionStatus'
  #-}
