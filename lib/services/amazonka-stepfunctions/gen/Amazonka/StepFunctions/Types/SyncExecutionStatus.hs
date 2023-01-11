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
-- Module      : Amazonka.StepFunctions.Types.SyncExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.SyncExecutionStatus
  ( SyncExecutionStatus
      ( ..,
        SyncExecutionStatus_FAILED,
        SyncExecutionStatus_SUCCEEDED,
        SyncExecutionStatus_TIMED_OUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SyncExecutionStatus = SyncExecutionStatus'
  { fromSyncExecutionStatus ::
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
