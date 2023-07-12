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
-- Module      : Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStepStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStepStatus
  ( RecoveryInstanceDataReplicationInitiationStepStatus
      ( ..,
        RecoveryInstanceDataReplicationInitiationStepStatus_FAILED,
        RecoveryInstanceDataReplicationInitiationStepStatus_IN_PROGRESS,
        RecoveryInstanceDataReplicationInitiationStepStatus_NOT_STARTED,
        RecoveryInstanceDataReplicationInitiationStepStatus_SKIPPED,
        RecoveryInstanceDataReplicationInitiationStepStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecoveryInstanceDataReplicationInitiationStepStatus = RecoveryInstanceDataReplicationInitiationStepStatus'
  { fromRecoveryInstanceDataReplicationInitiationStepStatus ::
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

pattern RecoveryInstanceDataReplicationInitiationStepStatus_FAILED :: RecoveryInstanceDataReplicationInitiationStepStatus
pattern RecoveryInstanceDataReplicationInitiationStepStatus_FAILED = RecoveryInstanceDataReplicationInitiationStepStatus' "FAILED"

pattern RecoveryInstanceDataReplicationInitiationStepStatus_IN_PROGRESS :: RecoveryInstanceDataReplicationInitiationStepStatus
pattern RecoveryInstanceDataReplicationInitiationStepStatus_IN_PROGRESS = RecoveryInstanceDataReplicationInitiationStepStatus' "IN_PROGRESS"

pattern RecoveryInstanceDataReplicationInitiationStepStatus_NOT_STARTED :: RecoveryInstanceDataReplicationInitiationStepStatus
pattern RecoveryInstanceDataReplicationInitiationStepStatus_NOT_STARTED = RecoveryInstanceDataReplicationInitiationStepStatus' "NOT_STARTED"

pattern RecoveryInstanceDataReplicationInitiationStepStatus_SKIPPED :: RecoveryInstanceDataReplicationInitiationStepStatus
pattern RecoveryInstanceDataReplicationInitiationStepStatus_SKIPPED = RecoveryInstanceDataReplicationInitiationStepStatus' "SKIPPED"

pattern RecoveryInstanceDataReplicationInitiationStepStatus_SUCCEEDED :: RecoveryInstanceDataReplicationInitiationStepStatus
pattern RecoveryInstanceDataReplicationInitiationStepStatus_SUCCEEDED = RecoveryInstanceDataReplicationInitiationStepStatus' "SUCCEEDED"

{-# COMPLETE
  RecoveryInstanceDataReplicationInitiationStepStatus_FAILED,
  RecoveryInstanceDataReplicationInitiationStepStatus_IN_PROGRESS,
  RecoveryInstanceDataReplicationInitiationStepStatus_NOT_STARTED,
  RecoveryInstanceDataReplicationInitiationStepStatus_SKIPPED,
  RecoveryInstanceDataReplicationInitiationStepStatus_SUCCEEDED,
  RecoveryInstanceDataReplicationInitiationStepStatus'
  #-}
