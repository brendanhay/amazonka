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
-- Module      : Amazonka.IoT.Types.AuditMitigationActionsExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditMitigationActionsExecutionStatus
  ( AuditMitigationActionsExecutionStatus
      ( ..,
        AuditMitigationActionsExecutionStatus_CANCELED,
        AuditMitigationActionsExecutionStatus_COMPLETED,
        AuditMitigationActionsExecutionStatus_FAILED,
        AuditMitigationActionsExecutionStatus_IN_PROGRESS,
        AuditMitigationActionsExecutionStatus_PENDING,
        AuditMitigationActionsExecutionStatus_SKIPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AuditMitigationActionsExecutionStatus = AuditMitigationActionsExecutionStatus'
  { fromAuditMitigationActionsExecutionStatus ::
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

pattern AuditMitigationActionsExecutionStatus_CANCELED :: AuditMitigationActionsExecutionStatus
pattern AuditMitigationActionsExecutionStatus_CANCELED = AuditMitigationActionsExecutionStatus' "CANCELED"

pattern AuditMitigationActionsExecutionStatus_COMPLETED :: AuditMitigationActionsExecutionStatus
pattern AuditMitigationActionsExecutionStatus_COMPLETED = AuditMitigationActionsExecutionStatus' "COMPLETED"

pattern AuditMitigationActionsExecutionStatus_FAILED :: AuditMitigationActionsExecutionStatus
pattern AuditMitigationActionsExecutionStatus_FAILED = AuditMitigationActionsExecutionStatus' "FAILED"

pattern AuditMitigationActionsExecutionStatus_IN_PROGRESS :: AuditMitigationActionsExecutionStatus
pattern AuditMitigationActionsExecutionStatus_IN_PROGRESS = AuditMitigationActionsExecutionStatus' "IN_PROGRESS"

pattern AuditMitigationActionsExecutionStatus_PENDING :: AuditMitigationActionsExecutionStatus
pattern AuditMitigationActionsExecutionStatus_PENDING = AuditMitigationActionsExecutionStatus' "PENDING"

pattern AuditMitigationActionsExecutionStatus_SKIPPED :: AuditMitigationActionsExecutionStatus
pattern AuditMitigationActionsExecutionStatus_SKIPPED = AuditMitigationActionsExecutionStatus' "SKIPPED"

{-# COMPLETE
  AuditMitigationActionsExecutionStatus_CANCELED,
  AuditMitigationActionsExecutionStatus_COMPLETED,
  AuditMitigationActionsExecutionStatus_FAILED,
  AuditMitigationActionsExecutionStatus_IN_PROGRESS,
  AuditMitigationActionsExecutionStatus_PENDING,
  AuditMitigationActionsExecutionStatus_SKIPPED,
  AuditMitigationActionsExecutionStatus'
  #-}
