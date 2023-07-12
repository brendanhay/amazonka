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
-- Module      : Amazonka.IoT.Types.AuditMitigationActionsTaskStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditMitigationActionsTaskStatus
  ( AuditMitigationActionsTaskStatus
      ( ..,
        AuditMitigationActionsTaskStatus_CANCELED,
        AuditMitigationActionsTaskStatus_COMPLETED,
        AuditMitigationActionsTaskStatus_FAILED,
        AuditMitigationActionsTaskStatus_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuditMitigationActionsTaskStatus = AuditMitigationActionsTaskStatus'
  { fromAuditMitigationActionsTaskStatus ::
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

pattern AuditMitigationActionsTaskStatus_CANCELED :: AuditMitigationActionsTaskStatus
pattern AuditMitigationActionsTaskStatus_CANCELED = AuditMitigationActionsTaskStatus' "CANCELED"

pattern AuditMitigationActionsTaskStatus_COMPLETED :: AuditMitigationActionsTaskStatus
pattern AuditMitigationActionsTaskStatus_COMPLETED = AuditMitigationActionsTaskStatus' "COMPLETED"

pattern AuditMitigationActionsTaskStatus_FAILED :: AuditMitigationActionsTaskStatus
pattern AuditMitigationActionsTaskStatus_FAILED = AuditMitigationActionsTaskStatus' "FAILED"

pattern AuditMitigationActionsTaskStatus_IN_PROGRESS :: AuditMitigationActionsTaskStatus
pattern AuditMitigationActionsTaskStatus_IN_PROGRESS = AuditMitigationActionsTaskStatus' "IN_PROGRESS"

{-# COMPLETE
  AuditMitigationActionsTaskStatus_CANCELED,
  AuditMitigationActionsTaskStatus_COMPLETED,
  AuditMitigationActionsTaskStatus_FAILED,
  AuditMitigationActionsTaskStatus_IN_PROGRESS,
  AuditMitigationActionsTaskStatus'
  #-}
