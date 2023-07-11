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
-- Module      : Amazonka.Backup.Types.BackupVaultEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.BackupVaultEvent
  ( BackupVaultEvent
      ( ..,
        BackupVaultEvent_BACKUP_JOB_COMPLETED,
        BackupVaultEvent_BACKUP_JOB_EXPIRED,
        BackupVaultEvent_BACKUP_JOB_FAILED,
        BackupVaultEvent_BACKUP_JOB_STARTED,
        BackupVaultEvent_BACKUP_JOB_SUCCESSFUL,
        BackupVaultEvent_BACKUP_PLAN_CREATED,
        BackupVaultEvent_BACKUP_PLAN_MODIFIED,
        BackupVaultEvent_COPY_JOB_FAILED,
        BackupVaultEvent_COPY_JOB_STARTED,
        BackupVaultEvent_COPY_JOB_SUCCESSFUL,
        BackupVaultEvent_RECOVERY_POINT_MODIFIED,
        BackupVaultEvent_RESTORE_JOB_COMPLETED,
        BackupVaultEvent_RESTORE_JOB_FAILED,
        BackupVaultEvent_RESTORE_JOB_STARTED,
        BackupVaultEvent_RESTORE_JOB_SUCCESSFUL,
        BackupVaultEvent_S3_BACKUP_OBJECT_FAILED,
        BackupVaultEvent_S3_RESTORE_OBJECT_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BackupVaultEvent = BackupVaultEvent'
  { fromBackupVaultEvent ::
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

pattern BackupVaultEvent_BACKUP_JOB_COMPLETED :: BackupVaultEvent
pattern BackupVaultEvent_BACKUP_JOB_COMPLETED = BackupVaultEvent' "BACKUP_JOB_COMPLETED"

pattern BackupVaultEvent_BACKUP_JOB_EXPIRED :: BackupVaultEvent
pattern BackupVaultEvent_BACKUP_JOB_EXPIRED = BackupVaultEvent' "BACKUP_JOB_EXPIRED"

pattern BackupVaultEvent_BACKUP_JOB_FAILED :: BackupVaultEvent
pattern BackupVaultEvent_BACKUP_JOB_FAILED = BackupVaultEvent' "BACKUP_JOB_FAILED"

pattern BackupVaultEvent_BACKUP_JOB_STARTED :: BackupVaultEvent
pattern BackupVaultEvent_BACKUP_JOB_STARTED = BackupVaultEvent' "BACKUP_JOB_STARTED"

pattern BackupVaultEvent_BACKUP_JOB_SUCCESSFUL :: BackupVaultEvent
pattern BackupVaultEvent_BACKUP_JOB_SUCCESSFUL = BackupVaultEvent' "BACKUP_JOB_SUCCESSFUL"

pattern BackupVaultEvent_BACKUP_PLAN_CREATED :: BackupVaultEvent
pattern BackupVaultEvent_BACKUP_PLAN_CREATED = BackupVaultEvent' "BACKUP_PLAN_CREATED"

pattern BackupVaultEvent_BACKUP_PLAN_MODIFIED :: BackupVaultEvent
pattern BackupVaultEvent_BACKUP_PLAN_MODIFIED = BackupVaultEvent' "BACKUP_PLAN_MODIFIED"

pattern BackupVaultEvent_COPY_JOB_FAILED :: BackupVaultEvent
pattern BackupVaultEvent_COPY_JOB_FAILED = BackupVaultEvent' "COPY_JOB_FAILED"

pattern BackupVaultEvent_COPY_JOB_STARTED :: BackupVaultEvent
pattern BackupVaultEvent_COPY_JOB_STARTED = BackupVaultEvent' "COPY_JOB_STARTED"

pattern BackupVaultEvent_COPY_JOB_SUCCESSFUL :: BackupVaultEvent
pattern BackupVaultEvent_COPY_JOB_SUCCESSFUL = BackupVaultEvent' "COPY_JOB_SUCCESSFUL"

pattern BackupVaultEvent_RECOVERY_POINT_MODIFIED :: BackupVaultEvent
pattern BackupVaultEvent_RECOVERY_POINT_MODIFIED = BackupVaultEvent' "RECOVERY_POINT_MODIFIED"

pattern BackupVaultEvent_RESTORE_JOB_COMPLETED :: BackupVaultEvent
pattern BackupVaultEvent_RESTORE_JOB_COMPLETED = BackupVaultEvent' "RESTORE_JOB_COMPLETED"

pattern BackupVaultEvent_RESTORE_JOB_FAILED :: BackupVaultEvent
pattern BackupVaultEvent_RESTORE_JOB_FAILED = BackupVaultEvent' "RESTORE_JOB_FAILED"

pattern BackupVaultEvent_RESTORE_JOB_STARTED :: BackupVaultEvent
pattern BackupVaultEvent_RESTORE_JOB_STARTED = BackupVaultEvent' "RESTORE_JOB_STARTED"

pattern BackupVaultEvent_RESTORE_JOB_SUCCESSFUL :: BackupVaultEvent
pattern BackupVaultEvent_RESTORE_JOB_SUCCESSFUL = BackupVaultEvent' "RESTORE_JOB_SUCCESSFUL"

pattern BackupVaultEvent_S3_BACKUP_OBJECT_FAILED :: BackupVaultEvent
pattern BackupVaultEvent_S3_BACKUP_OBJECT_FAILED = BackupVaultEvent' "S3_BACKUP_OBJECT_FAILED"

pattern BackupVaultEvent_S3_RESTORE_OBJECT_FAILED :: BackupVaultEvent
pattern BackupVaultEvent_S3_RESTORE_OBJECT_FAILED = BackupVaultEvent' "S3_RESTORE_OBJECT_FAILED"

{-# COMPLETE
  BackupVaultEvent_BACKUP_JOB_COMPLETED,
  BackupVaultEvent_BACKUP_JOB_EXPIRED,
  BackupVaultEvent_BACKUP_JOB_FAILED,
  BackupVaultEvent_BACKUP_JOB_STARTED,
  BackupVaultEvent_BACKUP_JOB_SUCCESSFUL,
  BackupVaultEvent_BACKUP_PLAN_CREATED,
  BackupVaultEvent_BACKUP_PLAN_MODIFIED,
  BackupVaultEvent_COPY_JOB_FAILED,
  BackupVaultEvent_COPY_JOB_STARTED,
  BackupVaultEvent_COPY_JOB_SUCCESSFUL,
  BackupVaultEvent_RECOVERY_POINT_MODIFIED,
  BackupVaultEvent_RESTORE_JOB_COMPLETED,
  BackupVaultEvent_RESTORE_JOB_FAILED,
  BackupVaultEvent_RESTORE_JOB_STARTED,
  BackupVaultEvent_RESTORE_JOB_SUCCESSFUL,
  BackupVaultEvent_S3_BACKUP_OBJECT_FAILED,
  BackupVaultEvent_S3_RESTORE_OBJECT_FAILED,
  BackupVaultEvent'
  #-}
