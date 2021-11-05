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
-- Module      : Amazonka.Backup.Types.BackupJobState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.BackupJobState
  ( BackupJobState
      ( ..,
        BackupJobState_ABORTED,
        BackupJobState_ABORTING,
        BackupJobState_COMPLETED,
        BackupJobState_CREATED,
        BackupJobState_EXPIRED,
        BackupJobState_FAILED,
        BackupJobState_PENDING,
        BackupJobState_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype BackupJobState = BackupJobState'
  { fromBackupJobState ::
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

pattern BackupJobState_ABORTED :: BackupJobState
pattern BackupJobState_ABORTED = BackupJobState' "ABORTED"

pattern BackupJobState_ABORTING :: BackupJobState
pattern BackupJobState_ABORTING = BackupJobState' "ABORTING"

pattern BackupJobState_COMPLETED :: BackupJobState
pattern BackupJobState_COMPLETED = BackupJobState' "COMPLETED"

pattern BackupJobState_CREATED :: BackupJobState
pattern BackupJobState_CREATED = BackupJobState' "CREATED"

pattern BackupJobState_EXPIRED :: BackupJobState
pattern BackupJobState_EXPIRED = BackupJobState' "EXPIRED"

pattern BackupJobState_FAILED :: BackupJobState
pattern BackupJobState_FAILED = BackupJobState' "FAILED"

pattern BackupJobState_PENDING :: BackupJobState
pattern BackupJobState_PENDING = BackupJobState' "PENDING"

pattern BackupJobState_RUNNING :: BackupJobState
pattern BackupJobState_RUNNING = BackupJobState' "RUNNING"

{-# COMPLETE
  BackupJobState_ABORTED,
  BackupJobState_ABORTING,
  BackupJobState_COMPLETED,
  BackupJobState_CREATED,
  BackupJobState_EXPIRED,
  BackupJobState_FAILED,
  BackupJobState_PENDING,
  BackupJobState_RUNNING,
  BackupJobState'
  #-}
