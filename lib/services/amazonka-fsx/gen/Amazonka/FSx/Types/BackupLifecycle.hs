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
-- Module      : Amazonka.FSx.Types.BackupLifecycle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.BackupLifecycle
  ( BackupLifecycle
      ( ..,
        BackupLifecycle_AVAILABLE,
        BackupLifecycle_COPYING,
        BackupLifecycle_CREATING,
        BackupLifecycle_DELETED,
        BackupLifecycle_FAILED,
        BackupLifecycle_PENDING,
        BackupLifecycle_TRANSFERRING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The lifecycle status of the backup.
--
-- -   @AVAILABLE@ - The backup is fully available.
--
-- -   @PENDING@ - For user-initiated backups on Lustre file systems only;
--     Amazon FSx hasn\'t started creating the backup.
--
-- -   @CREATING@ - Amazon FSx is creating the new user-initiated backup.
--
-- -   @TRANSFERRING@ - For user-initiated backups on Lustre file systems
--     only; Amazon FSx is backing up the file system.
--
-- -   @COPYING@ - Amazon FSx is copying the backup.
--
-- -   @DELETED@ - Amazon FSx deleted the backup and it\'s no longer
--     available.
--
-- -   @FAILED@ - Amazon FSx couldn\'t finish the backup.
newtype BackupLifecycle = BackupLifecycle'
  { fromBackupLifecycle ::
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

pattern BackupLifecycle_AVAILABLE :: BackupLifecycle
pattern BackupLifecycle_AVAILABLE = BackupLifecycle' "AVAILABLE"

pattern BackupLifecycle_COPYING :: BackupLifecycle
pattern BackupLifecycle_COPYING = BackupLifecycle' "COPYING"

pattern BackupLifecycle_CREATING :: BackupLifecycle
pattern BackupLifecycle_CREATING = BackupLifecycle' "CREATING"

pattern BackupLifecycle_DELETED :: BackupLifecycle
pattern BackupLifecycle_DELETED = BackupLifecycle' "DELETED"

pattern BackupLifecycle_FAILED :: BackupLifecycle
pattern BackupLifecycle_FAILED = BackupLifecycle' "FAILED"

pattern BackupLifecycle_PENDING :: BackupLifecycle
pattern BackupLifecycle_PENDING = BackupLifecycle' "PENDING"

pattern BackupLifecycle_TRANSFERRING :: BackupLifecycle
pattern BackupLifecycle_TRANSFERRING = BackupLifecycle' "TRANSFERRING"

{-# COMPLETE
  BackupLifecycle_AVAILABLE,
  BackupLifecycle_COPYING,
  BackupLifecycle_CREATING,
  BackupLifecycle_DELETED,
  BackupLifecycle_FAILED,
  BackupLifecycle_PENDING,
  BackupLifecycle_TRANSFERRING,
  BackupLifecycle'
  #-}
