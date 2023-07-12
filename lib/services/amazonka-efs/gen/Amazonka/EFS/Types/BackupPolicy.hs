{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EFS.Types.BackupPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.BackupPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types.BackupStatus
import qualified Amazonka.Prelude as Prelude

-- | The backup policy for the file system used to create automatic daily
-- backups. If status has a value of @ENABLED@, the file system is being
-- automatically backed up. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/awsbackup.html#automatic-backups Automatic backups>.
--
-- /See:/ 'newBackupPolicy' smart constructor.
data BackupPolicy = BackupPolicy'
  { -- | Describes the status of the file system\'s backup policy.
    --
    -- -   __@ENABLED@__ - EFS is automatically backing up the file system.
    --
    -- -   __@ENABLING@__ - EFS is turning on automatic backups for the file
    --     system.
    --
    -- -   __@DISABLED@__ - Automatic back ups are turned off for the file
    --     system.
    --
    -- -   __@DISABLING@__ - EFS is turning off automatic backups for the file
    --     system.
    status :: BackupStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'backupPolicy_status' - Describes the status of the file system\'s backup policy.
--
-- -   __@ENABLED@__ - EFS is automatically backing up the file system.
--
-- -   __@ENABLING@__ - EFS is turning on automatic backups for the file
--     system.
--
-- -   __@DISABLED@__ - Automatic back ups are turned off for the file
--     system.
--
-- -   __@DISABLING@__ - EFS is turning off automatic backups for the file
--     system.
newBackupPolicy ::
  -- | 'status'
  BackupStatus ->
  BackupPolicy
newBackupPolicy pStatus_ =
  BackupPolicy' {status = pStatus_}

-- | Describes the status of the file system\'s backup policy.
--
-- -   __@ENABLED@__ - EFS is automatically backing up the file system.
--
-- -   __@ENABLING@__ - EFS is turning on automatic backups for the file
--     system.
--
-- -   __@DISABLED@__ - Automatic back ups are turned off for the file
--     system.
--
-- -   __@DISABLING@__ - EFS is turning off automatic backups for the file
--     system.
backupPolicy_status :: Lens.Lens' BackupPolicy BackupStatus
backupPolicy_status = Lens.lens (\BackupPolicy' {status} -> status) (\s@BackupPolicy' {} a -> s {status = a} :: BackupPolicy)

instance Data.FromJSON BackupPolicy where
  parseJSON =
    Data.withObject
      "BackupPolicy"
      ( \x ->
          BackupPolicy' Prelude.<$> (x Data..: "Status")
      )

instance Prelude.Hashable BackupPolicy where
  hashWithSalt _salt BackupPolicy' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData BackupPolicy where
  rnf BackupPolicy' {..} = Prelude.rnf status

instance Data.ToJSON BackupPolicy where
  toJSON BackupPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Status" Data..= status)]
      )
