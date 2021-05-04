{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EFS.Types.BackupPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.BackupPolicy where

import Network.AWS.EFS.Types.BackupStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The backup policy for the file system, showing the curent status. If
-- @ENABLED@, the file system is being backed up.
--
-- /See:/ 'newBackupPolicy' smart constructor.
data BackupPolicy = BackupPolicy'
  { -- | Describes the status of the file system\'s backup policy.
    --
    -- -   /@ENABLED@ - EFS is automatically backing up the file system./
    --
    -- -   /@ENABLING@ - EFS is turning on automatic backups for the file
    --     system./
    --
    -- -   /@DISABLED@ - automatic back ups are turned off for the file
    --     system./
    --
    -- -   /@DISABLED@ - EFS is turning off automatic backups for the file
    --     system./
    status :: BackupStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- -   /@ENABLED@ - EFS is automatically backing up the file system./
--
-- -   /@ENABLING@ - EFS is turning on automatic backups for the file
--     system./
--
-- -   /@DISABLED@ - automatic back ups are turned off for the file
--     system./
--
-- -   /@DISABLED@ - EFS is turning off automatic backups for the file
--     system./
newBackupPolicy ::
  -- | 'status'
  BackupStatus ->
  BackupPolicy
newBackupPolicy pStatus_ =
  BackupPolicy' {status = pStatus_}

-- | Describes the status of the file system\'s backup policy.
--
-- -   /@ENABLED@ - EFS is automatically backing up the file system./
--
-- -   /@ENABLING@ - EFS is turning on automatic backups for the file
--     system./
--
-- -   /@DISABLED@ - automatic back ups are turned off for the file
--     system./
--
-- -   /@DISABLED@ - EFS is turning off automatic backups for the file
--     system./
backupPolicy_status :: Lens.Lens' BackupPolicy BackupStatus
backupPolicy_status = Lens.lens (\BackupPolicy' {status} -> status) (\s@BackupPolicy' {} a -> s {status = a} :: BackupPolicy)

instance Prelude.FromJSON BackupPolicy where
  parseJSON =
    Prelude.withObject
      "BackupPolicy"
      ( \x ->
          BackupPolicy' Prelude.<$> (x Prelude..: "Status")
      )

instance Prelude.Hashable BackupPolicy

instance Prelude.NFData BackupPolicy

instance Prelude.ToJSON BackupPolicy where
  toJSON BackupPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Status" Prelude..= status)]
      )
