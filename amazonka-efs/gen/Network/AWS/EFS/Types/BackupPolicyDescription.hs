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
-- Module      : Network.AWS.EFS.Types.BackupPolicyDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.BackupPolicyDescription where

import Network.AWS.EFS.Types.BackupPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newBackupPolicyDescription' smart constructor.
data BackupPolicyDescription = BackupPolicyDescription'
  { -- | Describes the file system\'s backup policy, indicating whether automatic
    -- backups are turned on or off..
    backupPolicy :: Prelude.Maybe BackupPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BackupPolicyDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPolicy', 'backupPolicyDescription_backupPolicy' - Describes the file system\'s backup policy, indicating whether automatic
-- backups are turned on or off..
newBackupPolicyDescription ::
  BackupPolicyDescription
newBackupPolicyDescription =
  BackupPolicyDescription'
    { backupPolicy =
        Prelude.Nothing
    }

-- | Describes the file system\'s backup policy, indicating whether automatic
-- backups are turned on or off..
backupPolicyDescription_backupPolicy :: Lens.Lens' BackupPolicyDescription (Prelude.Maybe BackupPolicy)
backupPolicyDescription_backupPolicy = Lens.lens (\BackupPolicyDescription' {backupPolicy} -> backupPolicy) (\s@BackupPolicyDescription' {} a -> s {backupPolicy = a} :: BackupPolicyDescription)

instance Prelude.FromJSON BackupPolicyDescription where
  parseJSON =
    Prelude.withObject
      "BackupPolicyDescription"
      ( \x ->
          BackupPolicyDescription'
            Prelude.<$> (x Prelude..:? "BackupPolicy")
      )

instance Prelude.Hashable BackupPolicyDescription

instance Prelude.NFData BackupPolicyDescription
