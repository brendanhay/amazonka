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
-- Module      : Amazonka.EFS.Types.BackupPolicyDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.BackupPolicyDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types.BackupPolicy
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newBackupPolicyDescription' smart constructor.
data BackupPolicyDescription = BackupPolicyDescription'
  { -- | Describes the file system\'s backup policy, indicating whether automatic
    -- backups are turned on or off.
    backupPolicy :: Prelude.Maybe BackupPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupPolicyDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPolicy', 'backupPolicyDescription_backupPolicy' - Describes the file system\'s backup policy, indicating whether automatic
-- backups are turned on or off.
newBackupPolicyDescription ::
  BackupPolicyDescription
newBackupPolicyDescription =
  BackupPolicyDescription'
    { backupPolicy =
        Prelude.Nothing
    }

-- | Describes the file system\'s backup policy, indicating whether automatic
-- backups are turned on or off.
backupPolicyDescription_backupPolicy :: Lens.Lens' BackupPolicyDescription (Prelude.Maybe BackupPolicy)
backupPolicyDescription_backupPolicy = Lens.lens (\BackupPolicyDescription' {backupPolicy} -> backupPolicy) (\s@BackupPolicyDescription' {} a -> s {backupPolicy = a} :: BackupPolicyDescription)

instance Data.FromJSON BackupPolicyDescription where
  parseJSON =
    Data.withObject
      "BackupPolicyDescription"
      ( \x ->
          BackupPolicyDescription'
            Prelude.<$> (x Data..:? "BackupPolicy")
      )

instance Prelude.Hashable BackupPolicyDescription where
  hashWithSalt _salt BackupPolicyDescription' {..} =
    _salt `Prelude.hashWithSalt` backupPolicy

instance Prelude.NFData BackupPolicyDescription where
  rnf BackupPolicyDescription' {..} =
    Prelude.rnf backupPolicy
