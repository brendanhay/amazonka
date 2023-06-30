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
-- Module      : Amazonka.Backup.Types.BackupPlanTemplatesListMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.BackupPlanTemplatesListMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object specifying metadata associated with a backup plan template.
--
-- /See:/ 'newBackupPlanTemplatesListMember' smart constructor.
data BackupPlanTemplatesListMember = BackupPlanTemplatesListMember'
  { -- | Uniquely identifies a stored backup plan template.
    backupPlanTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The optional display name of a backup plan template.
    backupPlanTemplateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupPlanTemplatesListMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanTemplateId', 'backupPlanTemplatesListMember_backupPlanTemplateId' - Uniquely identifies a stored backup plan template.
--
-- 'backupPlanTemplateName', 'backupPlanTemplatesListMember_backupPlanTemplateName' - The optional display name of a backup plan template.
newBackupPlanTemplatesListMember ::
  BackupPlanTemplatesListMember
newBackupPlanTemplatesListMember =
  BackupPlanTemplatesListMember'
    { backupPlanTemplateId =
        Prelude.Nothing,
      backupPlanTemplateName = Prelude.Nothing
    }

-- | Uniquely identifies a stored backup plan template.
backupPlanTemplatesListMember_backupPlanTemplateId :: Lens.Lens' BackupPlanTemplatesListMember (Prelude.Maybe Prelude.Text)
backupPlanTemplatesListMember_backupPlanTemplateId = Lens.lens (\BackupPlanTemplatesListMember' {backupPlanTemplateId} -> backupPlanTemplateId) (\s@BackupPlanTemplatesListMember' {} a -> s {backupPlanTemplateId = a} :: BackupPlanTemplatesListMember)

-- | The optional display name of a backup plan template.
backupPlanTemplatesListMember_backupPlanTemplateName :: Lens.Lens' BackupPlanTemplatesListMember (Prelude.Maybe Prelude.Text)
backupPlanTemplatesListMember_backupPlanTemplateName = Lens.lens (\BackupPlanTemplatesListMember' {backupPlanTemplateName} -> backupPlanTemplateName) (\s@BackupPlanTemplatesListMember' {} a -> s {backupPlanTemplateName = a} :: BackupPlanTemplatesListMember)

instance Data.FromJSON BackupPlanTemplatesListMember where
  parseJSON =
    Data.withObject
      "BackupPlanTemplatesListMember"
      ( \x ->
          BackupPlanTemplatesListMember'
            Prelude.<$> (x Data..:? "BackupPlanTemplateId")
            Prelude.<*> (x Data..:? "BackupPlanTemplateName")
      )

instance
  Prelude.Hashable
    BackupPlanTemplatesListMember
  where
  hashWithSalt _salt BackupPlanTemplatesListMember' {..} =
    _salt
      `Prelude.hashWithSalt` backupPlanTemplateId
      `Prelude.hashWithSalt` backupPlanTemplateName

instance Prelude.NFData BackupPlanTemplatesListMember where
  rnf BackupPlanTemplatesListMember' {..} =
    Prelude.rnf backupPlanTemplateId
      `Prelude.seq` Prelude.rnf backupPlanTemplateName
