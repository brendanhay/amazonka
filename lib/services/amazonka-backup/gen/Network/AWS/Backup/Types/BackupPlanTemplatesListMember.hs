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
-- Module      : Network.AWS.Backup.Types.BackupPlanTemplatesListMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Backup.Types.BackupPlanTemplatesListMember where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object specifying metadata associated with a backup plan template.
--
-- /See:/ 'newBackupPlanTemplatesListMember' smart constructor.
data BackupPlanTemplatesListMember = BackupPlanTemplatesListMember'
  { -- | The optional display name of a backup plan template.
    backupPlanTemplateName :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a stored backup plan template.
    backupPlanTemplateId :: Prelude.Maybe Prelude.Text
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
-- 'backupPlanTemplateName', 'backupPlanTemplatesListMember_backupPlanTemplateName' - The optional display name of a backup plan template.
--
-- 'backupPlanTemplateId', 'backupPlanTemplatesListMember_backupPlanTemplateId' - Uniquely identifies a stored backup plan template.
newBackupPlanTemplatesListMember ::
  BackupPlanTemplatesListMember
newBackupPlanTemplatesListMember =
  BackupPlanTemplatesListMember'
    { backupPlanTemplateName =
        Prelude.Nothing,
      backupPlanTemplateId = Prelude.Nothing
    }

-- | The optional display name of a backup plan template.
backupPlanTemplatesListMember_backupPlanTemplateName :: Lens.Lens' BackupPlanTemplatesListMember (Prelude.Maybe Prelude.Text)
backupPlanTemplatesListMember_backupPlanTemplateName = Lens.lens (\BackupPlanTemplatesListMember' {backupPlanTemplateName} -> backupPlanTemplateName) (\s@BackupPlanTemplatesListMember' {} a -> s {backupPlanTemplateName = a} :: BackupPlanTemplatesListMember)

-- | Uniquely identifies a stored backup plan template.
backupPlanTemplatesListMember_backupPlanTemplateId :: Lens.Lens' BackupPlanTemplatesListMember (Prelude.Maybe Prelude.Text)
backupPlanTemplatesListMember_backupPlanTemplateId = Lens.lens (\BackupPlanTemplatesListMember' {backupPlanTemplateId} -> backupPlanTemplateId) (\s@BackupPlanTemplatesListMember' {} a -> s {backupPlanTemplateId = a} :: BackupPlanTemplatesListMember)

instance Core.FromJSON BackupPlanTemplatesListMember where
  parseJSON =
    Core.withObject
      "BackupPlanTemplatesListMember"
      ( \x ->
          BackupPlanTemplatesListMember'
            Prelude.<$> (x Core..:? "BackupPlanTemplateName")
            Prelude.<*> (x Core..:? "BackupPlanTemplateId")
      )

instance
  Prelude.Hashable
    BackupPlanTemplatesListMember

instance Prelude.NFData BackupPlanTemplatesListMember
