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
-- Module      : Amazonka.Backup.Types.RecoveryPointCreator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.RecoveryPointCreator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the backup plan and rule that Backup used to
-- initiate the recovery point backup.
--
-- /See:/ 'newRecoveryPointCreator' smart constructor.
data RecoveryPointCreator = RecoveryPointCreator'
  { -- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
    backupPlanArn :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text,
    -- | Version IDs are unique, randomly generated, Unicode, UTF-8 encoded
    -- strings that are at most 1,024 bytes long. They cannot be edited.
    backupPlanVersion :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a rule used to schedule the backup of a selection of
    -- resources.
    backupRuleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryPointCreator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanArn', 'recoveryPointCreator_backupPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
--
-- 'backupPlanId', 'recoveryPointCreator_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'backupPlanVersion', 'recoveryPointCreator_backupPlanVersion' - Version IDs are unique, randomly generated, Unicode, UTF-8 encoded
-- strings that are at most 1,024 bytes long. They cannot be edited.
--
-- 'backupRuleId', 'recoveryPointCreator_backupRuleId' - Uniquely identifies a rule used to schedule the backup of a selection of
-- resources.
newRecoveryPointCreator ::
  RecoveryPointCreator
newRecoveryPointCreator =
  RecoveryPointCreator'
    { backupPlanArn =
        Prelude.Nothing,
      backupPlanId = Prelude.Nothing,
      backupPlanVersion = Prelude.Nothing,
      backupRuleId = Prelude.Nothing
    }

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
recoveryPointCreator_backupPlanArn :: Lens.Lens' RecoveryPointCreator (Prelude.Maybe Prelude.Text)
recoveryPointCreator_backupPlanArn = Lens.lens (\RecoveryPointCreator' {backupPlanArn} -> backupPlanArn) (\s@RecoveryPointCreator' {} a -> s {backupPlanArn = a} :: RecoveryPointCreator)

-- | Uniquely identifies a backup plan.
recoveryPointCreator_backupPlanId :: Lens.Lens' RecoveryPointCreator (Prelude.Maybe Prelude.Text)
recoveryPointCreator_backupPlanId = Lens.lens (\RecoveryPointCreator' {backupPlanId} -> backupPlanId) (\s@RecoveryPointCreator' {} a -> s {backupPlanId = a} :: RecoveryPointCreator)

-- | Version IDs are unique, randomly generated, Unicode, UTF-8 encoded
-- strings that are at most 1,024 bytes long. They cannot be edited.
recoveryPointCreator_backupPlanVersion :: Lens.Lens' RecoveryPointCreator (Prelude.Maybe Prelude.Text)
recoveryPointCreator_backupPlanVersion = Lens.lens (\RecoveryPointCreator' {backupPlanVersion} -> backupPlanVersion) (\s@RecoveryPointCreator' {} a -> s {backupPlanVersion = a} :: RecoveryPointCreator)

-- | Uniquely identifies a rule used to schedule the backup of a selection of
-- resources.
recoveryPointCreator_backupRuleId :: Lens.Lens' RecoveryPointCreator (Prelude.Maybe Prelude.Text)
recoveryPointCreator_backupRuleId = Lens.lens (\RecoveryPointCreator' {backupRuleId} -> backupRuleId) (\s@RecoveryPointCreator' {} a -> s {backupRuleId = a} :: RecoveryPointCreator)

instance Data.FromJSON RecoveryPointCreator where
  parseJSON =
    Data.withObject
      "RecoveryPointCreator"
      ( \x ->
          RecoveryPointCreator'
            Prelude.<$> (x Data..:? "BackupPlanArn")
            Prelude.<*> (x Data..:? "BackupPlanId")
            Prelude.<*> (x Data..:? "BackupPlanVersion")
            Prelude.<*> (x Data..:? "BackupRuleId")
      )

instance Prelude.Hashable RecoveryPointCreator where
  hashWithSalt _salt RecoveryPointCreator' {..} =
    _salt `Prelude.hashWithSalt` backupPlanArn
      `Prelude.hashWithSalt` backupPlanId
      `Prelude.hashWithSalt` backupPlanVersion
      `Prelude.hashWithSalt` backupRuleId

instance Prelude.NFData RecoveryPointCreator where
  rnf RecoveryPointCreator' {..} =
    Prelude.rnf backupPlanArn
      `Prelude.seq` Prelude.rnf backupPlanId
      `Prelude.seq` Prelude.rnf backupPlanVersion
      `Prelude.seq` Prelude.rnf backupRuleId
