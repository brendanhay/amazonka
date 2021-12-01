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
-- Module      : Amazonka.Backup.Types.BackupSelection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.BackupSelection where

import Amazonka.Backup.Types.Condition
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Used to specify a set of resources to a backup plan.
--
-- /See:/ 'newBackupSelection' smart constructor.
data BackupSelection = BackupSelection'
  { -- | An array of strings that contain Amazon Resource Names (ARNs) of
    -- resources to assign to a backup plan.
    resources :: Prelude.Maybe [Prelude.Text],
    -- | An array of conditions used to specify a set of resources to assign to a
    -- backup plan; for example,
    -- @\"StringEquals\": {\"ec2:ResourceTag\/Department\": \"accounting\"@.
    -- Assigns the backup plan to every resource with at least one matching
    -- tag.
    listOfTags :: Prelude.Maybe [Condition],
    -- | The display name of a resource selection document.
    selectionName :: Prelude.Text,
    -- | The ARN of the IAM role that Backup uses to authenticate when backing up
    -- the target resource; for example,
    -- @arn:aws:iam::123456789012:role\/S3Access@.
    iamRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupSelection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resources', 'backupSelection_resources' - An array of strings that contain Amazon Resource Names (ARNs) of
-- resources to assign to a backup plan.
--
-- 'listOfTags', 'backupSelection_listOfTags' - An array of conditions used to specify a set of resources to assign to a
-- backup plan; for example,
-- @\"StringEquals\": {\"ec2:ResourceTag\/Department\": \"accounting\"@.
-- Assigns the backup plan to every resource with at least one matching
-- tag.
--
-- 'selectionName', 'backupSelection_selectionName' - The display name of a resource selection document.
--
-- 'iamRoleArn', 'backupSelection_iamRoleArn' - The ARN of the IAM role that Backup uses to authenticate when backing up
-- the target resource; for example,
-- @arn:aws:iam::123456789012:role\/S3Access@.
newBackupSelection ::
  -- | 'selectionName'
  Prelude.Text ->
  -- | 'iamRoleArn'
  Prelude.Text ->
  BackupSelection
newBackupSelection pSelectionName_ pIamRoleArn_ =
  BackupSelection'
    { resources = Prelude.Nothing,
      listOfTags = Prelude.Nothing,
      selectionName = pSelectionName_,
      iamRoleArn = pIamRoleArn_
    }

-- | An array of strings that contain Amazon Resource Names (ARNs) of
-- resources to assign to a backup plan.
backupSelection_resources :: Lens.Lens' BackupSelection (Prelude.Maybe [Prelude.Text])
backupSelection_resources = Lens.lens (\BackupSelection' {resources} -> resources) (\s@BackupSelection' {} a -> s {resources = a} :: BackupSelection) Prelude.. Lens.mapping Lens.coerced

-- | An array of conditions used to specify a set of resources to assign to a
-- backup plan; for example,
-- @\"StringEquals\": {\"ec2:ResourceTag\/Department\": \"accounting\"@.
-- Assigns the backup plan to every resource with at least one matching
-- tag.
backupSelection_listOfTags :: Lens.Lens' BackupSelection (Prelude.Maybe [Condition])
backupSelection_listOfTags = Lens.lens (\BackupSelection' {listOfTags} -> listOfTags) (\s@BackupSelection' {} a -> s {listOfTags = a} :: BackupSelection) Prelude.. Lens.mapping Lens.coerced

-- | The display name of a resource selection document.
backupSelection_selectionName :: Lens.Lens' BackupSelection Prelude.Text
backupSelection_selectionName = Lens.lens (\BackupSelection' {selectionName} -> selectionName) (\s@BackupSelection' {} a -> s {selectionName = a} :: BackupSelection)

-- | The ARN of the IAM role that Backup uses to authenticate when backing up
-- the target resource; for example,
-- @arn:aws:iam::123456789012:role\/S3Access@.
backupSelection_iamRoleArn :: Lens.Lens' BackupSelection Prelude.Text
backupSelection_iamRoleArn = Lens.lens (\BackupSelection' {iamRoleArn} -> iamRoleArn) (\s@BackupSelection' {} a -> s {iamRoleArn = a} :: BackupSelection)

instance Core.FromJSON BackupSelection where
  parseJSON =
    Core.withObject
      "BackupSelection"
      ( \x ->
          BackupSelection'
            Prelude.<$> (x Core..:? "Resources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ListOfTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "SelectionName")
            Prelude.<*> (x Core..: "IamRoleArn")
      )

instance Prelude.Hashable BackupSelection where
  hashWithSalt salt' BackupSelection' {..} =
    salt' `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` selectionName
      `Prelude.hashWithSalt` listOfTags
      `Prelude.hashWithSalt` resources

instance Prelude.NFData BackupSelection where
  rnf BackupSelection' {..} =
    Prelude.rnf resources
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf selectionName
      `Prelude.seq` Prelude.rnf listOfTags

instance Core.ToJSON BackupSelection where
  toJSON BackupSelection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Resources" Core..=) Prelude.<$> resources,
            ("ListOfTags" Core..=) Prelude.<$> listOfTags,
            Prelude.Just ("SelectionName" Core..= selectionName),
            Prelude.Just ("IamRoleArn" Core..= iamRoleArn)
          ]
      )
