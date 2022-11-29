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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.BackupSelection where

import Amazonka.Backup.Types.Condition
import Amazonka.Backup.Types.Conditions
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Used to specify a set of resources to a backup plan.
--
-- Specifying your desired @Conditions@, @ListOfTags@, @NotResources@,
-- and\/or @Resources@ is recommended. If none of these are specified,
-- Backup will attempt to select all supported and opted-in storage
-- resources, which could have unintended cost implications.
--
-- /See:/ 'newBackupSelection' smart constructor.
data BackupSelection = BackupSelection'
  { -- | A list of conditions that you define to assign resources to your backup
    -- plans using tags. For example,
    -- @\"StringEquals\": { \"ConditionKey\": \"aws:ResourceTag\/CreatedByCryo\", \"ConditionValue\": \"true\" },@.
    -- Condition operators are case sensitive.
    --
    -- @Conditions@ differs from @ListOfTags@ as follows:
    --
    -- -   When you specify more than one condition, you only assign the
    --     resources that match ALL conditions (using AND logic).
    --
    -- -   @Conditions@ supports @StringEquals@, @StringLike@,
    --     @StringNotEquals@, and @StringNotLike@. @ListOfTags@ only supports
    --     @StringEquals@.
    conditions :: Prelude.Maybe Conditions,
    -- | A list of Amazon Resource Names (ARNs) to assign to a backup plan. The
    -- maximum number of ARNs is 500 without wildcards, or 30 ARNs with
    -- wildcards.
    --
    -- If you need to assign many resources to a backup plan, consider a
    -- different resource selection strategy, such as assigning all resources
    -- of a resource type or refining your resource selection using tags.
    resources :: Prelude.Maybe [Prelude.Text],
    -- | A list of conditions that you define to assign resources to your backup
    -- plans using tags. For example,
    -- @\"StringEquals\": { \"ConditionKey\": \"aws:ResourceTag\/CreatedByCryo\", \"ConditionValue\": \"true\" },@.
    -- Condition operators are case sensitive.
    --
    -- @ListOfTags@ differs from @Conditions@ as follows:
    --
    -- -   When you specify more than one condition, you assign all resources
    --     that match AT LEAST ONE condition (using OR logic).
    --
    -- -   @ListOfTags@ only supports @StringEquals@. @Conditions@ supports
    --     @StringEquals@, @StringLike@, @StringNotEquals@, and
    --     @StringNotLike@.
    listOfTags :: Prelude.Maybe [Condition],
    -- | A list of Amazon Resource Names (ARNs) to exclude from a backup plan.
    -- The maximum number of ARNs is 500 without wildcards, or 30 ARNs with
    -- wildcards.
    --
    -- If you need to exclude many resources from a backup plan, consider a
    -- different resource selection strategy, such as assigning only one or a
    -- few resource types or refining your resource selection using tags.
    notResources :: Prelude.Maybe [Prelude.Text],
    -- | The display name of a resource selection document. Must contain 1 to 50
    -- alphanumeric or \'-_.\' characters.
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
-- 'conditions', 'backupSelection_conditions' - A list of conditions that you define to assign resources to your backup
-- plans using tags. For example,
-- @\"StringEquals\": { \"ConditionKey\": \"aws:ResourceTag\/CreatedByCryo\", \"ConditionValue\": \"true\" },@.
-- Condition operators are case sensitive.
--
-- @Conditions@ differs from @ListOfTags@ as follows:
--
-- -   When you specify more than one condition, you only assign the
--     resources that match ALL conditions (using AND logic).
--
-- -   @Conditions@ supports @StringEquals@, @StringLike@,
--     @StringNotEquals@, and @StringNotLike@. @ListOfTags@ only supports
--     @StringEquals@.
--
-- 'resources', 'backupSelection_resources' - A list of Amazon Resource Names (ARNs) to assign to a backup plan. The
-- maximum number of ARNs is 500 without wildcards, or 30 ARNs with
-- wildcards.
--
-- If you need to assign many resources to a backup plan, consider a
-- different resource selection strategy, such as assigning all resources
-- of a resource type or refining your resource selection using tags.
--
-- 'listOfTags', 'backupSelection_listOfTags' - A list of conditions that you define to assign resources to your backup
-- plans using tags. For example,
-- @\"StringEquals\": { \"ConditionKey\": \"aws:ResourceTag\/CreatedByCryo\", \"ConditionValue\": \"true\" },@.
-- Condition operators are case sensitive.
--
-- @ListOfTags@ differs from @Conditions@ as follows:
--
-- -   When you specify more than one condition, you assign all resources
--     that match AT LEAST ONE condition (using OR logic).
--
-- -   @ListOfTags@ only supports @StringEquals@. @Conditions@ supports
--     @StringEquals@, @StringLike@, @StringNotEquals@, and
--     @StringNotLike@.
--
-- 'notResources', 'backupSelection_notResources' - A list of Amazon Resource Names (ARNs) to exclude from a backup plan.
-- The maximum number of ARNs is 500 without wildcards, or 30 ARNs with
-- wildcards.
--
-- If you need to exclude many resources from a backup plan, consider a
-- different resource selection strategy, such as assigning only one or a
-- few resource types or refining your resource selection using tags.
--
-- 'selectionName', 'backupSelection_selectionName' - The display name of a resource selection document. Must contain 1 to 50
-- alphanumeric or \'-_.\' characters.
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
    { conditions = Prelude.Nothing,
      resources = Prelude.Nothing,
      listOfTags = Prelude.Nothing,
      notResources = Prelude.Nothing,
      selectionName = pSelectionName_,
      iamRoleArn = pIamRoleArn_
    }

-- | A list of conditions that you define to assign resources to your backup
-- plans using tags. For example,
-- @\"StringEquals\": { \"ConditionKey\": \"aws:ResourceTag\/CreatedByCryo\", \"ConditionValue\": \"true\" },@.
-- Condition operators are case sensitive.
--
-- @Conditions@ differs from @ListOfTags@ as follows:
--
-- -   When you specify more than one condition, you only assign the
--     resources that match ALL conditions (using AND logic).
--
-- -   @Conditions@ supports @StringEquals@, @StringLike@,
--     @StringNotEquals@, and @StringNotLike@. @ListOfTags@ only supports
--     @StringEquals@.
backupSelection_conditions :: Lens.Lens' BackupSelection (Prelude.Maybe Conditions)
backupSelection_conditions = Lens.lens (\BackupSelection' {conditions} -> conditions) (\s@BackupSelection' {} a -> s {conditions = a} :: BackupSelection)

-- | A list of Amazon Resource Names (ARNs) to assign to a backup plan. The
-- maximum number of ARNs is 500 without wildcards, or 30 ARNs with
-- wildcards.
--
-- If you need to assign many resources to a backup plan, consider a
-- different resource selection strategy, such as assigning all resources
-- of a resource type or refining your resource selection using tags.
backupSelection_resources :: Lens.Lens' BackupSelection (Prelude.Maybe [Prelude.Text])
backupSelection_resources = Lens.lens (\BackupSelection' {resources} -> resources) (\s@BackupSelection' {} a -> s {resources = a} :: BackupSelection) Prelude.. Lens.mapping Lens.coerced

-- | A list of conditions that you define to assign resources to your backup
-- plans using tags. For example,
-- @\"StringEquals\": { \"ConditionKey\": \"aws:ResourceTag\/CreatedByCryo\", \"ConditionValue\": \"true\" },@.
-- Condition operators are case sensitive.
--
-- @ListOfTags@ differs from @Conditions@ as follows:
--
-- -   When you specify more than one condition, you assign all resources
--     that match AT LEAST ONE condition (using OR logic).
--
-- -   @ListOfTags@ only supports @StringEquals@. @Conditions@ supports
--     @StringEquals@, @StringLike@, @StringNotEquals@, and
--     @StringNotLike@.
backupSelection_listOfTags :: Lens.Lens' BackupSelection (Prelude.Maybe [Condition])
backupSelection_listOfTags = Lens.lens (\BackupSelection' {listOfTags} -> listOfTags) (\s@BackupSelection' {} a -> s {listOfTags = a} :: BackupSelection) Prelude.. Lens.mapping Lens.coerced

-- | A list of Amazon Resource Names (ARNs) to exclude from a backup plan.
-- The maximum number of ARNs is 500 without wildcards, or 30 ARNs with
-- wildcards.
--
-- If you need to exclude many resources from a backup plan, consider a
-- different resource selection strategy, such as assigning only one or a
-- few resource types or refining your resource selection using tags.
backupSelection_notResources :: Lens.Lens' BackupSelection (Prelude.Maybe [Prelude.Text])
backupSelection_notResources = Lens.lens (\BackupSelection' {notResources} -> notResources) (\s@BackupSelection' {} a -> s {notResources = a} :: BackupSelection) Prelude.. Lens.mapping Lens.coerced

-- | The display name of a resource selection document. Must contain 1 to 50
-- alphanumeric or \'-_.\' characters.
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
            Prelude.<$> (x Core..:? "Conditions")
            Prelude.<*> (x Core..:? "Resources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ListOfTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "NotResources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "SelectionName")
            Prelude.<*> (x Core..: "IamRoleArn")
      )

instance Prelude.Hashable BackupSelection where
  hashWithSalt _salt BackupSelection' {..} =
    _salt `Prelude.hashWithSalt` conditions
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` listOfTags
      `Prelude.hashWithSalt` notResources
      `Prelude.hashWithSalt` selectionName
      `Prelude.hashWithSalt` iamRoleArn

instance Prelude.NFData BackupSelection where
  rnf BackupSelection' {..} =
    Prelude.rnf conditions
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf listOfTags
      `Prelude.seq` Prelude.rnf notResources
      `Prelude.seq` Prelude.rnf selectionName
      `Prelude.seq` Prelude.rnf iamRoleArn

instance Core.ToJSON BackupSelection where
  toJSON BackupSelection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Conditions" Core..=) Prelude.<$> conditions,
            ("Resources" Core..=) Prelude.<$> resources,
            ("ListOfTags" Core..=) Prelude.<$> listOfTags,
            ("NotResources" Core..=) Prelude.<$> notResources,
            Prelude.Just ("SelectionName" Core..= selectionName),
            Prelude.Just ("IamRoleArn" Core..= iamRoleArn)
          ]
      )
