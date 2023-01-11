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
-- Module      : Amazonka.DLM.Types.Schedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.Schedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.ArchiveRule
import Amazonka.DLM.Types.CreateRule
import Amazonka.DLM.Types.CrossRegionCopyRule
import Amazonka.DLM.Types.DeprecateRule
import Amazonka.DLM.Types.FastRestoreRule
import Amazonka.DLM.Types.RetainRule
import Amazonka.DLM.Types.ShareRule
import Amazonka.DLM.Types.Tag
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __[Snapshot and AMI policies only]__ Specifies a schedule for a snapshot
-- or AMI lifecycle policy.
--
-- /See:/ 'newSchedule' smart constructor.
data Schedule = Schedule'
  { -- | __[Snapshot policies that target volumes only]__ The snapshot archiving
    -- rule for the schedule. When you specify an archiving rule, snapshots are
    -- automatically moved from the standard tier to the archive tier once the
    -- schedule\'s retention threshold is met. Snapshots are then retained in
    -- the archive tier for the archive retention period that you specify.
    --
    -- For more information about using snapshot archiving, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshot-ami-policy.html#dlm-archive Considerations for snapshot lifecycle policies>.
    archiveRule :: Prelude.Maybe ArchiveRule,
    -- | Copy all user-defined tags on a source volume to snapshots of the volume
    -- created by this policy.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | The creation rule.
    createRule :: Prelude.Maybe CreateRule,
    -- | Specifies a rule for copying snapshots or AMIs across regions.
    --
    -- You can\'t specify cross-Region copy rules for policies that create
    -- snapshots on an Outpost. If the policy creates snapshots in a Region,
    -- then snapshots can be copied to up to three Regions or Outposts.
    crossRegionCopyRules :: Prelude.Maybe [CrossRegionCopyRule],
    -- | __[AMI policies only]__ The AMI deprecation rule for the schedule.
    deprecateRule :: Prelude.Maybe DeprecateRule,
    -- | __[Snapshot policies only]__ The rule for enabling fast snapshot
    -- restore.
    fastRestoreRule :: Prelude.Maybe FastRestoreRule,
    -- | The name of the schedule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The retention rule for snapshots or AMIs created by the policy.
    retainRule :: Prelude.Maybe RetainRule,
    -- | __[Snapshot policies only]__ The rule for sharing snapshots with other
    -- Amazon Web Services accounts.
    shareRules :: Prelude.Maybe [ShareRule],
    -- | The tags to apply to policy-created resources. These user-defined tags
    -- are in addition to the Amazon Web Services-added lifecycle tags.
    tagsToAdd :: Prelude.Maybe [Tag],
    -- | __[AMI policies and snapshot policies that target instances only]__ A
    -- collection of key\/value pairs with values determined dynamically when
    -- the policy is executed. Keys may be any valid Amazon EC2 tag key. Values
    -- must be in one of the two following formats: @$(instance-id)@ or
    -- @$(timestamp)@. Variable tags are only valid for EBS Snapshot Management
    -- – Instance policies.
    variableTags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Schedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveRule', 'schedule_archiveRule' - __[Snapshot policies that target volumes only]__ The snapshot archiving
-- rule for the schedule. When you specify an archiving rule, snapshots are
-- automatically moved from the standard tier to the archive tier once the
-- schedule\'s retention threshold is met. Snapshots are then retained in
-- the archive tier for the archive retention period that you specify.
--
-- For more information about using snapshot archiving, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshot-ami-policy.html#dlm-archive Considerations for snapshot lifecycle policies>.
--
-- 'copyTags', 'schedule_copyTags' - Copy all user-defined tags on a source volume to snapshots of the volume
-- created by this policy.
--
-- 'createRule', 'schedule_createRule' - The creation rule.
--
-- 'crossRegionCopyRules', 'schedule_crossRegionCopyRules' - Specifies a rule for copying snapshots or AMIs across regions.
--
-- You can\'t specify cross-Region copy rules for policies that create
-- snapshots on an Outpost. If the policy creates snapshots in a Region,
-- then snapshots can be copied to up to three Regions or Outposts.
--
-- 'deprecateRule', 'schedule_deprecateRule' - __[AMI policies only]__ The AMI deprecation rule for the schedule.
--
-- 'fastRestoreRule', 'schedule_fastRestoreRule' - __[Snapshot policies only]__ The rule for enabling fast snapshot
-- restore.
--
-- 'name', 'schedule_name' - The name of the schedule.
--
-- 'retainRule', 'schedule_retainRule' - The retention rule for snapshots or AMIs created by the policy.
--
-- 'shareRules', 'schedule_shareRules' - __[Snapshot policies only]__ The rule for sharing snapshots with other
-- Amazon Web Services accounts.
--
-- 'tagsToAdd', 'schedule_tagsToAdd' - The tags to apply to policy-created resources. These user-defined tags
-- are in addition to the Amazon Web Services-added lifecycle tags.
--
-- 'variableTags', 'schedule_variableTags' - __[AMI policies and snapshot policies that target instances only]__ A
-- collection of key\/value pairs with values determined dynamically when
-- the policy is executed. Keys may be any valid Amazon EC2 tag key. Values
-- must be in one of the two following formats: @$(instance-id)@ or
-- @$(timestamp)@. Variable tags are only valid for EBS Snapshot Management
-- – Instance policies.
newSchedule ::
  Schedule
newSchedule =
  Schedule'
    { archiveRule = Prelude.Nothing,
      copyTags = Prelude.Nothing,
      createRule = Prelude.Nothing,
      crossRegionCopyRules = Prelude.Nothing,
      deprecateRule = Prelude.Nothing,
      fastRestoreRule = Prelude.Nothing,
      name = Prelude.Nothing,
      retainRule = Prelude.Nothing,
      shareRules = Prelude.Nothing,
      tagsToAdd = Prelude.Nothing,
      variableTags = Prelude.Nothing
    }

-- | __[Snapshot policies that target volumes only]__ The snapshot archiving
-- rule for the schedule. When you specify an archiving rule, snapshots are
-- automatically moved from the standard tier to the archive tier once the
-- schedule\'s retention threshold is met. Snapshots are then retained in
-- the archive tier for the archive retention period that you specify.
--
-- For more information about using snapshot archiving, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshot-ami-policy.html#dlm-archive Considerations for snapshot lifecycle policies>.
schedule_archiveRule :: Lens.Lens' Schedule (Prelude.Maybe ArchiveRule)
schedule_archiveRule = Lens.lens (\Schedule' {archiveRule} -> archiveRule) (\s@Schedule' {} a -> s {archiveRule = a} :: Schedule)

-- | Copy all user-defined tags on a source volume to snapshots of the volume
-- created by this policy.
schedule_copyTags :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Bool)
schedule_copyTags = Lens.lens (\Schedule' {copyTags} -> copyTags) (\s@Schedule' {} a -> s {copyTags = a} :: Schedule)

-- | The creation rule.
schedule_createRule :: Lens.Lens' Schedule (Prelude.Maybe CreateRule)
schedule_createRule = Lens.lens (\Schedule' {createRule} -> createRule) (\s@Schedule' {} a -> s {createRule = a} :: Schedule)

-- | Specifies a rule for copying snapshots or AMIs across regions.
--
-- You can\'t specify cross-Region copy rules for policies that create
-- snapshots on an Outpost. If the policy creates snapshots in a Region,
-- then snapshots can be copied to up to three Regions or Outposts.
schedule_crossRegionCopyRules :: Lens.Lens' Schedule (Prelude.Maybe [CrossRegionCopyRule])
schedule_crossRegionCopyRules = Lens.lens (\Schedule' {crossRegionCopyRules} -> crossRegionCopyRules) (\s@Schedule' {} a -> s {crossRegionCopyRules = a} :: Schedule) Prelude.. Lens.mapping Lens.coerced

-- | __[AMI policies only]__ The AMI deprecation rule for the schedule.
schedule_deprecateRule :: Lens.Lens' Schedule (Prelude.Maybe DeprecateRule)
schedule_deprecateRule = Lens.lens (\Schedule' {deprecateRule} -> deprecateRule) (\s@Schedule' {} a -> s {deprecateRule = a} :: Schedule)

-- | __[Snapshot policies only]__ The rule for enabling fast snapshot
-- restore.
schedule_fastRestoreRule :: Lens.Lens' Schedule (Prelude.Maybe FastRestoreRule)
schedule_fastRestoreRule = Lens.lens (\Schedule' {fastRestoreRule} -> fastRestoreRule) (\s@Schedule' {} a -> s {fastRestoreRule = a} :: Schedule)

-- | The name of the schedule.
schedule_name :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Text)
schedule_name = Lens.lens (\Schedule' {name} -> name) (\s@Schedule' {} a -> s {name = a} :: Schedule)

-- | The retention rule for snapshots or AMIs created by the policy.
schedule_retainRule :: Lens.Lens' Schedule (Prelude.Maybe RetainRule)
schedule_retainRule = Lens.lens (\Schedule' {retainRule} -> retainRule) (\s@Schedule' {} a -> s {retainRule = a} :: Schedule)

-- | __[Snapshot policies only]__ The rule for sharing snapshots with other
-- Amazon Web Services accounts.
schedule_shareRules :: Lens.Lens' Schedule (Prelude.Maybe [ShareRule])
schedule_shareRules = Lens.lens (\Schedule' {shareRules} -> shareRules) (\s@Schedule' {} a -> s {shareRules = a} :: Schedule) Prelude.. Lens.mapping Lens.coerced

-- | The tags to apply to policy-created resources. These user-defined tags
-- are in addition to the Amazon Web Services-added lifecycle tags.
schedule_tagsToAdd :: Lens.Lens' Schedule (Prelude.Maybe [Tag])
schedule_tagsToAdd = Lens.lens (\Schedule' {tagsToAdd} -> tagsToAdd) (\s@Schedule' {} a -> s {tagsToAdd = a} :: Schedule) Prelude.. Lens.mapping Lens.coerced

-- | __[AMI policies and snapshot policies that target instances only]__ A
-- collection of key\/value pairs with values determined dynamically when
-- the policy is executed. Keys may be any valid Amazon EC2 tag key. Values
-- must be in one of the two following formats: @$(instance-id)@ or
-- @$(timestamp)@. Variable tags are only valid for EBS Snapshot Management
-- – Instance policies.
schedule_variableTags :: Lens.Lens' Schedule (Prelude.Maybe [Tag])
schedule_variableTags = Lens.lens (\Schedule' {variableTags} -> variableTags) (\s@Schedule' {} a -> s {variableTags = a} :: Schedule) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Schedule where
  parseJSON =
    Data.withObject
      "Schedule"
      ( \x ->
          Schedule'
            Prelude.<$> (x Data..:? "ArchiveRule")
            Prelude.<*> (x Data..:? "CopyTags")
            Prelude.<*> (x Data..:? "CreateRule")
            Prelude.<*> ( x Data..:? "CrossRegionCopyRules"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DeprecateRule")
            Prelude.<*> (x Data..:? "FastRestoreRule")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RetainRule")
            Prelude.<*> (x Data..:? "ShareRules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TagsToAdd" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VariableTags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Schedule where
  hashWithSalt _salt Schedule' {..} =
    _salt `Prelude.hashWithSalt` archiveRule
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` createRule
      `Prelude.hashWithSalt` crossRegionCopyRules
      `Prelude.hashWithSalt` deprecateRule
      `Prelude.hashWithSalt` fastRestoreRule
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` retainRule
      `Prelude.hashWithSalt` shareRules
      `Prelude.hashWithSalt` tagsToAdd
      `Prelude.hashWithSalt` variableTags

instance Prelude.NFData Schedule where
  rnf Schedule' {..} =
    Prelude.rnf archiveRule
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf createRule
      `Prelude.seq` Prelude.rnf crossRegionCopyRules
      `Prelude.seq` Prelude.rnf deprecateRule
      `Prelude.seq` Prelude.rnf fastRestoreRule
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf retainRule
      `Prelude.seq` Prelude.rnf shareRules
      `Prelude.seq` Prelude.rnf tagsToAdd
      `Prelude.seq` Prelude.rnf variableTags

instance Data.ToJSON Schedule where
  toJSON Schedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ArchiveRule" Data..=) Prelude.<$> archiveRule,
            ("CopyTags" Data..=) Prelude.<$> copyTags,
            ("CreateRule" Data..=) Prelude.<$> createRule,
            ("CrossRegionCopyRules" Data..=)
              Prelude.<$> crossRegionCopyRules,
            ("DeprecateRule" Data..=) Prelude.<$> deprecateRule,
            ("FastRestoreRule" Data..=)
              Prelude.<$> fastRestoreRule,
            ("Name" Data..=) Prelude.<$> name,
            ("RetainRule" Data..=) Prelude.<$> retainRule,
            ("ShareRules" Data..=) Prelude.<$> shareRules,
            ("TagsToAdd" Data..=) Prelude.<$> tagsToAdd,
            ("VariableTags" Data..=) Prelude.<$> variableTags
          ]
      )
