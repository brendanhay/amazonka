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
-- Module      : Network.AWS.SSM.Types.OpsItemSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.OpsItemDataValue
import Network.AWS.SSM.Types.OpsItemStatus

-- | A count of OpsItems.
--
-- /See:/ 'newOpsItemSummary' smart constructor.
data OpsItemSummary = OpsItemSummary'
  { -- | The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
    status :: Prelude.Maybe OpsItemStatus,
    -- | The time specified in a change request for a runbook workflow to end.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedEndTime :: Prelude.Maybe Core.POSIX,
    -- | A list of OpsItems by severity.
    severity :: Prelude.Maybe Prelude.Text,
    -- | The time a runbook workflow started. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualStartTime :: Prelude.Maybe Core.POSIX,
    -- | A list of OpsItems by category.
    category :: Prelude.Maybe Prelude.Text,
    -- | Operational data is custom data that provides useful reference details
    -- about the OpsItem.
    operationalData :: Prelude.Maybe (Prelude.HashMap Prelude.Text OpsItemDataValue),
    -- | A short heading that describes the nature of the OpsItem and the
    -- impacted resource.
    title :: Prelude.Maybe Prelude.Text,
    -- | The impacted AWS resource.
    source :: Prelude.Maybe Prelude.Text,
    -- | The date and time the OpsItem was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The importance of this OpsItem in relation to other OpsItems in the
    -- system.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The time a runbook workflow ended. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualEndTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the OpsItem.
    opsItemId :: Prelude.Maybe Prelude.Text,
    -- | The type of OpsItem. Currently, the only valid values are
    -- @\/aws\/changerequest@ and @\/aws\/issue@.
    opsItemType :: Prelude.Maybe Prelude.Text,
    -- | The time specified in a change request for a runbook workflow to start.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedStartTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time the OpsItem was last updated.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the IAM entity that created the
    -- OpsItem.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM entity that created the
    -- OpsItem.
    lastModifiedBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsItemSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'opsItemSummary_status' - The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
--
-- 'plannedEndTime', 'opsItemSummary_plannedEndTime' - The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'severity', 'opsItemSummary_severity' - A list of OpsItems by severity.
--
-- 'actualStartTime', 'opsItemSummary_actualStartTime' - The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'category', 'opsItemSummary_category' - A list of OpsItems by category.
--
-- 'operationalData', 'opsItemSummary_operationalData' - Operational data is custom data that provides useful reference details
-- about the OpsItem.
--
-- 'title', 'opsItemSummary_title' - A short heading that describes the nature of the OpsItem and the
-- impacted resource.
--
-- 'source', 'opsItemSummary_source' - The impacted AWS resource.
--
-- 'createdTime', 'opsItemSummary_createdTime' - The date and time the OpsItem was created.
--
-- 'priority', 'opsItemSummary_priority' - The importance of this OpsItem in relation to other OpsItems in the
-- system.
--
-- 'actualEndTime', 'opsItemSummary_actualEndTime' - The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'opsItemId', 'opsItemSummary_opsItemId' - The ID of the OpsItem.
--
-- 'opsItemType', 'opsItemSummary_opsItemType' - The type of OpsItem. Currently, the only valid values are
-- @\/aws\/changerequest@ and @\/aws\/issue@.
--
-- 'plannedStartTime', 'opsItemSummary_plannedStartTime' - The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'lastModifiedTime', 'opsItemSummary_lastModifiedTime' - The date and time the OpsItem was last updated.
--
-- 'createdBy', 'opsItemSummary_createdBy' - The Amazon Resource Name (ARN) of the IAM entity that created the
-- OpsItem.
--
-- 'lastModifiedBy', 'opsItemSummary_lastModifiedBy' - The Amazon Resource Name (ARN) of the IAM entity that created the
-- OpsItem.
newOpsItemSummary ::
  OpsItemSummary
newOpsItemSummary =
  OpsItemSummary'
    { status = Prelude.Nothing,
      plannedEndTime = Prelude.Nothing,
      severity = Prelude.Nothing,
      actualStartTime = Prelude.Nothing,
      category = Prelude.Nothing,
      operationalData = Prelude.Nothing,
      title = Prelude.Nothing,
      source = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      priority = Prelude.Nothing,
      actualEndTime = Prelude.Nothing,
      opsItemId = Prelude.Nothing,
      opsItemType = Prelude.Nothing,
      plannedStartTime = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing
    }

-- | The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
opsItemSummary_status :: Lens.Lens' OpsItemSummary (Prelude.Maybe OpsItemStatus)
opsItemSummary_status = Lens.lens (\OpsItemSummary' {status} -> status) (\s@OpsItemSummary' {} a -> s {status = a} :: OpsItemSummary)

-- | The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
opsItemSummary_plannedEndTime :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemSummary_plannedEndTime = Lens.lens (\OpsItemSummary' {plannedEndTime} -> plannedEndTime) (\s@OpsItemSummary' {} a -> s {plannedEndTime = a} :: OpsItemSummary) Prelude.. Lens.mapping Core._Time

-- | A list of OpsItems by severity.
opsItemSummary_severity :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_severity = Lens.lens (\OpsItemSummary' {severity} -> severity) (\s@OpsItemSummary' {} a -> s {severity = a} :: OpsItemSummary)

-- | The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
opsItemSummary_actualStartTime :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemSummary_actualStartTime = Lens.lens (\OpsItemSummary' {actualStartTime} -> actualStartTime) (\s@OpsItemSummary' {} a -> s {actualStartTime = a} :: OpsItemSummary) Prelude.. Lens.mapping Core._Time

-- | A list of OpsItems by category.
opsItemSummary_category :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_category = Lens.lens (\OpsItemSummary' {category} -> category) (\s@OpsItemSummary' {} a -> s {category = a} :: OpsItemSummary)

-- | Operational data is custom data that provides useful reference details
-- about the OpsItem.
opsItemSummary_operationalData :: Lens.Lens' OpsItemSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text OpsItemDataValue))
opsItemSummary_operationalData = Lens.lens (\OpsItemSummary' {operationalData} -> operationalData) (\s@OpsItemSummary' {} a -> s {operationalData = a} :: OpsItemSummary) Prelude.. Lens.mapping Lens._Coerce

-- | A short heading that describes the nature of the OpsItem and the
-- impacted resource.
opsItemSummary_title :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_title = Lens.lens (\OpsItemSummary' {title} -> title) (\s@OpsItemSummary' {} a -> s {title = a} :: OpsItemSummary)

-- | The impacted AWS resource.
opsItemSummary_source :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_source = Lens.lens (\OpsItemSummary' {source} -> source) (\s@OpsItemSummary' {} a -> s {source = a} :: OpsItemSummary)

-- | The date and time the OpsItem was created.
opsItemSummary_createdTime :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemSummary_createdTime = Lens.lens (\OpsItemSummary' {createdTime} -> createdTime) (\s@OpsItemSummary' {} a -> s {createdTime = a} :: OpsItemSummary) Prelude.. Lens.mapping Core._Time

-- | The importance of this OpsItem in relation to other OpsItems in the
-- system.
opsItemSummary_priority :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Natural)
opsItemSummary_priority = Lens.lens (\OpsItemSummary' {priority} -> priority) (\s@OpsItemSummary' {} a -> s {priority = a} :: OpsItemSummary)

-- | The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
opsItemSummary_actualEndTime :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemSummary_actualEndTime = Lens.lens (\OpsItemSummary' {actualEndTime} -> actualEndTime) (\s@OpsItemSummary' {} a -> s {actualEndTime = a} :: OpsItemSummary) Prelude.. Lens.mapping Core._Time

-- | The ID of the OpsItem.
opsItemSummary_opsItemId :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_opsItemId = Lens.lens (\OpsItemSummary' {opsItemId} -> opsItemId) (\s@OpsItemSummary' {} a -> s {opsItemId = a} :: OpsItemSummary)

-- | The type of OpsItem. Currently, the only valid values are
-- @\/aws\/changerequest@ and @\/aws\/issue@.
opsItemSummary_opsItemType :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_opsItemType = Lens.lens (\OpsItemSummary' {opsItemType} -> opsItemType) (\s@OpsItemSummary' {} a -> s {opsItemType = a} :: OpsItemSummary)

-- | The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
opsItemSummary_plannedStartTime :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemSummary_plannedStartTime = Lens.lens (\OpsItemSummary' {plannedStartTime} -> plannedStartTime) (\s@OpsItemSummary' {} a -> s {plannedStartTime = a} :: OpsItemSummary) Prelude.. Lens.mapping Core._Time

-- | The date and time the OpsItem was last updated.
opsItemSummary_lastModifiedTime :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemSummary_lastModifiedTime = Lens.lens (\OpsItemSummary' {lastModifiedTime} -> lastModifiedTime) (\s@OpsItemSummary' {} a -> s {lastModifiedTime = a} :: OpsItemSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the IAM entity that created the
-- OpsItem.
opsItemSummary_createdBy :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_createdBy = Lens.lens (\OpsItemSummary' {createdBy} -> createdBy) (\s@OpsItemSummary' {} a -> s {createdBy = a} :: OpsItemSummary)

-- | The Amazon Resource Name (ARN) of the IAM entity that created the
-- OpsItem.
opsItemSummary_lastModifiedBy :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_lastModifiedBy = Lens.lens (\OpsItemSummary' {lastModifiedBy} -> lastModifiedBy) (\s@OpsItemSummary' {} a -> s {lastModifiedBy = a} :: OpsItemSummary)

instance Core.FromJSON OpsItemSummary where
  parseJSON =
    Core.withObject
      "OpsItemSummary"
      ( \x ->
          OpsItemSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "PlannedEndTime")
            Prelude.<*> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "ActualStartTime")
            Prelude.<*> (x Core..:? "Category")
            Prelude.<*> ( x Core..:? "OperationalData"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Title")
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "Priority")
            Prelude.<*> (x Core..:? "ActualEndTime")
            Prelude.<*> (x Core..:? "OpsItemId")
            Prelude.<*> (x Core..:? "OpsItemType")
            Prelude.<*> (x Core..:? "PlannedStartTime")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "LastModifiedBy")
      )

instance Prelude.Hashable OpsItemSummary

instance Prelude.NFData OpsItemSummary
