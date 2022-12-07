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
-- Module      : Amazonka.SSM.Types.OpsItemSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsItemSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.OpsItemDataValue
import Amazonka.SSM.Types.OpsItemStatus

-- | A count of OpsItems.
--
-- /See:/ 'newOpsItemSummary' smart constructor.
data OpsItemSummary = OpsItemSummary'
  { -- | A list of OpsItems by severity.
    severity :: Prelude.Maybe Prelude.Text,
    -- | The date and time the OpsItem was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The time specified in a change request for a runbook workflow to start.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedStartTime :: Prelude.Maybe Data.POSIX,
    -- | The time specified in a change request for a runbook workflow to end.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedEndTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the OpsItem.
    opsItemId :: Prelude.Maybe Prelude.Text,
    -- | The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
    status :: Prelude.Maybe OpsItemStatus,
    -- | The date and time the OpsItem was last updated.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | A short heading that describes the nature of the OpsItem and the
    -- impacted resource.
    title :: Prelude.Maybe Prelude.Text,
    -- | The impacted Amazon Web Services resource.
    source :: Prelude.Maybe Prelude.Text,
    -- | The importance of this OpsItem in relation to other OpsItems in the
    -- system.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The type of OpsItem. Systems Manager supports the following types of
    -- OpsItems:
    --
    -- -   @\/aws\/issue@
    --
    --     This type of OpsItem is used for default OpsItems created by
    --     OpsCenter.
    --
    -- -   @\/aws\/changerequest@
    --
    --     This type of OpsItem is used by Change Manager for reviewing and
    --     approving or rejecting change requests.
    --
    -- -   @\/aws\/insights@
    --
    --     This type of OpsItem is used by OpsCenter for aggregating and
    --     reporting on duplicate OpsItems.
    opsItemType :: Prelude.Maybe Prelude.Text,
    -- | A list of OpsItems by category.
    category :: Prelude.Maybe Prelude.Text,
    -- | Operational data is custom data that provides useful reference details
    -- about the OpsItem.
    operationalData :: Prelude.Maybe (Prelude.HashMap Prelude.Text OpsItemDataValue),
    -- | The time a runbook workflow started. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualStartTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the IAM entity that created the
    -- OpsItem.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM entity that created the
    -- OpsItem.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The time a runbook workflow ended. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualEndTime :: Prelude.Maybe Data.POSIX
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
-- 'severity', 'opsItemSummary_severity' - A list of OpsItems by severity.
--
-- 'createdTime', 'opsItemSummary_createdTime' - The date and time the OpsItem was created.
--
-- 'plannedStartTime', 'opsItemSummary_plannedStartTime' - The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'plannedEndTime', 'opsItemSummary_plannedEndTime' - The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'opsItemId', 'opsItemSummary_opsItemId' - The ID of the OpsItem.
--
-- 'status', 'opsItemSummary_status' - The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
--
-- 'lastModifiedTime', 'opsItemSummary_lastModifiedTime' - The date and time the OpsItem was last updated.
--
-- 'title', 'opsItemSummary_title' - A short heading that describes the nature of the OpsItem and the
-- impacted resource.
--
-- 'source', 'opsItemSummary_source' - The impacted Amazon Web Services resource.
--
-- 'priority', 'opsItemSummary_priority' - The importance of this OpsItem in relation to other OpsItems in the
-- system.
--
-- 'opsItemType', 'opsItemSummary_opsItemType' - The type of OpsItem. Systems Manager supports the following types of
-- OpsItems:
--
-- -   @\/aws\/issue@
--
--     This type of OpsItem is used for default OpsItems created by
--     OpsCenter.
--
-- -   @\/aws\/changerequest@
--
--     This type of OpsItem is used by Change Manager for reviewing and
--     approving or rejecting change requests.
--
-- -   @\/aws\/insights@
--
--     This type of OpsItem is used by OpsCenter for aggregating and
--     reporting on duplicate OpsItems.
--
-- 'category', 'opsItemSummary_category' - A list of OpsItems by category.
--
-- 'operationalData', 'opsItemSummary_operationalData' - Operational data is custom data that provides useful reference details
-- about the OpsItem.
--
-- 'actualStartTime', 'opsItemSummary_actualStartTime' - The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'lastModifiedBy', 'opsItemSummary_lastModifiedBy' - The Amazon Resource Name (ARN) of the IAM entity that created the
-- OpsItem.
--
-- 'createdBy', 'opsItemSummary_createdBy' - The Amazon Resource Name (ARN) of the IAM entity that created the
-- OpsItem.
--
-- 'actualEndTime', 'opsItemSummary_actualEndTime' - The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
newOpsItemSummary ::
  OpsItemSummary
newOpsItemSummary =
  OpsItemSummary'
    { severity = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      plannedStartTime = Prelude.Nothing,
      plannedEndTime = Prelude.Nothing,
      opsItemId = Prelude.Nothing,
      status = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      title = Prelude.Nothing,
      source = Prelude.Nothing,
      priority = Prelude.Nothing,
      opsItemType = Prelude.Nothing,
      category = Prelude.Nothing,
      operationalData = Prelude.Nothing,
      actualStartTime = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      actualEndTime = Prelude.Nothing
    }

-- | A list of OpsItems by severity.
opsItemSummary_severity :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_severity = Lens.lens (\OpsItemSummary' {severity} -> severity) (\s@OpsItemSummary' {} a -> s {severity = a} :: OpsItemSummary)

-- | The date and time the OpsItem was created.
opsItemSummary_createdTime :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemSummary_createdTime = Lens.lens (\OpsItemSummary' {createdTime} -> createdTime) (\s@OpsItemSummary' {} a -> s {createdTime = a} :: OpsItemSummary) Prelude.. Lens.mapping Data._Time

-- | The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
opsItemSummary_plannedStartTime :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemSummary_plannedStartTime = Lens.lens (\OpsItemSummary' {plannedStartTime} -> plannedStartTime) (\s@OpsItemSummary' {} a -> s {plannedStartTime = a} :: OpsItemSummary) Prelude.. Lens.mapping Data._Time

-- | The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
opsItemSummary_plannedEndTime :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemSummary_plannedEndTime = Lens.lens (\OpsItemSummary' {plannedEndTime} -> plannedEndTime) (\s@OpsItemSummary' {} a -> s {plannedEndTime = a} :: OpsItemSummary) Prelude.. Lens.mapping Data._Time

-- | The ID of the OpsItem.
opsItemSummary_opsItemId :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_opsItemId = Lens.lens (\OpsItemSummary' {opsItemId} -> opsItemId) (\s@OpsItemSummary' {} a -> s {opsItemId = a} :: OpsItemSummary)

-- | The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
opsItemSummary_status :: Lens.Lens' OpsItemSummary (Prelude.Maybe OpsItemStatus)
opsItemSummary_status = Lens.lens (\OpsItemSummary' {status} -> status) (\s@OpsItemSummary' {} a -> s {status = a} :: OpsItemSummary)

-- | The date and time the OpsItem was last updated.
opsItemSummary_lastModifiedTime :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemSummary_lastModifiedTime = Lens.lens (\OpsItemSummary' {lastModifiedTime} -> lastModifiedTime) (\s@OpsItemSummary' {} a -> s {lastModifiedTime = a} :: OpsItemSummary) Prelude.. Lens.mapping Data._Time

-- | A short heading that describes the nature of the OpsItem and the
-- impacted resource.
opsItemSummary_title :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_title = Lens.lens (\OpsItemSummary' {title} -> title) (\s@OpsItemSummary' {} a -> s {title = a} :: OpsItemSummary)

-- | The impacted Amazon Web Services resource.
opsItemSummary_source :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_source = Lens.lens (\OpsItemSummary' {source} -> source) (\s@OpsItemSummary' {} a -> s {source = a} :: OpsItemSummary)

-- | The importance of this OpsItem in relation to other OpsItems in the
-- system.
opsItemSummary_priority :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Natural)
opsItemSummary_priority = Lens.lens (\OpsItemSummary' {priority} -> priority) (\s@OpsItemSummary' {} a -> s {priority = a} :: OpsItemSummary)

-- | The type of OpsItem. Systems Manager supports the following types of
-- OpsItems:
--
-- -   @\/aws\/issue@
--
--     This type of OpsItem is used for default OpsItems created by
--     OpsCenter.
--
-- -   @\/aws\/changerequest@
--
--     This type of OpsItem is used by Change Manager for reviewing and
--     approving or rejecting change requests.
--
-- -   @\/aws\/insights@
--
--     This type of OpsItem is used by OpsCenter for aggregating and
--     reporting on duplicate OpsItems.
opsItemSummary_opsItemType :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_opsItemType = Lens.lens (\OpsItemSummary' {opsItemType} -> opsItemType) (\s@OpsItemSummary' {} a -> s {opsItemType = a} :: OpsItemSummary)

-- | A list of OpsItems by category.
opsItemSummary_category :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_category = Lens.lens (\OpsItemSummary' {category} -> category) (\s@OpsItemSummary' {} a -> s {category = a} :: OpsItemSummary)

-- | Operational data is custom data that provides useful reference details
-- about the OpsItem.
opsItemSummary_operationalData :: Lens.Lens' OpsItemSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text OpsItemDataValue))
opsItemSummary_operationalData = Lens.lens (\OpsItemSummary' {operationalData} -> operationalData) (\s@OpsItemSummary' {} a -> s {operationalData = a} :: OpsItemSummary) Prelude.. Lens.mapping Lens.coerced

-- | The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
opsItemSummary_actualStartTime :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemSummary_actualStartTime = Lens.lens (\OpsItemSummary' {actualStartTime} -> actualStartTime) (\s@OpsItemSummary' {} a -> s {actualStartTime = a} :: OpsItemSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the IAM entity that created the
-- OpsItem.
opsItemSummary_lastModifiedBy :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_lastModifiedBy = Lens.lens (\OpsItemSummary' {lastModifiedBy} -> lastModifiedBy) (\s@OpsItemSummary' {} a -> s {lastModifiedBy = a} :: OpsItemSummary)

-- | The Amazon Resource Name (ARN) of the IAM entity that created the
-- OpsItem.
opsItemSummary_createdBy :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.Text)
opsItemSummary_createdBy = Lens.lens (\OpsItemSummary' {createdBy} -> createdBy) (\s@OpsItemSummary' {} a -> s {createdBy = a} :: OpsItemSummary)

-- | The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
opsItemSummary_actualEndTime :: Lens.Lens' OpsItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemSummary_actualEndTime = Lens.lens (\OpsItemSummary' {actualEndTime} -> actualEndTime) (\s@OpsItemSummary' {} a -> s {actualEndTime = a} :: OpsItemSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON OpsItemSummary where
  parseJSON =
    Data.withObject
      "OpsItemSummary"
      ( \x ->
          OpsItemSummary'
            Prelude.<$> (x Data..:? "Severity")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "PlannedStartTime")
            Prelude.<*> (x Data..:? "PlannedEndTime")
            Prelude.<*> (x Data..:? "OpsItemId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..:? "Source")
            Prelude.<*> (x Data..:? "Priority")
            Prelude.<*> (x Data..:? "OpsItemType")
            Prelude.<*> (x Data..:? "Category")
            Prelude.<*> ( x Data..:? "OperationalData"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ActualStartTime")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "ActualEndTime")
      )

instance Prelude.Hashable OpsItemSummary where
  hashWithSalt _salt OpsItemSummary' {..} =
    _salt `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` plannedStartTime
      `Prelude.hashWithSalt` plannedEndTime
      `Prelude.hashWithSalt` opsItemId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` opsItemType
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` operationalData
      `Prelude.hashWithSalt` actualStartTime
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` actualEndTime

instance Prelude.NFData OpsItemSummary where
  rnf OpsItemSummary' {..} =
    Prelude.rnf severity
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf plannedStartTime
      `Prelude.seq` Prelude.rnf plannedEndTime
      `Prelude.seq` Prelude.rnf opsItemId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf opsItemType
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf operationalData
      `Prelude.seq` Prelude.rnf actualStartTime
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf actualEndTime
