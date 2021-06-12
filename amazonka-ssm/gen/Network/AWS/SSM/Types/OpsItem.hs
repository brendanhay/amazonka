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
-- Module      : Network.AWS.SSM.Types.OpsItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.OpsItemDataValue
import Network.AWS.SSM.Types.OpsItemNotification
import Network.AWS.SSM.Types.OpsItemStatus
import Network.AWS.SSM.Types.RelatedOpsItem

-- | Operations engineers and IT professionals use OpsCenter to view,
-- investigate, and remediate operational issues impacting the performance
-- and health of their AWS resources. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter>
-- in the /AWS Systems Manager User Guide/.
--
-- /See:/ 'newOpsItem' smart constructor.
data OpsItem = OpsItem'
  { -- | The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems-editing-details.html Editing OpsItem details>
    -- in the /AWS Systems Manager User Guide/.
    status :: Core.Maybe OpsItemStatus,
    -- | The time specified in a change request for a runbook workflow to end.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedEndTime :: Core.Maybe Core.POSIX,
    -- | The severity of the OpsItem. Severity options range from 1 to 4.
    severity :: Core.Maybe Core.Text,
    -- | The time a runbook workflow started. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualStartTime :: Core.Maybe Core.POSIX,
    -- | An OpsItem category. Category options include: Availability, Cost,
    -- Performance, Recovery, Security.
    category :: Core.Maybe Core.Text,
    -- | Operational data is custom data that provides useful reference details
    -- about the OpsItem. For example, you can specify log files, error
    -- strings, license keys, troubleshooting tips, or other relevant data. You
    -- enter operational data as key-value pairs. The key has a maximum length
    -- of 128 characters. The value has a maximum size of 20 KB.
    --
    -- Operational data keys /can\'t/ begin with the following: amazon, aws,
    -- amzn, ssm, \/amazon, \/aws, \/amzn, \/ssm.
    --
    -- You can choose to make the data searchable by other users in the account
    -- or you can restrict search access. Searchable data means that all users
    -- with access to the OpsItem Overview page (as provided by the
    -- DescribeOpsItems API action) can view and search on the specified data.
    -- Operational data that is not searchable is only viewable by users who
    -- have access to the OpsItem (as provided by the GetOpsItem API action).
    --
    -- Use the @\/aws\/resources@ key in OperationalData to specify a related
    -- resource in the request. Use the @\/aws\/automations@ key in
    -- OperationalData to associate an Automation runbook with the OpsItem. To
    -- view AWS CLI example commands that use these keys, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually>
    -- in the /AWS Systems Manager User Guide/.
    operationalData :: Core.Maybe (Core.HashMap Core.Text OpsItemDataValue),
    -- | A short heading that describes the nature of the OpsItem and the
    -- impacted resource.
    title :: Core.Maybe Core.Text,
    -- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager. The
    -- impacted resource is a subset of source.
    source :: Core.Maybe Core.Text,
    -- | The date and time the OpsItem was created.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The version of this OpsItem. Each time the OpsItem is edited the version
    -- number increments by one.
    version :: Core.Maybe Core.Text,
    -- | The importance of this OpsItem in relation to other OpsItems in the
    -- system.
    priority :: Core.Maybe Core.Natural,
    -- | The time a runbook workflow ended. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualEndTime :: Core.Maybe Core.POSIX,
    -- | The ID of the OpsItem.
    opsItemId :: Core.Maybe Core.Text,
    -- | The type of OpsItem. Currently, the only valid values are
    -- @\/aws\/changerequest@ and @\/aws\/issue@.
    opsItemType :: Core.Maybe Core.Text,
    -- | The time specified in a change request for a runbook workflow to start.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedStartTime :: Core.Maybe Core.POSIX,
    -- | The date and time the OpsItem was last updated.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
    -- sent when this OpsItem is edited or changed.
    notifications :: Core.Maybe [OpsItemNotification],
    -- | The OpsItem description.
    description :: Core.Maybe Core.Text,
    -- | The ARN of the AWS account that created the OpsItem.
    createdBy :: Core.Maybe Core.Text,
    -- | The ARN of the AWS account that last updated the OpsItem.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | One or more OpsItems that share something in common with the current
    -- OpsItem. For example, related OpsItems can include OpsItems with similar
    -- error messages, impacted resources, or statuses for the impacted
    -- resource.
    relatedOpsItems :: Core.Maybe [RelatedOpsItem]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OpsItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'opsItem_status' - The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
-- For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems-editing-details.html Editing OpsItem details>
-- in the /AWS Systems Manager User Guide/.
--
-- 'plannedEndTime', 'opsItem_plannedEndTime' - The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'severity', 'opsItem_severity' - The severity of the OpsItem. Severity options range from 1 to 4.
--
-- 'actualStartTime', 'opsItem_actualStartTime' - The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'category', 'opsItem_category' - An OpsItem category. Category options include: Availability, Cost,
-- Performance, Recovery, Security.
--
-- 'operationalData', 'opsItem_operationalData' - Operational data is custom data that provides useful reference details
-- about the OpsItem. For example, you can specify log files, error
-- strings, license keys, troubleshooting tips, or other relevant data. You
-- enter operational data as key-value pairs. The key has a maximum length
-- of 128 characters. The value has a maximum size of 20 KB.
--
-- Operational data keys /can\'t/ begin with the following: amazon, aws,
-- amzn, ssm, \/amazon, \/aws, \/amzn, \/ssm.
--
-- You can choose to make the data searchable by other users in the account
-- or you can restrict search access. Searchable data means that all users
-- with access to the OpsItem Overview page (as provided by the
-- DescribeOpsItems API action) can view and search on the specified data.
-- Operational data that is not searchable is only viewable by users who
-- have access to the OpsItem (as provided by the GetOpsItem API action).
--
-- Use the @\/aws\/resources@ key in OperationalData to specify a related
-- resource in the request. Use the @\/aws\/automations@ key in
-- OperationalData to associate an Automation runbook with the OpsItem. To
-- view AWS CLI example commands that use these keys, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually>
-- in the /AWS Systems Manager User Guide/.
--
-- 'title', 'opsItem_title' - A short heading that describes the nature of the OpsItem and the
-- impacted resource.
--
-- 'source', 'opsItem_source' - The origin of the OpsItem, such as Amazon EC2 or Systems Manager. The
-- impacted resource is a subset of source.
--
-- 'createdTime', 'opsItem_createdTime' - The date and time the OpsItem was created.
--
-- 'version', 'opsItem_version' - The version of this OpsItem. Each time the OpsItem is edited the version
-- number increments by one.
--
-- 'priority', 'opsItem_priority' - The importance of this OpsItem in relation to other OpsItems in the
-- system.
--
-- 'actualEndTime', 'opsItem_actualEndTime' - The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'opsItemId', 'opsItem_opsItemId' - The ID of the OpsItem.
--
-- 'opsItemType', 'opsItem_opsItemType' - The type of OpsItem. Currently, the only valid values are
-- @\/aws\/changerequest@ and @\/aws\/issue@.
--
-- 'plannedStartTime', 'opsItem_plannedStartTime' - The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'lastModifiedTime', 'opsItem_lastModifiedTime' - The date and time the OpsItem was last updated.
--
-- 'notifications', 'opsItem_notifications' - The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
--
-- 'description', 'opsItem_description' - The OpsItem description.
--
-- 'createdBy', 'opsItem_createdBy' - The ARN of the AWS account that created the OpsItem.
--
-- 'lastModifiedBy', 'opsItem_lastModifiedBy' - The ARN of the AWS account that last updated the OpsItem.
--
-- 'relatedOpsItems', 'opsItem_relatedOpsItems' - One or more OpsItems that share something in common with the current
-- OpsItem. For example, related OpsItems can include OpsItems with similar
-- error messages, impacted resources, or statuses for the impacted
-- resource.
newOpsItem ::
  OpsItem
newOpsItem =
  OpsItem'
    { status = Core.Nothing,
      plannedEndTime = Core.Nothing,
      severity = Core.Nothing,
      actualStartTime = Core.Nothing,
      category = Core.Nothing,
      operationalData = Core.Nothing,
      title = Core.Nothing,
      source = Core.Nothing,
      createdTime = Core.Nothing,
      version = Core.Nothing,
      priority = Core.Nothing,
      actualEndTime = Core.Nothing,
      opsItemId = Core.Nothing,
      opsItemType = Core.Nothing,
      plannedStartTime = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      notifications = Core.Nothing,
      description = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      relatedOpsItems = Core.Nothing
    }

-- | The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
-- For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems-editing-details.html Editing OpsItem details>
-- in the /AWS Systems Manager User Guide/.
opsItem_status :: Lens.Lens' OpsItem (Core.Maybe OpsItemStatus)
opsItem_status = Lens.lens (\OpsItem' {status} -> status) (\s@OpsItem' {} a -> s {status = a} :: OpsItem)

-- | The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
opsItem_plannedEndTime :: Lens.Lens' OpsItem (Core.Maybe Core.UTCTime)
opsItem_plannedEndTime = Lens.lens (\OpsItem' {plannedEndTime} -> plannedEndTime) (\s@OpsItem' {} a -> s {plannedEndTime = a} :: OpsItem) Core.. Lens.mapping Core._Time

-- | The severity of the OpsItem. Severity options range from 1 to 4.
opsItem_severity :: Lens.Lens' OpsItem (Core.Maybe Core.Text)
opsItem_severity = Lens.lens (\OpsItem' {severity} -> severity) (\s@OpsItem' {} a -> s {severity = a} :: OpsItem)

-- | The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
opsItem_actualStartTime :: Lens.Lens' OpsItem (Core.Maybe Core.UTCTime)
opsItem_actualStartTime = Lens.lens (\OpsItem' {actualStartTime} -> actualStartTime) (\s@OpsItem' {} a -> s {actualStartTime = a} :: OpsItem) Core.. Lens.mapping Core._Time

-- | An OpsItem category. Category options include: Availability, Cost,
-- Performance, Recovery, Security.
opsItem_category :: Lens.Lens' OpsItem (Core.Maybe Core.Text)
opsItem_category = Lens.lens (\OpsItem' {category} -> category) (\s@OpsItem' {} a -> s {category = a} :: OpsItem)

-- | Operational data is custom data that provides useful reference details
-- about the OpsItem. For example, you can specify log files, error
-- strings, license keys, troubleshooting tips, or other relevant data. You
-- enter operational data as key-value pairs. The key has a maximum length
-- of 128 characters. The value has a maximum size of 20 KB.
--
-- Operational data keys /can\'t/ begin with the following: amazon, aws,
-- amzn, ssm, \/amazon, \/aws, \/amzn, \/ssm.
--
-- You can choose to make the data searchable by other users in the account
-- or you can restrict search access. Searchable data means that all users
-- with access to the OpsItem Overview page (as provided by the
-- DescribeOpsItems API action) can view and search on the specified data.
-- Operational data that is not searchable is only viewable by users who
-- have access to the OpsItem (as provided by the GetOpsItem API action).
--
-- Use the @\/aws\/resources@ key in OperationalData to specify a related
-- resource in the request. Use the @\/aws\/automations@ key in
-- OperationalData to associate an Automation runbook with the OpsItem. To
-- view AWS CLI example commands that use these keys, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually>
-- in the /AWS Systems Manager User Guide/.
opsItem_operationalData :: Lens.Lens' OpsItem (Core.Maybe (Core.HashMap Core.Text OpsItemDataValue))
opsItem_operationalData = Lens.lens (\OpsItem' {operationalData} -> operationalData) (\s@OpsItem' {} a -> s {operationalData = a} :: OpsItem) Core.. Lens.mapping Lens._Coerce

-- | A short heading that describes the nature of the OpsItem and the
-- impacted resource.
opsItem_title :: Lens.Lens' OpsItem (Core.Maybe Core.Text)
opsItem_title = Lens.lens (\OpsItem' {title} -> title) (\s@OpsItem' {} a -> s {title = a} :: OpsItem)

-- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager. The
-- impacted resource is a subset of source.
opsItem_source :: Lens.Lens' OpsItem (Core.Maybe Core.Text)
opsItem_source = Lens.lens (\OpsItem' {source} -> source) (\s@OpsItem' {} a -> s {source = a} :: OpsItem)

-- | The date and time the OpsItem was created.
opsItem_createdTime :: Lens.Lens' OpsItem (Core.Maybe Core.UTCTime)
opsItem_createdTime = Lens.lens (\OpsItem' {createdTime} -> createdTime) (\s@OpsItem' {} a -> s {createdTime = a} :: OpsItem) Core.. Lens.mapping Core._Time

-- | The version of this OpsItem. Each time the OpsItem is edited the version
-- number increments by one.
opsItem_version :: Lens.Lens' OpsItem (Core.Maybe Core.Text)
opsItem_version = Lens.lens (\OpsItem' {version} -> version) (\s@OpsItem' {} a -> s {version = a} :: OpsItem)

-- | The importance of this OpsItem in relation to other OpsItems in the
-- system.
opsItem_priority :: Lens.Lens' OpsItem (Core.Maybe Core.Natural)
opsItem_priority = Lens.lens (\OpsItem' {priority} -> priority) (\s@OpsItem' {} a -> s {priority = a} :: OpsItem)

-- | The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
opsItem_actualEndTime :: Lens.Lens' OpsItem (Core.Maybe Core.UTCTime)
opsItem_actualEndTime = Lens.lens (\OpsItem' {actualEndTime} -> actualEndTime) (\s@OpsItem' {} a -> s {actualEndTime = a} :: OpsItem) Core.. Lens.mapping Core._Time

-- | The ID of the OpsItem.
opsItem_opsItemId :: Lens.Lens' OpsItem (Core.Maybe Core.Text)
opsItem_opsItemId = Lens.lens (\OpsItem' {opsItemId} -> opsItemId) (\s@OpsItem' {} a -> s {opsItemId = a} :: OpsItem)

-- | The type of OpsItem. Currently, the only valid values are
-- @\/aws\/changerequest@ and @\/aws\/issue@.
opsItem_opsItemType :: Lens.Lens' OpsItem (Core.Maybe Core.Text)
opsItem_opsItemType = Lens.lens (\OpsItem' {opsItemType} -> opsItemType) (\s@OpsItem' {} a -> s {opsItemType = a} :: OpsItem)

-- | The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
opsItem_plannedStartTime :: Lens.Lens' OpsItem (Core.Maybe Core.UTCTime)
opsItem_plannedStartTime = Lens.lens (\OpsItem' {plannedStartTime} -> plannedStartTime) (\s@OpsItem' {} a -> s {plannedStartTime = a} :: OpsItem) Core.. Lens.mapping Core._Time

-- | The date and time the OpsItem was last updated.
opsItem_lastModifiedTime :: Lens.Lens' OpsItem (Core.Maybe Core.UTCTime)
opsItem_lastModifiedTime = Lens.lens (\OpsItem' {lastModifiedTime} -> lastModifiedTime) (\s@OpsItem' {} a -> s {lastModifiedTime = a} :: OpsItem) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
opsItem_notifications :: Lens.Lens' OpsItem (Core.Maybe [OpsItemNotification])
opsItem_notifications = Lens.lens (\OpsItem' {notifications} -> notifications) (\s@OpsItem' {} a -> s {notifications = a} :: OpsItem) Core.. Lens.mapping Lens._Coerce

-- | The OpsItem description.
opsItem_description :: Lens.Lens' OpsItem (Core.Maybe Core.Text)
opsItem_description = Lens.lens (\OpsItem' {description} -> description) (\s@OpsItem' {} a -> s {description = a} :: OpsItem)

-- | The ARN of the AWS account that created the OpsItem.
opsItem_createdBy :: Lens.Lens' OpsItem (Core.Maybe Core.Text)
opsItem_createdBy = Lens.lens (\OpsItem' {createdBy} -> createdBy) (\s@OpsItem' {} a -> s {createdBy = a} :: OpsItem)

-- | The ARN of the AWS account that last updated the OpsItem.
opsItem_lastModifiedBy :: Lens.Lens' OpsItem (Core.Maybe Core.Text)
opsItem_lastModifiedBy = Lens.lens (\OpsItem' {lastModifiedBy} -> lastModifiedBy) (\s@OpsItem' {} a -> s {lastModifiedBy = a} :: OpsItem)

-- | One or more OpsItems that share something in common with the current
-- OpsItem. For example, related OpsItems can include OpsItems with similar
-- error messages, impacted resources, or statuses for the impacted
-- resource.
opsItem_relatedOpsItems :: Lens.Lens' OpsItem (Core.Maybe [RelatedOpsItem])
opsItem_relatedOpsItems = Lens.lens (\OpsItem' {relatedOpsItems} -> relatedOpsItems) (\s@OpsItem' {} a -> s {relatedOpsItems = a} :: OpsItem) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON OpsItem where
  parseJSON =
    Core.withObject
      "OpsItem"
      ( \x ->
          OpsItem'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "PlannedEndTime")
            Core.<*> (x Core..:? "Severity")
            Core.<*> (x Core..:? "ActualStartTime")
            Core.<*> (x Core..:? "Category")
            Core.<*> (x Core..:? "OperationalData" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Title")
            Core.<*> (x Core..:? "Source")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "Priority")
            Core.<*> (x Core..:? "ActualEndTime")
            Core.<*> (x Core..:? "OpsItemId")
            Core.<*> (x Core..:? "OpsItemType")
            Core.<*> (x Core..:? "PlannedStartTime")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "Notifications" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..:? "RelatedOpsItems" Core..!= Core.mempty)
      )

instance Core.Hashable OpsItem

instance Core.NFData OpsItem
