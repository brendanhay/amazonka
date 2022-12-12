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
-- Module      : Amazonka.SSM.Types.OpsItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.OpsItemDataValue
import Amazonka.SSM.Types.OpsItemNotification
import Amazonka.SSM.Types.OpsItemStatus
import Amazonka.SSM.Types.RelatedOpsItem

-- | Operations engineers and IT professionals use Amazon Web Services
-- Systems Manager OpsCenter to view, investigate, and remediate
-- operational work items (OpsItems) impacting the performance and health
-- of their Amazon Web Services resources. OpsCenter is integrated with
-- Amazon EventBridge and Amazon CloudWatch. This means you can configure
-- these services to automatically create an OpsItem in OpsCenter when a
-- CloudWatch alarm enters the ALARM state or when EventBridge processes an
-- event from any Amazon Web Services service that publishes events.
-- Configuring Amazon CloudWatch alarms and EventBridge events to
-- automatically create OpsItems allows you to quickly diagnose and
-- remediate issues with Amazon Web Services resources from a single
-- console.
--
-- To help you diagnose issues, each OpsItem includes contextually relevant
-- information such as the name and ID of the Amazon Web Services resource
-- that generated the OpsItem, alarm or event details, alarm history, and
-- an alarm timeline graph. For the Amazon Web Services resource, OpsCenter
-- aggregates information from Config, CloudTrail logs, and EventBridge, so
-- you don\'t have to navigate across multiple console pages during your
-- investigation. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html OpsCenter>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- /See:/ 'newOpsItem' smart constructor.
data OpsItem = OpsItem'
  { -- | The time a runbook workflow ended. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualEndTime :: Prelude.Maybe Data.POSIX,
    -- | The time a runbook workflow started. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualStartTime :: Prelude.Maybe Data.POSIX,
    -- | An OpsItem category. Category options include: Availability, Cost,
    -- Performance, Recovery, Security.
    category :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon Web Services account that created the OpsItem.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time the OpsItem was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The OpsItem description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon Web Services account that last updated the
    -- OpsItem.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time the OpsItem was last updated.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of an Amazon Simple Notification Service
    -- (Amazon SNS) topic where notifications are sent when this OpsItem is
    -- edited or changed.
    notifications :: Prelude.Maybe [OpsItemNotification],
    -- | Operational data is custom data that provides useful reference details
    -- about the OpsItem. For example, you can specify log files, error
    -- strings, license keys, troubleshooting tips, or other relevant data. You
    -- enter operational data as key-value pairs. The key has a maximum length
    -- of 128 characters. The value has a maximum size of 20 KB.
    --
    -- Operational data keys /can\'t/ begin with the following: @amazon@,
    -- @aws@, @amzn@, @ssm@, @\/amazon@, @\/aws@, @\/amzn@, @\/ssm@.
    --
    -- You can choose to make the data searchable by other users in the account
    -- or you can restrict search access. Searchable data means that all users
    -- with access to the OpsItem Overview page (as provided by the
    -- DescribeOpsItems API operation) can view and search on the specified
    -- data. Operational data that isn\'t searchable is only viewable by users
    -- who have access to the OpsItem (as provided by the GetOpsItem API
    -- operation).
    --
    -- Use the @\/aws\/resources@ key in OperationalData to specify a related
    -- resource in the request. Use the @\/aws\/automations@ key in
    -- OperationalData to associate an Automation runbook with the OpsItem. To
    -- view Amazon Web Services CLI example commands that use these keys, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    operationalData :: Prelude.Maybe (Prelude.HashMap Prelude.Text OpsItemDataValue),
    -- | The OpsItem Amazon Resource Name (ARN).
    opsItemArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the OpsItem.
    opsItemId :: Prelude.Maybe Prelude.Text,
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
    -- | The time specified in a change request for a runbook workflow to end.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedEndTime :: Prelude.Maybe Data.POSIX,
    -- | The time specified in a change request for a runbook workflow to start.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedStartTime :: Prelude.Maybe Data.POSIX,
    -- | The importance of this OpsItem in relation to other OpsItems in the
    -- system.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | One or more OpsItems that share something in common with the current
    -- OpsItem. For example, related OpsItems can include OpsItems with similar
    -- error messages, impacted resources, or statuses for the impacted
    -- resource.
    relatedOpsItems :: Prelude.Maybe [RelatedOpsItem],
    -- | The severity of the OpsItem. Severity options range from 1 to 4.
    severity :: Prelude.Maybe Prelude.Text,
    -- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager. The
    -- impacted resource is a subset of source.
    source :: Prelude.Maybe Prelude.Text,
    -- | The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems-editing-details.html Editing OpsItem details>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    status :: Prelude.Maybe OpsItemStatus,
    -- | A short heading that describes the nature of the OpsItem and the
    -- impacted resource.
    title :: Prelude.Maybe Prelude.Text,
    -- | The version of this OpsItem. Each time the OpsItem is edited the version
    -- number increments by one.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actualEndTime', 'opsItem_actualEndTime' - The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'actualStartTime', 'opsItem_actualStartTime' - The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'category', 'opsItem_category' - An OpsItem category. Category options include: Availability, Cost,
-- Performance, Recovery, Security.
--
-- 'createdBy', 'opsItem_createdBy' - The ARN of the Amazon Web Services account that created the OpsItem.
--
-- 'createdTime', 'opsItem_createdTime' - The date and time the OpsItem was created.
--
-- 'description', 'opsItem_description' - The OpsItem description.
--
-- 'lastModifiedBy', 'opsItem_lastModifiedBy' - The ARN of the Amazon Web Services account that last updated the
-- OpsItem.
--
-- 'lastModifiedTime', 'opsItem_lastModifiedTime' - The date and time the OpsItem was last updated.
--
-- 'notifications', 'opsItem_notifications' - The Amazon Resource Name (ARN) of an Amazon Simple Notification Service
-- (Amazon SNS) topic where notifications are sent when this OpsItem is
-- edited or changed.
--
-- 'operationalData', 'opsItem_operationalData' - Operational data is custom data that provides useful reference details
-- about the OpsItem. For example, you can specify log files, error
-- strings, license keys, troubleshooting tips, or other relevant data. You
-- enter operational data as key-value pairs. The key has a maximum length
-- of 128 characters. The value has a maximum size of 20 KB.
--
-- Operational data keys /can\'t/ begin with the following: @amazon@,
-- @aws@, @amzn@, @ssm@, @\/amazon@, @\/aws@, @\/amzn@, @\/ssm@.
--
-- You can choose to make the data searchable by other users in the account
-- or you can restrict search access. Searchable data means that all users
-- with access to the OpsItem Overview page (as provided by the
-- DescribeOpsItems API operation) can view and search on the specified
-- data. Operational data that isn\'t searchable is only viewable by users
-- who have access to the OpsItem (as provided by the GetOpsItem API
-- operation).
--
-- Use the @\/aws\/resources@ key in OperationalData to specify a related
-- resource in the request. Use the @\/aws\/automations@ key in
-- OperationalData to associate an Automation runbook with the OpsItem. To
-- view Amazon Web Services CLI example commands that use these keys, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'opsItemArn', 'opsItem_opsItemArn' - The OpsItem Amazon Resource Name (ARN).
--
-- 'opsItemId', 'opsItem_opsItemId' - The ID of the OpsItem.
--
-- 'opsItemType', 'opsItem_opsItemType' - The type of OpsItem. Systems Manager supports the following types of
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
-- 'plannedEndTime', 'opsItem_plannedEndTime' - The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'plannedStartTime', 'opsItem_plannedStartTime' - The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'priority', 'opsItem_priority' - The importance of this OpsItem in relation to other OpsItems in the
-- system.
--
-- 'relatedOpsItems', 'opsItem_relatedOpsItems' - One or more OpsItems that share something in common with the current
-- OpsItem. For example, related OpsItems can include OpsItems with similar
-- error messages, impacted resources, or statuses for the impacted
-- resource.
--
-- 'severity', 'opsItem_severity' - The severity of the OpsItem. Severity options range from 1 to 4.
--
-- 'source', 'opsItem_source' - The origin of the OpsItem, such as Amazon EC2 or Systems Manager. The
-- impacted resource is a subset of source.
--
-- 'status', 'opsItem_status' - The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
-- For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems-editing-details.html Editing OpsItem details>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'title', 'opsItem_title' - A short heading that describes the nature of the OpsItem and the
-- impacted resource.
--
-- 'version', 'opsItem_version' - The version of this OpsItem. Each time the OpsItem is edited the version
-- number increments by one.
newOpsItem ::
  OpsItem
newOpsItem =
  OpsItem'
    { actualEndTime = Prelude.Nothing,
      actualStartTime = Prelude.Nothing,
      category = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      notifications = Prelude.Nothing,
      operationalData = Prelude.Nothing,
      opsItemArn = Prelude.Nothing,
      opsItemId = Prelude.Nothing,
      opsItemType = Prelude.Nothing,
      plannedEndTime = Prelude.Nothing,
      plannedStartTime = Prelude.Nothing,
      priority = Prelude.Nothing,
      relatedOpsItems = Prelude.Nothing,
      severity = Prelude.Nothing,
      source = Prelude.Nothing,
      status = Prelude.Nothing,
      title = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
opsItem_actualEndTime :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.UTCTime)
opsItem_actualEndTime = Lens.lens (\OpsItem' {actualEndTime} -> actualEndTime) (\s@OpsItem' {} a -> s {actualEndTime = a} :: OpsItem) Prelude.. Lens.mapping Data._Time

-- | The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
opsItem_actualStartTime :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.UTCTime)
opsItem_actualStartTime = Lens.lens (\OpsItem' {actualStartTime} -> actualStartTime) (\s@OpsItem' {} a -> s {actualStartTime = a} :: OpsItem) Prelude.. Lens.mapping Data._Time

-- | An OpsItem category. Category options include: Availability, Cost,
-- Performance, Recovery, Security.
opsItem_category :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.Text)
opsItem_category = Lens.lens (\OpsItem' {category} -> category) (\s@OpsItem' {} a -> s {category = a} :: OpsItem)

-- | The ARN of the Amazon Web Services account that created the OpsItem.
opsItem_createdBy :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.Text)
opsItem_createdBy = Lens.lens (\OpsItem' {createdBy} -> createdBy) (\s@OpsItem' {} a -> s {createdBy = a} :: OpsItem)

-- | The date and time the OpsItem was created.
opsItem_createdTime :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.UTCTime)
opsItem_createdTime = Lens.lens (\OpsItem' {createdTime} -> createdTime) (\s@OpsItem' {} a -> s {createdTime = a} :: OpsItem) Prelude.. Lens.mapping Data._Time

-- | The OpsItem description.
opsItem_description :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.Text)
opsItem_description = Lens.lens (\OpsItem' {description} -> description) (\s@OpsItem' {} a -> s {description = a} :: OpsItem)

-- | The ARN of the Amazon Web Services account that last updated the
-- OpsItem.
opsItem_lastModifiedBy :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.Text)
opsItem_lastModifiedBy = Lens.lens (\OpsItem' {lastModifiedBy} -> lastModifiedBy) (\s@OpsItem' {} a -> s {lastModifiedBy = a} :: OpsItem)

-- | The date and time the OpsItem was last updated.
opsItem_lastModifiedTime :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.UTCTime)
opsItem_lastModifiedTime = Lens.lens (\OpsItem' {lastModifiedTime} -> lastModifiedTime) (\s@OpsItem' {} a -> s {lastModifiedTime = a} :: OpsItem) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of an Amazon Simple Notification Service
-- (Amazon SNS) topic where notifications are sent when this OpsItem is
-- edited or changed.
opsItem_notifications :: Lens.Lens' OpsItem (Prelude.Maybe [OpsItemNotification])
opsItem_notifications = Lens.lens (\OpsItem' {notifications} -> notifications) (\s@OpsItem' {} a -> s {notifications = a} :: OpsItem) Prelude.. Lens.mapping Lens.coerced

-- | Operational data is custom data that provides useful reference details
-- about the OpsItem. For example, you can specify log files, error
-- strings, license keys, troubleshooting tips, or other relevant data. You
-- enter operational data as key-value pairs. The key has a maximum length
-- of 128 characters. The value has a maximum size of 20 KB.
--
-- Operational data keys /can\'t/ begin with the following: @amazon@,
-- @aws@, @amzn@, @ssm@, @\/amazon@, @\/aws@, @\/amzn@, @\/ssm@.
--
-- You can choose to make the data searchable by other users in the account
-- or you can restrict search access. Searchable data means that all users
-- with access to the OpsItem Overview page (as provided by the
-- DescribeOpsItems API operation) can view and search on the specified
-- data. Operational data that isn\'t searchable is only viewable by users
-- who have access to the OpsItem (as provided by the GetOpsItem API
-- operation).
--
-- Use the @\/aws\/resources@ key in OperationalData to specify a related
-- resource in the request. Use the @\/aws\/automations@ key in
-- OperationalData to associate an Automation runbook with the OpsItem. To
-- view Amazon Web Services CLI example commands that use these keys, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually>
-- in the /Amazon Web Services Systems Manager User Guide/.
opsItem_operationalData :: Lens.Lens' OpsItem (Prelude.Maybe (Prelude.HashMap Prelude.Text OpsItemDataValue))
opsItem_operationalData = Lens.lens (\OpsItem' {operationalData} -> operationalData) (\s@OpsItem' {} a -> s {operationalData = a} :: OpsItem) Prelude.. Lens.mapping Lens.coerced

-- | The OpsItem Amazon Resource Name (ARN).
opsItem_opsItemArn :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.Text)
opsItem_opsItemArn = Lens.lens (\OpsItem' {opsItemArn} -> opsItemArn) (\s@OpsItem' {} a -> s {opsItemArn = a} :: OpsItem)

-- | The ID of the OpsItem.
opsItem_opsItemId :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.Text)
opsItem_opsItemId = Lens.lens (\OpsItem' {opsItemId} -> opsItemId) (\s@OpsItem' {} a -> s {opsItemId = a} :: OpsItem)

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
opsItem_opsItemType :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.Text)
opsItem_opsItemType = Lens.lens (\OpsItem' {opsItemType} -> opsItemType) (\s@OpsItem' {} a -> s {opsItemType = a} :: OpsItem)

-- | The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
opsItem_plannedEndTime :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.UTCTime)
opsItem_plannedEndTime = Lens.lens (\OpsItem' {plannedEndTime} -> plannedEndTime) (\s@OpsItem' {} a -> s {plannedEndTime = a} :: OpsItem) Prelude.. Lens.mapping Data._Time

-- | The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
opsItem_plannedStartTime :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.UTCTime)
opsItem_plannedStartTime = Lens.lens (\OpsItem' {plannedStartTime} -> plannedStartTime) (\s@OpsItem' {} a -> s {plannedStartTime = a} :: OpsItem) Prelude.. Lens.mapping Data._Time

-- | The importance of this OpsItem in relation to other OpsItems in the
-- system.
opsItem_priority :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.Natural)
opsItem_priority = Lens.lens (\OpsItem' {priority} -> priority) (\s@OpsItem' {} a -> s {priority = a} :: OpsItem)

-- | One or more OpsItems that share something in common with the current
-- OpsItem. For example, related OpsItems can include OpsItems with similar
-- error messages, impacted resources, or statuses for the impacted
-- resource.
opsItem_relatedOpsItems :: Lens.Lens' OpsItem (Prelude.Maybe [RelatedOpsItem])
opsItem_relatedOpsItems = Lens.lens (\OpsItem' {relatedOpsItems} -> relatedOpsItems) (\s@OpsItem' {} a -> s {relatedOpsItems = a} :: OpsItem) Prelude.. Lens.mapping Lens.coerced

-- | The severity of the OpsItem. Severity options range from 1 to 4.
opsItem_severity :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.Text)
opsItem_severity = Lens.lens (\OpsItem' {severity} -> severity) (\s@OpsItem' {} a -> s {severity = a} :: OpsItem)

-- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager. The
-- impacted resource is a subset of source.
opsItem_source :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.Text)
opsItem_source = Lens.lens (\OpsItem' {source} -> source) (\s@OpsItem' {} a -> s {source = a} :: OpsItem)

-- | The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
-- For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems-editing-details.html Editing OpsItem details>
-- in the /Amazon Web Services Systems Manager User Guide/.
opsItem_status :: Lens.Lens' OpsItem (Prelude.Maybe OpsItemStatus)
opsItem_status = Lens.lens (\OpsItem' {status} -> status) (\s@OpsItem' {} a -> s {status = a} :: OpsItem)

-- | A short heading that describes the nature of the OpsItem and the
-- impacted resource.
opsItem_title :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.Text)
opsItem_title = Lens.lens (\OpsItem' {title} -> title) (\s@OpsItem' {} a -> s {title = a} :: OpsItem)

-- | The version of this OpsItem. Each time the OpsItem is edited the version
-- number increments by one.
opsItem_version :: Lens.Lens' OpsItem (Prelude.Maybe Prelude.Text)
opsItem_version = Lens.lens (\OpsItem' {version} -> version) (\s@OpsItem' {} a -> s {version = a} :: OpsItem)

instance Data.FromJSON OpsItem where
  parseJSON =
    Data.withObject
      "OpsItem"
      ( \x ->
          OpsItem'
            Prelude.<$> (x Data..:? "ActualEndTime")
            Prelude.<*> (x Data..:? "ActualStartTime")
            Prelude.<*> (x Data..:? "Category")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Notifications" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "OperationalData"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OpsItemArn")
            Prelude.<*> (x Data..:? "OpsItemId")
            Prelude.<*> (x Data..:? "OpsItemType")
            Prelude.<*> (x Data..:? "PlannedEndTime")
            Prelude.<*> (x Data..:? "PlannedStartTime")
            Prelude.<*> (x Data..:? "Priority")
            Prelude.<*> ( x Data..:? "RelatedOpsItems"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Severity")
            Prelude.<*> (x Data..:? "Source")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable OpsItem where
  hashWithSalt _salt OpsItem' {..} =
    _salt `Prelude.hashWithSalt` actualEndTime
      `Prelude.hashWithSalt` actualStartTime
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` notifications
      `Prelude.hashWithSalt` operationalData
      `Prelude.hashWithSalt` opsItemArn
      `Prelude.hashWithSalt` opsItemId
      `Prelude.hashWithSalt` opsItemType
      `Prelude.hashWithSalt` plannedEndTime
      `Prelude.hashWithSalt` plannedStartTime
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` relatedOpsItems
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` version

instance Prelude.NFData OpsItem where
  rnf OpsItem' {..} =
    Prelude.rnf actualEndTime
      `Prelude.seq` Prelude.rnf actualStartTime
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf notifications
      `Prelude.seq` Prelude.rnf operationalData
      `Prelude.seq` Prelude.rnf opsItemArn
      `Prelude.seq` Prelude.rnf opsItemId
      `Prelude.seq` Prelude.rnf opsItemType
      `Prelude.seq` Prelude.rnf plannedEndTime
      `Prelude.seq` Prelude.rnf plannedStartTime
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf relatedOpsItems
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf version
