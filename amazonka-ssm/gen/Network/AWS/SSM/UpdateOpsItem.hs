{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateOpsItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Edit or change an OpsItem. You must have permission in AWS Identity and
-- Access Management (IAM) to update an OpsItem. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter>
-- in the /AWS Systems Manager User Guide/.
--
-- Operations engineers and IT professionals use OpsCenter to view,
-- investigate, and remediate operational issues impacting the performance
-- and health of their AWS resources. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter>
-- in the /AWS Systems Manager User Guide/.
module Network.AWS.SSM.UpdateOpsItem
  ( -- * Creating a Request
    UpdateOpsItem (..),
    newUpdateOpsItem,

    -- * Request Lenses
    updateOpsItem_status,
    updateOpsItem_plannedEndTime,
    updateOpsItem_severity,
    updateOpsItem_actualStartTime,
    updateOpsItem_category,
    updateOpsItem_operationalData,
    updateOpsItem_title,
    updateOpsItem_priority,
    updateOpsItem_actualEndTime,
    updateOpsItem_plannedStartTime,
    updateOpsItem_notifications,
    updateOpsItem_description,
    updateOpsItem_relatedOpsItems,
    updateOpsItem_operationalDataToDelete,
    updateOpsItem_opsItemId,

    -- * Destructuring the Response
    UpdateOpsItemResponse (..),
    newUpdateOpsItemResponse,

    -- * Response Lenses
    updateOpsItemResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdateOpsItem' smart constructor.
data UpdateOpsItem = UpdateOpsItem'
  { -- | The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details>
    -- in the /AWS Systems Manager User Guide/.
    status :: Prelude.Maybe OpsItemStatus,
    -- | The time specified in a change request for a runbook workflow to end.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedEndTime :: Prelude.Maybe Core.POSIX,
    -- | Specify a new severity for an OpsItem.
    severity :: Prelude.Maybe Prelude.Text,
    -- | The time a runbook workflow started. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualStartTime :: Prelude.Maybe Core.POSIX,
    -- | Specify a new category for an OpsItem.
    category :: Prelude.Maybe Prelude.Text,
    -- | Add new keys or edit existing key-value pairs of the OperationalData map
    -- in the OpsItem object.
    --
    -- Operational data is custom data that provides useful reference details
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
    operationalData :: Prelude.Maybe (Prelude.HashMap Prelude.Text OpsItemDataValue),
    -- | A short heading that describes the nature of the OpsItem and the
    -- impacted resource.
    title :: Prelude.Maybe Prelude.Text,
    -- | The importance of this OpsItem in relation to other OpsItems in the
    -- system.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The time a runbook workflow ended. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualEndTime :: Prelude.Maybe Core.POSIX,
    -- | The time specified in a change request for a runbook workflow to start.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedStartTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
    -- sent when this OpsItem is edited or changed.
    notifications :: Prelude.Maybe [OpsItemNotification],
    -- | Update the information about the OpsItem. Provide enough information so
    -- that users reading this OpsItem for the first time understand the issue.
    description :: Prelude.Maybe Prelude.Text,
    -- | One or more OpsItems that share something in common with the current
    -- OpsItems. For example, related OpsItems can include OpsItems with
    -- similar error messages, impacted resources, or statuses for the impacted
    -- resource.
    relatedOpsItems :: Prelude.Maybe [RelatedOpsItem],
    -- | Keys that you want to remove from the OperationalData map.
    operationalDataToDelete :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the OpsItem.
    opsItemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOpsItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'updateOpsItem_status' - The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
-- For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details>
-- in the /AWS Systems Manager User Guide/.
--
-- 'plannedEndTime', 'updateOpsItem_plannedEndTime' - The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'severity', 'updateOpsItem_severity' - Specify a new severity for an OpsItem.
--
-- 'actualStartTime', 'updateOpsItem_actualStartTime' - The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'category', 'updateOpsItem_category' - Specify a new category for an OpsItem.
--
-- 'operationalData', 'updateOpsItem_operationalData' - Add new keys or edit existing key-value pairs of the OperationalData map
-- in the OpsItem object.
--
-- Operational data is custom data that provides useful reference details
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
-- 'title', 'updateOpsItem_title' - A short heading that describes the nature of the OpsItem and the
-- impacted resource.
--
-- 'priority', 'updateOpsItem_priority' - The importance of this OpsItem in relation to other OpsItems in the
-- system.
--
-- 'actualEndTime', 'updateOpsItem_actualEndTime' - The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'plannedStartTime', 'updateOpsItem_plannedStartTime' - The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'notifications', 'updateOpsItem_notifications' - The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
--
-- 'description', 'updateOpsItem_description' - Update the information about the OpsItem. Provide enough information so
-- that users reading this OpsItem for the first time understand the issue.
--
-- 'relatedOpsItems', 'updateOpsItem_relatedOpsItems' - One or more OpsItems that share something in common with the current
-- OpsItems. For example, related OpsItems can include OpsItems with
-- similar error messages, impacted resources, or statuses for the impacted
-- resource.
--
-- 'operationalDataToDelete', 'updateOpsItem_operationalDataToDelete' - Keys that you want to remove from the OperationalData map.
--
-- 'opsItemId', 'updateOpsItem_opsItemId' - The ID of the OpsItem.
newUpdateOpsItem ::
  -- | 'opsItemId'
  Prelude.Text ->
  UpdateOpsItem
newUpdateOpsItem pOpsItemId_ =
  UpdateOpsItem'
    { status = Prelude.Nothing,
      plannedEndTime = Prelude.Nothing,
      severity = Prelude.Nothing,
      actualStartTime = Prelude.Nothing,
      category = Prelude.Nothing,
      operationalData = Prelude.Nothing,
      title = Prelude.Nothing,
      priority = Prelude.Nothing,
      actualEndTime = Prelude.Nothing,
      plannedStartTime = Prelude.Nothing,
      notifications = Prelude.Nothing,
      description = Prelude.Nothing,
      relatedOpsItems = Prelude.Nothing,
      operationalDataToDelete = Prelude.Nothing,
      opsItemId = pOpsItemId_
    }

-- | The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
-- For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details>
-- in the /AWS Systems Manager User Guide/.
updateOpsItem_status :: Lens.Lens' UpdateOpsItem (Prelude.Maybe OpsItemStatus)
updateOpsItem_status = Lens.lens (\UpdateOpsItem' {status} -> status) (\s@UpdateOpsItem' {} a -> s {status = a} :: UpdateOpsItem)

-- | The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
updateOpsItem_plannedEndTime :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.UTCTime)
updateOpsItem_plannedEndTime = Lens.lens (\UpdateOpsItem' {plannedEndTime} -> plannedEndTime) (\s@UpdateOpsItem' {} a -> s {plannedEndTime = a} :: UpdateOpsItem) Prelude.. Lens.mapping Core._Time

-- | Specify a new severity for an OpsItem.
updateOpsItem_severity :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.Text)
updateOpsItem_severity = Lens.lens (\UpdateOpsItem' {severity} -> severity) (\s@UpdateOpsItem' {} a -> s {severity = a} :: UpdateOpsItem)

-- | The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
updateOpsItem_actualStartTime :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.UTCTime)
updateOpsItem_actualStartTime = Lens.lens (\UpdateOpsItem' {actualStartTime} -> actualStartTime) (\s@UpdateOpsItem' {} a -> s {actualStartTime = a} :: UpdateOpsItem) Prelude.. Lens.mapping Core._Time

-- | Specify a new category for an OpsItem.
updateOpsItem_category :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.Text)
updateOpsItem_category = Lens.lens (\UpdateOpsItem' {category} -> category) (\s@UpdateOpsItem' {} a -> s {category = a} :: UpdateOpsItem)

-- | Add new keys or edit existing key-value pairs of the OperationalData map
-- in the OpsItem object.
--
-- Operational data is custom data that provides useful reference details
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
updateOpsItem_operationalData :: Lens.Lens' UpdateOpsItem (Prelude.Maybe (Prelude.HashMap Prelude.Text OpsItemDataValue))
updateOpsItem_operationalData = Lens.lens (\UpdateOpsItem' {operationalData} -> operationalData) (\s@UpdateOpsItem' {} a -> s {operationalData = a} :: UpdateOpsItem) Prelude.. Lens.mapping Lens._Coerce

-- | A short heading that describes the nature of the OpsItem and the
-- impacted resource.
updateOpsItem_title :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.Text)
updateOpsItem_title = Lens.lens (\UpdateOpsItem' {title} -> title) (\s@UpdateOpsItem' {} a -> s {title = a} :: UpdateOpsItem)

-- | The importance of this OpsItem in relation to other OpsItems in the
-- system.
updateOpsItem_priority :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.Natural)
updateOpsItem_priority = Lens.lens (\UpdateOpsItem' {priority} -> priority) (\s@UpdateOpsItem' {} a -> s {priority = a} :: UpdateOpsItem)

-- | The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
updateOpsItem_actualEndTime :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.UTCTime)
updateOpsItem_actualEndTime = Lens.lens (\UpdateOpsItem' {actualEndTime} -> actualEndTime) (\s@UpdateOpsItem' {} a -> s {actualEndTime = a} :: UpdateOpsItem) Prelude.. Lens.mapping Core._Time

-- | The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
updateOpsItem_plannedStartTime :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.UTCTime)
updateOpsItem_plannedStartTime = Lens.lens (\UpdateOpsItem' {plannedStartTime} -> plannedStartTime) (\s@UpdateOpsItem' {} a -> s {plannedStartTime = a} :: UpdateOpsItem) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
updateOpsItem_notifications :: Lens.Lens' UpdateOpsItem (Prelude.Maybe [OpsItemNotification])
updateOpsItem_notifications = Lens.lens (\UpdateOpsItem' {notifications} -> notifications) (\s@UpdateOpsItem' {} a -> s {notifications = a} :: UpdateOpsItem) Prelude.. Lens.mapping Lens._Coerce

-- | Update the information about the OpsItem. Provide enough information so
-- that users reading this OpsItem for the first time understand the issue.
updateOpsItem_description :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.Text)
updateOpsItem_description = Lens.lens (\UpdateOpsItem' {description} -> description) (\s@UpdateOpsItem' {} a -> s {description = a} :: UpdateOpsItem)

-- | One or more OpsItems that share something in common with the current
-- OpsItems. For example, related OpsItems can include OpsItems with
-- similar error messages, impacted resources, or statuses for the impacted
-- resource.
updateOpsItem_relatedOpsItems :: Lens.Lens' UpdateOpsItem (Prelude.Maybe [RelatedOpsItem])
updateOpsItem_relatedOpsItems = Lens.lens (\UpdateOpsItem' {relatedOpsItems} -> relatedOpsItems) (\s@UpdateOpsItem' {} a -> s {relatedOpsItems = a} :: UpdateOpsItem) Prelude.. Lens.mapping Lens._Coerce

-- | Keys that you want to remove from the OperationalData map.
updateOpsItem_operationalDataToDelete :: Lens.Lens' UpdateOpsItem (Prelude.Maybe [Prelude.Text])
updateOpsItem_operationalDataToDelete = Lens.lens (\UpdateOpsItem' {operationalDataToDelete} -> operationalDataToDelete) (\s@UpdateOpsItem' {} a -> s {operationalDataToDelete = a} :: UpdateOpsItem) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the OpsItem.
updateOpsItem_opsItemId :: Lens.Lens' UpdateOpsItem Prelude.Text
updateOpsItem_opsItemId = Lens.lens (\UpdateOpsItem' {opsItemId} -> opsItemId) (\s@UpdateOpsItem' {} a -> s {opsItemId = a} :: UpdateOpsItem)

instance Core.AWSRequest UpdateOpsItem where
  type
    AWSResponse UpdateOpsItem =
      UpdateOpsItemResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateOpsItemResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateOpsItem

instance Prelude.NFData UpdateOpsItem

instance Core.ToHeaders UpdateOpsItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.UpdateOpsItem" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateOpsItem where
  toJSON UpdateOpsItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("PlannedEndTime" Core..=)
              Prelude.<$> plannedEndTime,
            ("Severity" Core..=) Prelude.<$> severity,
            ("ActualStartTime" Core..=)
              Prelude.<$> actualStartTime,
            ("Category" Core..=) Prelude.<$> category,
            ("OperationalData" Core..=)
              Prelude.<$> operationalData,
            ("Title" Core..=) Prelude.<$> title,
            ("Priority" Core..=) Prelude.<$> priority,
            ("ActualEndTime" Core..=) Prelude.<$> actualEndTime,
            ("PlannedStartTime" Core..=)
              Prelude.<$> plannedStartTime,
            ("Notifications" Core..=) Prelude.<$> notifications,
            ("Description" Core..=) Prelude.<$> description,
            ("RelatedOpsItems" Core..=)
              Prelude.<$> relatedOpsItems,
            ("OperationalDataToDelete" Core..=)
              Prelude.<$> operationalDataToDelete,
            Prelude.Just ("OpsItemId" Core..= opsItemId)
          ]
      )

instance Core.ToPath UpdateOpsItem where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateOpsItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateOpsItemResponse' smart constructor.
data UpdateOpsItemResponse = UpdateOpsItemResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOpsItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateOpsItemResponse_httpStatus' - The response's http status code.
newUpdateOpsItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateOpsItemResponse
newUpdateOpsItemResponse pHttpStatus_ =
  UpdateOpsItemResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateOpsItemResponse_httpStatus :: Lens.Lens' UpdateOpsItemResponse Prelude.Int
updateOpsItemResponse_httpStatus = Lens.lens (\UpdateOpsItemResponse' {httpStatus} -> httpStatus) (\s@UpdateOpsItemResponse' {} a -> s {httpStatus = a} :: UpdateOpsItemResponse)

instance Prelude.NFData UpdateOpsItemResponse
