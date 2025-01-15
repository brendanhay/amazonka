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
-- Module      : Amazonka.SSM.UpdateOpsItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Edit or change an OpsItem. You must have permission in Identity and
-- Access Management (IAM) to update an OpsItem. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- Operations engineers and IT professionals use Amazon Web Services
-- Systems Manager OpsCenter to view, investigate, and remediate
-- operational issues impacting the performance and health of their Amazon
-- Web Services resources. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html OpsCenter>
-- in the /Amazon Web Services Systems Manager User Guide/.
module Amazonka.SSM.UpdateOpsItem
  ( -- * Creating a Request
    UpdateOpsItem (..),
    newUpdateOpsItem,

    -- * Request Lenses
    updateOpsItem_actualEndTime,
    updateOpsItem_actualStartTime,
    updateOpsItem_category,
    updateOpsItem_description,
    updateOpsItem_notifications,
    updateOpsItem_operationalData,
    updateOpsItem_operationalDataToDelete,
    updateOpsItem_opsItemArn,
    updateOpsItem_plannedEndTime,
    updateOpsItem_plannedStartTime,
    updateOpsItem_priority,
    updateOpsItem_relatedOpsItems,
    updateOpsItem_severity,
    updateOpsItem_status,
    updateOpsItem_title,
    updateOpsItem_opsItemId,

    -- * Destructuring the Response
    UpdateOpsItemResponse (..),
    newUpdateOpsItemResponse,

    -- * Response Lenses
    updateOpsItemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateOpsItem' smart constructor.
data UpdateOpsItem = UpdateOpsItem'
  { -- | The time a runbook workflow ended. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualEndTime :: Prelude.Maybe Data.POSIX,
    -- | The time a runbook workflow started. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualStartTime :: Prelude.Maybe Data.POSIX,
    -- | Specify a new category for an OpsItem.
    category :: Prelude.Maybe Prelude.Text,
    -- | Update the information about the OpsItem. Provide enough information so
    -- that users reading this OpsItem for the first time understand the issue.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
    -- sent when this OpsItem is edited or changed.
    notifications :: Prelude.Maybe [OpsItemNotification],
    -- | Add new keys or edit existing key-value pairs of the OperationalData map
    -- in the OpsItem object.
    --
    -- Operational data is custom data that provides useful reference details
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
    -- | Keys that you want to remove from the OperationalData map.
    operationalDataToDelete :: Prelude.Maybe [Prelude.Text],
    -- | The OpsItem Amazon Resource Name (ARN).
    opsItemArn :: Prelude.Maybe Prelude.Text,
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
    -- OpsItems. For example, related OpsItems can include OpsItems with
    -- similar error messages, impacted resources, or statuses for the impacted
    -- resource.
    relatedOpsItems :: Prelude.Maybe [RelatedOpsItem],
    -- | Specify a new severity for an OpsItem.
    severity :: Prelude.Maybe Prelude.Text,
    -- | The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    status :: Prelude.Maybe OpsItemStatus,
    -- | A short heading that describes the nature of the OpsItem and the
    -- impacted resource.
    title :: Prelude.Maybe Prelude.Text,
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
-- 'actualEndTime', 'updateOpsItem_actualEndTime' - The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'actualStartTime', 'updateOpsItem_actualStartTime' - The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'category', 'updateOpsItem_category' - Specify a new category for an OpsItem.
--
-- 'description', 'updateOpsItem_description' - Update the information about the OpsItem. Provide enough information so
-- that users reading this OpsItem for the first time understand the issue.
--
-- 'notifications', 'updateOpsItem_notifications' - The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
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
-- 'operationalDataToDelete', 'updateOpsItem_operationalDataToDelete' - Keys that you want to remove from the OperationalData map.
--
-- 'opsItemArn', 'updateOpsItem_opsItemArn' - The OpsItem Amazon Resource Name (ARN).
--
-- 'plannedEndTime', 'updateOpsItem_plannedEndTime' - The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'plannedStartTime', 'updateOpsItem_plannedStartTime' - The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'priority', 'updateOpsItem_priority' - The importance of this OpsItem in relation to other OpsItems in the
-- system.
--
-- 'relatedOpsItems', 'updateOpsItem_relatedOpsItems' - One or more OpsItems that share something in common with the current
-- OpsItems. For example, related OpsItems can include OpsItems with
-- similar error messages, impacted resources, or statuses for the impacted
-- resource.
--
-- 'severity', 'updateOpsItem_severity' - Specify a new severity for an OpsItem.
--
-- 'status', 'updateOpsItem_status' - The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
-- For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'title', 'updateOpsItem_title' - A short heading that describes the nature of the OpsItem and the
-- impacted resource.
--
-- 'opsItemId', 'updateOpsItem_opsItemId' - The ID of the OpsItem.
newUpdateOpsItem ::
  -- | 'opsItemId'
  Prelude.Text ->
  UpdateOpsItem
newUpdateOpsItem pOpsItemId_ =
  UpdateOpsItem'
    { actualEndTime = Prelude.Nothing,
      actualStartTime = Prelude.Nothing,
      category = Prelude.Nothing,
      description = Prelude.Nothing,
      notifications = Prelude.Nothing,
      operationalData = Prelude.Nothing,
      operationalDataToDelete = Prelude.Nothing,
      opsItemArn = Prelude.Nothing,
      plannedEndTime = Prelude.Nothing,
      plannedStartTime = Prelude.Nothing,
      priority = Prelude.Nothing,
      relatedOpsItems = Prelude.Nothing,
      severity = Prelude.Nothing,
      status = Prelude.Nothing,
      title = Prelude.Nothing,
      opsItemId = pOpsItemId_
    }

-- | The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
updateOpsItem_actualEndTime :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.UTCTime)
updateOpsItem_actualEndTime = Lens.lens (\UpdateOpsItem' {actualEndTime} -> actualEndTime) (\s@UpdateOpsItem' {} a -> s {actualEndTime = a} :: UpdateOpsItem) Prelude.. Lens.mapping Data._Time

-- | The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
updateOpsItem_actualStartTime :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.UTCTime)
updateOpsItem_actualStartTime = Lens.lens (\UpdateOpsItem' {actualStartTime} -> actualStartTime) (\s@UpdateOpsItem' {} a -> s {actualStartTime = a} :: UpdateOpsItem) Prelude.. Lens.mapping Data._Time

-- | Specify a new category for an OpsItem.
updateOpsItem_category :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.Text)
updateOpsItem_category = Lens.lens (\UpdateOpsItem' {category} -> category) (\s@UpdateOpsItem' {} a -> s {category = a} :: UpdateOpsItem)

-- | Update the information about the OpsItem. Provide enough information so
-- that users reading this OpsItem for the first time understand the issue.
updateOpsItem_description :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.Text)
updateOpsItem_description = Lens.lens (\UpdateOpsItem' {description} -> description) (\s@UpdateOpsItem' {} a -> s {description = a} :: UpdateOpsItem)

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
updateOpsItem_notifications :: Lens.Lens' UpdateOpsItem (Prelude.Maybe [OpsItemNotification])
updateOpsItem_notifications = Lens.lens (\UpdateOpsItem' {notifications} -> notifications) (\s@UpdateOpsItem' {} a -> s {notifications = a} :: UpdateOpsItem) Prelude.. Lens.mapping Lens.coerced

-- | Add new keys or edit existing key-value pairs of the OperationalData map
-- in the OpsItem object.
--
-- Operational data is custom data that provides useful reference details
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
updateOpsItem_operationalData :: Lens.Lens' UpdateOpsItem (Prelude.Maybe (Prelude.HashMap Prelude.Text OpsItemDataValue))
updateOpsItem_operationalData = Lens.lens (\UpdateOpsItem' {operationalData} -> operationalData) (\s@UpdateOpsItem' {} a -> s {operationalData = a} :: UpdateOpsItem) Prelude.. Lens.mapping Lens.coerced

-- | Keys that you want to remove from the OperationalData map.
updateOpsItem_operationalDataToDelete :: Lens.Lens' UpdateOpsItem (Prelude.Maybe [Prelude.Text])
updateOpsItem_operationalDataToDelete = Lens.lens (\UpdateOpsItem' {operationalDataToDelete} -> operationalDataToDelete) (\s@UpdateOpsItem' {} a -> s {operationalDataToDelete = a} :: UpdateOpsItem) Prelude.. Lens.mapping Lens.coerced

-- | The OpsItem Amazon Resource Name (ARN).
updateOpsItem_opsItemArn :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.Text)
updateOpsItem_opsItemArn = Lens.lens (\UpdateOpsItem' {opsItemArn} -> opsItemArn) (\s@UpdateOpsItem' {} a -> s {opsItemArn = a} :: UpdateOpsItem)

-- | The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
updateOpsItem_plannedEndTime :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.UTCTime)
updateOpsItem_plannedEndTime = Lens.lens (\UpdateOpsItem' {plannedEndTime} -> plannedEndTime) (\s@UpdateOpsItem' {} a -> s {plannedEndTime = a} :: UpdateOpsItem) Prelude.. Lens.mapping Data._Time

-- | The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
updateOpsItem_plannedStartTime :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.UTCTime)
updateOpsItem_plannedStartTime = Lens.lens (\UpdateOpsItem' {plannedStartTime} -> plannedStartTime) (\s@UpdateOpsItem' {} a -> s {plannedStartTime = a} :: UpdateOpsItem) Prelude.. Lens.mapping Data._Time

-- | The importance of this OpsItem in relation to other OpsItems in the
-- system.
updateOpsItem_priority :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.Natural)
updateOpsItem_priority = Lens.lens (\UpdateOpsItem' {priority} -> priority) (\s@UpdateOpsItem' {} a -> s {priority = a} :: UpdateOpsItem)

-- | One or more OpsItems that share something in common with the current
-- OpsItems. For example, related OpsItems can include OpsItems with
-- similar error messages, impacted resources, or statuses for the impacted
-- resource.
updateOpsItem_relatedOpsItems :: Lens.Lens' UpdateOpsItem (Prelude.Maybe [RelatedOpsItem])
updateOpsItem_relatedOpsItems = Lens.lens (\UpdateOpsItem' {relatedOpsItems} -> relatedOpsItems) (\s@UpdateOpsItem' {} a -> s {relatedOpsItems = a} :: UpdateOpsItem) Prelude.. Lens.mapping Lens.coerced

-- | Specify a new severity for an OpsItem.
updateOpsItem_severity :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.Text)
updateOpsItem_severity = Lens.lens (\UpdateOpsItem' {severity} -> severity) (\s@UpdateOpsItem' {} a -> s {severity = a} :: UpdateOpsItem)

-- | The OpsItem status. Status can be @Open@, @In Progress@, or @Resolved@.
-- For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details>
-- in the /Amazon Web Services Systems Manager User Guide/.
updateOpsItem_status :: Lens.Lens' UpdateOpsItem (Prelude.Maybe OpsItemStatus)
updateOpsItem_status = Lens.lens (\UpdateOpsItem' {status} -> status) (\s@UpdateOpsItem' {} a -> s {status = a} :: UpdateOpsItem)

-- | A short heading that describes the nature of the OpsItem and the
-- impacted resource.
updateOpsItem_title :: Lens.Lens' UpdateOpsItem (Prelude.Maybe Prelude.Text)
updateOpsItem_title = Lens.lens (\UpdateOpsItem' {title} -> title) (\s@UpdateOpsItem' {} a -> s {title = a} :: UpdateOpsItem)

-- | The ID of the OpsItem.
updateOpsItem_opsItemId :: Lens.Lens' UpdateOpsItem Prelude.Text
updateOpsItem_opsItemId = Lens.lens (\UpdateOpsItem' {opsItemId} -> opsItemId) (\s@UpdateOpsItem' {} a -> s {opsItemId = a} :: UpdateOpsItem)

instance Core.AWSRequest UpdateOpsItem where
  type
    AWSResponse UpdateOpsItem =
      UpdateOpsItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateOpsItemResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateOpsItem where
  hashWithSalt _salt UpdateOpsItem' {..} =
    _salt
      `Prelude.hashWithSalt` actualEndTime
      `Prelude.hashWithSalt` actualStartTime
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` notifications
      `Prelude.hashWithSalt` operationalData
      `Prelude.hashWithSalt` operationalDataToDelete
      `Prelude.hashWithSalt` opsItemArn
      `Prelude.hashWithSalt` plannedEndTime
      `Prelude.hashWithSalt` plannedStartTime
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` relatedOpsItems
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` opsItemId

instance Prelude.NFData UpdateOpsItem where
  rnf UpdateOpsItem' {..} =
    Prelude.rnf actualEndTime `Prelude.seq`
      Prelude.rnf actualStartTime `Prelude.seq`
        Prelude.rnf category `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf notifications `Prelude.seq`
              Prelude.rnf operationalData `Prelude.seq`
                Prelude.rnf operationalDataToDelete `Prelude.seq`
                  Prelude.rnf opsItemArn `Prelude.seq`
                    Prelude.rnf plannedEndTime `Prelude.seq`
                      Prelude.rnf plannedStartTime `Prelude.seq`
                        Prelude.rnf priority `Prelude.seq`
                          Prelude.rnf relatedOpsItems `Prelude.seq`
                            Prelude.rnf severity `Prelude.seq`
                              Prelude.rnf status `Prelude.seq`
                                Prelude.rnf title `Prelude.seq`
                                  Prelude.rnf opsItemId

instance Data.ToHeaders UpdateOpsItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.UpdateOpsItem" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateOpsItem where
  toJSON UpdateOpsItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActualEndTime" Data..=) Prelude.<$> actualEndTime,
            ("ActualStartTime" Data..=)
              Prelude.<$> actualStartTime,
            ("Category" Data..=) Prelude.<$> category,
            ("Description" Data..=) Prelude.<$> description,
            ("Notifications" Data..=) Prelude.<$> notifications,
            ("OperationalData" Data..=)
              Prelude.<$> operationalData,
            ("OperationalDataToDelete" Data..=)
              Prelude.<$> operationalDataToDelete,
            ("OpsItemArn" Data..=) Prelude.<$> opsItemArn,
            ("PlannedEndTime" Data..=)
              Prelude.<$> plannedEndTime,
            ("PlannedStartTime" Data..=)
              Prelude.<$> plannedStartTime,
            ("Priority" Data..=) Prelude.<$> priority,
            ("RelatedOpsItems" Data..=)
              Prelude.<$> relatedOpsItems,
            ("Severity" Data..=) Prelude.<$> severity,
            ("Status" Data..=) Prelude.<$> status,
            ("Title" Data..=) Prelude.<$> title,
            Prelude.Just ("OpsItemId" Data..= opsItemId)
          ]
      )

instance Data.ToPath UpdateOpsItem where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateOpsItem where
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

instance Prelude.NFData UpdateOpsItemResponse where
  rnf UpdateOpsItemResponse' {..} =
    Prelude.rnf httpStatus
