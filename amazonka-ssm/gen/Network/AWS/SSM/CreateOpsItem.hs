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
-- Module      : Network.AWS.SSM.CreateOpsItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OpsItem. You must have permission in AWS Identity and
-- Access Management (IAM) to create a new OpsItem. For more information,
-- see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter>
-- in the /AWS Systems Manager User Guide/.
--
-- Operations engineers and IT professionals use OpsCenter to view,
-- investigate, and remediate operational issues impacting the performance
-- and health of their AWS resources. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter>
-- in the /AWS Systems Manager User Guide/.
module Network.AWS.SSM.CreateOpsItem
  ( -- * Creating a Request
    CreateOpsItem (..),
    newCreateOpsItem,

    -- * Request Lenses
    createOpsItem_plannedEndTime,
    createOpsItem_severity,
    createOpsItem_actualStartTime,
    createOpsItem_category,
    createOpsItem_operationalData,
    createOpsItem_priority,
    createOpsItem_actualEndTime,
    createOpsItem_tags,
    createOpsItem_opsItemType,
    createOpsItem_plannedStartTime,
    createOpsItem_notifications,
    createOpsItem_relatedOpsItems,
    createOpsItem_description,
    createOpsItem_source,
    createOpsItem_title,

    -- * Destructuring the Response
    CreateOpsItemResponse (..),
    newCreateOpsItemResponse,

    -- * Response Lenses
    createOpsItemResponse_opsItemId,
    createOpsItemResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreateOpsItem' smart constructor.
data CreateOpsItem = CreateOpsItem'
  { -- | The time specified in a change request for a runbook workflow to end.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedEndTime :: Core.Maybe Core.POSIX,
    -- | Specify a severity to assign to an OpsItem.
    severity :: Core.Maybe Core.Text,
    -- | The time a runbook workflow started. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualStartTime :: Core.Maybe Core.POSIX,
    -- | Specify a category to assign to an OpsItem.
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
    -- | The importance of this OpsItem in relation to other OpsItems in the
    -- system.
    priority :: Core.Maybe Core.Natural,
    -- | The time a runbook workflow ended. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualEndTime :: Core.Maybe Core.POSIX,
    -- | Optional metadata that you assign to a resource. You can restrict access
    -- to OpsItems by using an inline IAM policy that specifies tags. For more
    -- information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter>
    -- in the /AWS Systems Manager User Guide/.
    --
    -- Tags use a key-value pair. For example:
    --
    -- @Key=Department,Value=Finance@
    --
    -- To add tags to an existing OpsItem, use the AddTagsToResource action.
    tags :: Core.Maybe [Tag],
    -- | The type of OpsItem to create. Currently, the only valid values are
    -- @\/aws\/changerequest@ and @\/aws\/issue@.
    opsItemType :: Core.Maybe Core.Text,
    -- | The time specified in a change request for a runbook workflow to start.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedStartTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
    -- sent when this OpsItem is edited or changed.
    notifications :: Core.Maybe [OpsItemNotification],
    -- | One or more OpsItems that share something in common with the current
    -- OpsItems. For example, related OpsItems can include OpsItems with
    -- similar error messages, impacted resources, or statuses for the impacted
    -- resource.
    relatedOpsItems :: Core.Maybe [RelatedOpsItem],
    -- | Information about the OpsItem.
    description :: Core.Text,
    -- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
    --
    -- The source name can\'t contain the following strings: aws, amazon, and
    -- amzn.
    source :: Core.Text,
    -- | A short heading that describes the nature of the OpsItem and the
    -- impacted resource.
    title :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOpsItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'plannedEndTime', 'createOpsItem_plannedEndTime' - The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'severity', 'createOpsItem_severity' - Specify a severity to assign to an OpsItem.
--
-- 'actualStartTime', 'createOpsItem_actualStartTime' - The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'category', 'createOpsItem_category' - Specify a category to assign to an OpsItem.
--
-- 'operationalData', 'createOpsItem_operationalData' - Operational data is custom data that provides useful reference details
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
-- 'priority', 'createOpsItem_priority' - The importance of this OpsItem in relation to other OpsItems in the
-- system.
--
-- 'actualEndTime', 'createOpsItem_actualEndTime' - The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'tags', 'createOpsItem_tags' - Optional metadata that you assign to a resource. You can restrict access
-- to OpsItems by using an inline IAM policy that specifies tags. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter>
-- in the /AWS Systems Manager User Guide/.
--
-- Tags use a key-value pair. For example:
--
-- @Key=Department,Value=Finance@
--
-- To add tags to an existing OpsItem, use the AddTagsToResource action.
--
-- 'opsItemType', 'createOpsItem_opsItemType' - The type of OpsItem to create. Currently, the only valid values are
-- @\/aws\/changerequest@ and @\/aws\/issue@.
--
-- 'plannedStartTime', 'createOpsItem_plannedStartTime' - The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'notifications', 'createOpsItem_notifications' - The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
--
-- 'relatedOpsItems', 'createOpsItem_relatedOpsItems' - One or more OpsItems that share something in common with the current
-- OpsItems. For example, related OpsItems can include OpsItems with
-- similar error messages, impacted resources, or statuses for the impacted
-- resource.
--
-- 'description', 'createOpsItem_description' - Information about the OpsItem.
--
-- 'source', 'createOpsItem_source' - The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
--
-- The source name can\'t contain the following strings: aws, amazon, and
-- amzn.
--
-- 'title', 'createOpsItem_title' - A short heading that describes the nature of the OpsItem and the
-- impacted resource.
newCreateOpsItem ::
  -- | 'description'
  Core.Text ->
  -- | 'source'
  Core.Text ->
  -- | 'title'
  Core.Text ->
  CreateOpsItem
newCreateOpsItem pDescription_ pSource_ pTitle_ =
  CreateOpsItem'
    { plannedEndTime = Core.Nothing,
      severity = Core.Nothing,
      actualStartTime = Core.Nothing,
      category = Core.Nothing,
      operationalData = Core.Nothing,
      priority = Core.Nothing,
      actualEndTime = Core.Nothing,
      tags = Core.Nothing,
      opsItemType = Core.Nothing,
      plannedStartTime = Core.Nothing,
      notifications = Core.Nothing,
      relatedOpsItems = Core.Nothing,
      description = pDescription_,
      source = pSource_,
      title = pTitle_
    }

-- | The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
createOpsItem_plannedEndTime :: Lens.Lens' CreateOpsItem (Core.Maybe Core.UTCTime)
createOpsItem_plannedEndTime = Lens.lens (\CreateOpsItem' {plannedEndTime} -> plannedEndTime) (\s@CreateOpsItem' {} a -> s {plannedEndTime = a} :: CreateOpsItem) Core.. Lens.mapping Core._Time

-- | Specify a severity to assign to an OpsItem.
createOpsItem_severity :: Lens.Lens' CreateOpsItem (Core.Maybe Core.Text)
createOpsItem_severity = Lens.lens (\CreateOpsItem' {severity} -> severity) (\s@CreateOpsItem' {} a -> s {severity = a} :: CreateOpsItem)

-- | The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
createOpsItem_actualStartTime :: Lens.Lens' CreateOpsItem (Core.Maybe Core.UTCTime)
createOpsItem_actualStartTime = Lens.lens (\CreateOpsItem' {actualStartTime} -> actualStartTime) (\s@CreateOpsItem' {} a -> s {actualStartTime = a} :: CreateOpsItem) Core.. Lens.mapping Core._Time

-- | Specify a category to assign to an OpsItem.
createOpsItem_category :: Lens.Lens' CreateOpsItem (Core.Maybe Core.Text)
createOpsItem_category = Lens.lens (\CreateOpsItem' {category} -> category) (\s@CreateOpsItem' {} a -> s {category = a} :: CreateOpsItem)

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
createOpsItem_operationalData :: Lens.Lens' CreateOpsItem (Core.Maybe (Core.HashMap Core.Text OpsItemDataValue))
createOpsItem_operationalData = Lens.lens (\CreateOpsItem' {operationalData} -> operationalData) (\s@CreateOpsItem' {} a -> s {operationalData = a} :: CreateOpsItem) Core.. Lens.mapping Lens._Coerce

-- | The importance of this OpsItem in relation to other OpsItems in the
-- system.
createOpsItem_priority :: Lens.Lens' CreateOpsItem (Core.Maybe Core.Natural)
createOpsItem_priority = Lens.lens (\CreateOpsItem' {priority} -> priority) (\s@CreateOpsItem' {} a -> s {priority = a} :: CreateOpsItem)

-- | The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
createOpsItem_actualEndTime :: Lens.Lens' CreateOpsItem (Core.Maybe Core.UTCTime)
createOpsItem_actualEndTime = Lens.lens (\CreateOpsItem' {actualEndTime} -> actualEndTime) (\s@CreateOpsItem' {} a -> s {actualEndTime = a} :: CreateOpsItem) Core.. Lens.mapping Core._Time

-- | Optional metadata that you assign to a resource. You can restrict access
-- to OpsItems by using an inline IAM policy that specifies tags. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter>
-- in the /AWS Systems Manager User Guide/.
--
-- Tags use a key-value pair. For example:
--
-- @Key=Department,Value=Finance@
--
-- To add tags to an existing OpsItem, use the AddTagsToResource action.
createOpsItem_tags :: Lens.Lens' CreateOpsItem (Core.Maybe [Tag])
createOpsItem_tags = Lens.lens (\CreateOpsItem' {tags} -> tags) (\s@CreateOpsItem' {} a -> s {tags = a} :: CreateOpsItem) Core.. Lens.mapping Lens._Coerce

-- | The type of OpsItem to create. Currently, the only valid values are
-- @\/aws\/changerequest@ and @\/aws\/issue@.
createOpsItem_opsItemType :: Lens.Lens' CreateOpsItem (Core.Maybe Core.Text)
createOpsItem_opsItemType = Lens.lens (\CreateOpsItem' {opsItemType} -> opsItemType) (\s@CreateOpsItem' {} a -> s {opsItemType = a} :: CreateOpsItem)

-- | The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
createOpsItem_plannedStartTime :: Lens.Lens' CreateOpsItem (Core.Maybe Core.UTCTime)
createOpsItem_plannedStartTime = Lens.lens (\CreateOpsItem' {plannedStartTime} -> plannedStartTime) (\s@CreateOpsItem' {} a -> s {plannedStartTime = a} :: CreateOpsItem) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
createOpsItem_notifications :: Lens.Lens' CreateOpsItem (Core.Maybe [OpsItemNotification])
createOpsItem_notifications = Lens.lens (\CreateOpsItem' {notifications} -> notifications) (\s@CreateOpsItem' {} a -> s {notifications = a} :: CreateOpsItem) Core.. Lens.mapping Lens._Coerce

-- | One or more OpsItems that share something in common with the current
-- OpsItems. For example, related OpsItems can include OpsItems with
-- similar error messages, impacted resources, or statuses for the impacted
-- resource.
createOpsItem_relatedOpsItems :: Lens.Lens' CreateOpsItem (Core.Maybe [RelatedOpsItem])
createOpsItem_relatedOpsItems = Lens.lens (\CreateOpsItem' {relatedOpsItems} -> relatedOpsItems) (\s@CreateOpsItem' {} a -> s {relatedOpsItems = a} :: CreateOpsItem) Core.. Lens.mapping Lens._Coerce

-- | Information about the OpsItem.
createOpsItem_description :: Lens.Lens' CreateOpsItem Core.Text
createOpsItem_description = Lens.lens (\CreateOpsItem' {description} -> description) (\s@CreateOpsItem' {} a -> s {description = a} :: CreateOpsItem)

-- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
--
-- The source name can\'t contain the following strings: aws, amazon, and
-- amzn.
createOpsItem_source :: Lens.Lens' CreateOpsItem Core.Text
createOpsItem_source = Lens.lens (\CreateOpsItem' {source} -> source) (\s@CreateOpsItem' {} a -> s {source = a} :: CreateOpsItem)

-- | A short heading that describes the nature of the OpsItem and the
-- impacted resource.
createOpsItem_title :: Lens.Lens' CreateOpsItem Core.Text
createOpsItem_title = Lens.lens (\CreateOpsItem' {title} -> title) (\s@CreateOpsItem' {} a -> s {title = a} :: CreateOpsItem)

instance Core.AWSRequest CreateOpsItem where
  type
    AWSResponse CreateOpsItem =
      CreateOpsItemResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOpsItemResponse'
            Core.<$> (x Core..?> "OpsItemId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateOpsItem

instance Core.NFData CreateOpsItem

instance Core.ToHeaders CreateOpsItem where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.CreateOpsItem" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateOpsItem where
  toJSON CreateOpsItem' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PlannedEndTime" Core..=) Core.<$> plannedEndTime,
            ("Severity" Core..=) Core.<$> severity,
            ("ActualStartTime" Core..=) Core.<$> actualStartTime,
            ("Category" Core..=) Core.<$> category,
            ("OperationalData" Core..=) Core.<$> operationalData,
            ("Priority" Core..=) Core.<$> priority,
            ("ActualEndTime" Core..=) Core.<$> actualEndTime,
            ("Tags" Core..=) Core.<$> tags,
            ("OpsItemType" Core..=) Core.<$> opsItemType,
            ("PlannedStartTime" Core..=)
              Core.<$> plannedStartTime,
            ("Notifications" Core..=) Core.<$> notifications,
            ("RelatedOpsItems" Core..=) Core.<$> relatedOpsItems,
            Core.Just ("Description" Core..= description),
            Core.Just ("Source" Core..= source),
            Core.Just ("Title" Core..= title)
          ]
      )

instance Core.ToPath CreateOpsItem where
  toPath = Core.const "/"

instance Core.ToQuery CreateOpsItem where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateOpsItemResponse' smart constructor.
data CreateOpsItemResponse = CreateOpsItemResponse'
  { -- | The ID of the OpsItem.
    opsItemId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOpsItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsItemId', 'createOpsItemResponse_opsItemId' - The ID of the OpsItem.
--
-- 'httpStatus', 'createOpsItemResponse_httpStatus' - The response's http status code.
newCreateOpsItemResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateOpsItemResponse
newCreateOpsItemResponse pHttpStatus_ =
  CreateOpsItemResponse'
    { opsItemId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the OpsItem.
createOpsItemResponse_opsItemId :: Lens.Lens' CreateOpsItemResponse (Core.Maybe Core.Text)
createOpsItemResponse_opsItemId = Lens.lens (\CreateOpsItemResponse' {opsItemId} -> opsItemId) (\s@CreateOpsItemResponse' {} a -> s {opsItemId = a} :: CreateOpsItemResponse)

-- | The response's http status code.
createOpsItemResponse_httpStatus :: Lens.Lens' CreateOpsItemResponse Core.Int
createOpsItemResponse_httpStatus = Lens.lens (\CreateOpsItemResponse' {httpStatus} -> httpStatus) (\s@CreateOpsItemResponse' {} a -> s {httpStatus = a} :: CreateOpsItemResponse)

instance Core.NFData CreateOpsItemResponse
