{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreateOpsItem' smart constructor.
data CreateOpsItem = CreateOpsItem'
  { -- | The time specified in a change request for a runbook workflow to end.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedEndTime :: Prelude.Maybe Prelude.POSIX,
    -- | Specify a severity to assign to an OpsItem.
    severity :: Prelude.Maybe Prelude.Text,
    -- | The time a runbook workflow started. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualStartTime :: Prelude.Maybe Prelude.POSIX,
    -- | Specify a category to assign to an OpsItem.
    category :: Prelude.Maybe Prelude.Text,
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
    operationalData :: Prelude.Maybe (Prelude.HashMap Prelude.Text OpsItemDataValue),
    -- | The importance of this OpsItem in relation to other OpsItems in the
    -- system.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The time a runbook workflow ended. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualEndTime :: Prelude.Maybe Prelude.POSIX,
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
    tags :: Prelude.Maybe [Tag],
    -- | The type of OpsItem to create. Currently, the only valid values are
    -- @\/aws\/changerequest@ and @\/aws\/issue@.
    opsItemType :: Prelude.Maybe Prelude.Text,
    -- | The time specified in a change request for a runbook workflow to start.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedStartTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
    -- sent when this OpsItem is edited or changed.
    notifications :: Prelude.Maybe [OpsItemNotification],
    -- | One or more OpsItems that share something in common with the current
    -- OpsItems. For example, related OpsItems can include OpsItems with
    -- similar error messages, impacted resources, or statuses for the impacted
    -- resource.
    relatedOpsItems :: Prelude.Maybe [RelatedOpsItem],
    -- | Information about the OpsItem.
    description :: Prelude.Text,
    -- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
    --
    -- The source name can\'t contain the following strings: aws, amazon, and
    -- amzn.
    source :: Prelude.Text,
    -- | A short heading that describes the nature of the OpsItem and the
    -- impacted resource.
    title :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'source'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  CreateOpsItem
newCreateOpsItem pDescription_ pSource_ pTitle_ =
  CreateOpsItem'
    { plannedEndTime = Prelude.Nothing,
      severity = Prelude.Nothing,
      actualStartTime = Prelude.Nothing,
      category = Prelude.Nothing,
      operationalData = Prelude.Nothing,
      priority = Prelude.Nothing,
      actualEndTime = Prelude.Nothing,
      tags = Prelude.Nothing,
      opsItemType = Prelude.Nothing,
      plannedStartTime = Prelude.Nothing,
      notifications = Prelude.Nothing,
      relatedOpsItems = Prelude.Nothing,
      description = pDescription_,
      source = pSource_,
      title = pTitle_
    }

-- | The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
createOpsItem_plannedEndTime :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.UTCTime)
createOpsItem_plannedEndTime = Lens.lens (\CreateOpsItem' {plannedEndTime} -> plannedEndTime) (\s@CreateOpsItem' {} a -> s {plannedEndTime = a} :: CreateOpsItem) Prelude.. Lens.mapping Prelude._Time

-- | Specify a severity to assign to an OpsItem.
createOpsItem_severity :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.Text)
createOpsItem_severity = Lens.lens (\CreateOpsItem' {severity} -> severity) (\s@CreateOpsItem' {} a -> s {severity = a} :: CreateOpsItem)

-- | The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
createOpsItem_actualStartTime :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.UTCTime)
createOpsItem_actualStartTime = Lens.lens (\CreateOpsItem' {actualStartTime} -> actualStartTime) (\s@CreateOpsItem' {} a -> s {actualStartTime = a} :: CreateOpsItem) Prelude.. Lens.mapping Prelude._Time

-- | Specify a category to assign to an OpsItem.
createOpsItem_category :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.Text)
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
createOpsItem_operationalData :: Lens.Lens' CreateOpsItem (Prelude.Maybe (Prelude.HashMap Prelude.Text OpsItemDataValue))
createOpsItem_operationalData = Lens.lens (\CreateOpsItem' {operationalData} -> operationalData) (\s@CreateOpsItem' {} a -> s {operationalData = a} :: CreateOpsItem) Prelude.. Lens.mapping Prelude._Coerce

-- | The importance of this OpsItem in relation to other OpsItems in the
-- system.
createOpsItem_priority :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.Natural)
createOpsItem_priority = Lens.lens (\CreateOpsItem' {priority} -> priority) (\s@CreateOpsItem' {} a -> s {priority = a} :: CreateOpsItem)

-- | The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
createOpsItem_actualEndTime :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.UTCTime)
createOpsItem_actualEndTime = Lens.lens (\CreateOpsItem' {actualEndTime} -> actualEndTime) (\s@CreateOpsItem' {} a -> s {actualEndTime = a} :: CreateOpsItem) Prelude.. Lens.mapping Prelude._Time

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
createOpsItem_tags :: Lens.Lens' CreateOpsItem (Prelude.Maybe [Tag])
createOpsItem_tags = Lens.lens (\CreateOpsItem' {tags} -> tags) (\s@CreateOpsItem' {} a -> s {tags = a} :: CreateOpsItem) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of OpsItem to create. Currently, the only valid values are
-- @\/aws\/changerequest@ and @\/aws\/issue@.
createOpsItem_opsItemType :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.Text)
createOpsItem_opsItemType = Lens.lens (\CreateOpsItem' {opsItemType} -> opsItemType) (\s@CreateOpsItem' {} a -> s {opsItemType = a} :: CreateOpsItem)

-- | The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
createOpsItem_plannedStartTime :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.UTCTime)
createOpsItem_plannedStartTime = Lens.lens (\CreateOpsItem' {plannedStartTime} -> plannedStartTime) (\s@CreateOpsItem' {} a -> s {plannedStartTime = a} :: CreateOpsItem) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
createOpsItem_notifications :: Lens.Lens' CreateOpsItem (Prelude.Maybe [OpsItemNotification])
createOpsItem_notifications = Lens.lens (\CreateOpsItem' {notifications} -> notifications) (\s@CreateOpsItem' {} a -> s {notifications = a} :: CreateOpsItem) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more OpsItems that share something in common with the current
-- OpsItems. For example, related OpsItems can include OpsItems with
-- similar error messages, impacted resources, or statuses for the impacted
-- resource.
createOpsItem_relatedOpsItems :: Lens.Lens' CreateOpsItem (Prelude.Maybe [RelatedOpsItem])
createOpsItem_relatedOpsItems = Lens.lens (\CreateOpsItem' {relatedOpsItems} -> relatedOpsItems) (\s@CreateOpsItem' {} a -> s {relatedOpsItems = a} :: CreateOpsItem) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the OpsItem.
createOpsItem_description :: Lens.Lens' CreateOpsItem Prelude.Text
createOpsItem_description = Lens.lens (\CreateOpsItem' {description} -> description) (\s@CreateOpsItem' {} a -> s {description = a} :: CreateOpsItem)

-- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
--
-- The source name can\'t contain the following strings: aws, amazon, and
-- amzn.
createOpsItem_source :: Lens.Lens' CreateOpsItem Prelude.Text
createOpsItem_source = Lens.lens (\CreateOpsItem' {source} -> source) (\s@CreateOpsItem' {} a -> s {source = a} :: CreateOpsItem)

-- | A short heading that describes the nature of the OpsItem and the
-- impacted resource.
createOpsItem_title :: Lens.Lens' CreateOpsItem Prelude.Text
createOpsItem_title = Lens.lens (\CreateOpsItem' {title} -> title) (\s@CreateOpsItem' {} a -> s {title = a} :: CreateOpsItem)

instance Prelude.AWSRequest CreateOpsItem where
  type Rs CreateOpsItem = CreateOpsItemResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOpsItemResponse'
            Prelude.<$> (x Prelude..?> "OpsItemId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOpsItem

instance Prelude.NFData CreateOpsItem

instance Prelude.ToHeaders CreateOpsItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AmazonSSM.CreateOpsItem" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateOpsItem where
  toJSON CreateOpsItem' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PlannedEndTime" Prelude..=)
              Prelude.<$> plannedEndTime,
            ("Severity" Prelude..=) Prelude.<$> severity,
            ("ActualStartTime" Prelude..=)
              Prelude.<$> actualStartTime,
            ("Category" Prelude..=) Prelude.<$> category,
            ("OperationalData" Prelude..=)
              Prelude.<$> operationalData,
            ("Priority" Prelude..=) Prelude.<$> priority,
            ("ActualEndTime" Prelude..=)
              Prelude.<$> actualEndTime,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("OpsItemType" Prelude..=) Prelude.<$> opsItemType,
            ("PlannedStartTime" Prelude..=)
              Prelude.<$> plannedStartTime,
            ("Notifications" Prelude..=)
              Prelude.<$> notifications,
            ("RelatedOpsItems" Prelude..=)
              Prelude.<$> relatedOpsItems,
            Prelude.Just ("Description" Prelude..= description),
            Prelude.Just ("Source" Prelude..= source),
            Prelude.Just ("Title" Prelude..= title)
          ]
      )

instance Prelude.ToPath CreateOpsItem where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateOpsItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOpsItemResponse' smart constructor.
data CreateOpsItemResponse = CreateOpsItemResponse'
  { -- | The ID of the OpsItem.
    opsItemId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateOpsItemResponse
newCreateOpsItemResponse pHttpStatus_ =
  CreateOpsItemResponse'
    { opsItemId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the OpsItem.
createOpsItemResponse_opsItemId :: Lens.Lens' CreateOpsItemResponse (Prelude.Maybe Prelude.Text)
createOpsItemResponse_opsItemId = Lens.lens (\CreateOpsItemResponse' {opsItemId} -> opsItemId) (\s@CreateOpsItemResponse' {} a -> s {opsItemId = a} :: CreateOpsItemResponse)

-- | The response's http status code.
createOpsItemResponse_httpStatus :: Lens.Lens' CreateOpsItemResponse Prelude.Int
createOpsItemResponse_httpStatus = Lens.lens (\CreateOpsItemResponse' {httpStatus} -> httpStatus) (\s@CreateOpsItemResponse' {} a -> s {httpStatus = a} :: CreateOpsItemResponse)

instance Prelude.NFData CreateOpsItemResponse
