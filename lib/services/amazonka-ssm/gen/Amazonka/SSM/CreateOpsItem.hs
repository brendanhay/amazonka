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
-- Module      : Amazonka.SSM.CreateOpsItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OpsItem. You must have permission in Identity and Access
-- Management (IAM) to create a new OpsItem. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- Operations engineers and IT professionals use Amazon Web Services
-- Systems Manager OpsCenter to view, investigate, and remediate
-- operational issues impacting the performance and health of their Amazon
-- Web Services resources. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html Amazon Web Services Systems Manager OpsCenter>
-- in the /Amazon Web Services Systems Manager User Guide/.
module Amazonka.SSM.CreateOpsItem
  ( -- * Creating a Request
    CreateOpsItem (..),
    newCreateOpsItem,

    -- * Request Lenses
    createOpsItem_tags,
    createOpsItem_notifications,
    createOpsItem_severity,
    createOpsItem_plannedStartTime,
    createOpsItem_plannedEndTime,
    createOpsItem_accountId,
    createOpsItem_priority,
    createOpsItem_opsItemType,
    createOpsItem_category,
    createOpsItem_operationalData,
    createOpsItem_actualStartTime,
    createOpsItem_actualEndTime,
    createOpsItem_relatedOpsItems,
    createOpsItem_description,
    createOpsItem_source,
    createOpsItem_title,

    -- * Destructuring the Response
    CreateOpsItemResponse (..),
    newCreateOpsItemResponse,

    -- * Response Lenses
    createOpsItemResponse_opsItemArn,
    createOpsItemResponse_opsItemId,
    createOpsItemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newCreateOpsItem' smart constructor.
data CreateOpsItem = CreateOpsItem'
  { -- | Optional metadata that you assign to a resource. You can restrict access
    -- to OpsItems by using an inline IAM policy that specifies tags. For more
    -- information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    --
    -- Tags use a key-value pair. For example:
    --
    -- @Key=Department,Value=Finance@
    --
    -- To add tags to a new OpsItem, a user must have IAM permissions for both
    -- the @ssm:CreateOpsItems@ operation and the @ssm:AddTagsToResource@
    -- operation. To add tags to an existing OpsItem, use the AddTagsToResource
    -- operation.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
    -- sent when this OpsItem is edited or changed.
    notifications :: Prelude.Maybe [OpsItemNotification],
    -- | Specify a severity to assign to an OpsItem.
    severity :: Prelude.Maybe Prelude.Text,
    -- | The time specified in a change request for a runbook workflow to start.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedStartTime :: Prelude.Maybe Core.POSIX,
    -- | The time specified in a change request for a runbook workflow to end.
    -- Currently supported only for the OpsItem type @\/aws\/changerequest@.
    plannedEndTime :: Prelude.Maybe Core.POSIX,
    -- | The target Amazon Web Services account where you want to create an
    -- OpsItem. To make this call, your account must be configured to work with
    -- OpsItems across accounts. For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-OpsCenter-multiple-accounts.html Setting up OpsCenter to work with OpsItems across accounts>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The importance of this OpsItem in relation to other OpsItems in the
    -- system.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The type of OpsItem to create. Systems Manager supports the following
    -- types of OpsItems:
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
    -- | Specify a category to assign to an OpsItem.
    category :: Prelude.Maybe Prelude.Text,
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
    -- | The time a runbook workflow started. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualStartTime :: Prelude.Maybe Core.POSIX,
    -- | The time a runbook workflow ended. Currently reported only for the
    -- OpsItem type @\/aws\/changerequest@.
    actualEndTime :: Prelude.Maybe Core.POSIX,
    -- | One or more OpsItems that share something in common with the current
    -- OpsItems. For example, related OpsItems can include OpsItems with
    -- similar error messages, impacted resources, or statuses for the impacted
    -- resource.
    relatedOpsItems :: Prelude.Maybe [RelatedOpsItem],
    -- | Information about the OpsItem.
    description :: Prelude.Text,
    -- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
    --
    -- The source name can\'t contain the following strings: @aws@, @amazon@,
    -- and @amzn@.
    source :: Prelude.Text,
    -- | A short heading that describes the nature of the OpsItem and the
    -- impacted resource.
    title :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOpsItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createOpsItem_tags' - Optional metadata that you assign to a resource. You can restrict access
-- to OpsItems by using an inline IAM policy that specifies tags. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- Tags use a key-value pair. For example:
--
-- @Key=Department,Value=Finance@
--
-- To add tags to a new OpsItem, a user must have IAM permissions for both
-- the @ssm:CreateOpsItems@ operation and the @ssm:AddTagsToResource@
-- operation. To add tags to an existing OpsItem, use the AddTagsToResource
-- operation.
--
-- 'notifications', 'createOpsItem_notifications' - The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
--
-- 'severity', 'createOpsItem_severity' - Specify a severity to assign to an OpsItem.
--
-- 'plannedStartTime', 'createOpsItem_plannedStartTime' - The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'plannedEndTime', 'createOpsItem_plannedEndTime' - The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
--
-- 'accountId', 'createOpsItem_accountId' - The target Amazon Web Services account where you want to create an
-- OpsItem. To make this call, your account must be configured to work with
-- OpsItems across accounts. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-OpsCenter-multiple-accounts.html Setting up OpsCenter to work with OpsItems across accounts>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'priority', 'createOpsItem_priority' - The importance of this OpsItem in relation to other OpsItems in the
-- system.
--
-- 'opsItemType', 'createOpsItem_opsItemType' - The type of OpsItem to create. Systems Manager supports the following
-- types of OpsItems:
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
-- 'category', 'createOpsItem_category' - Specify a category to assign to an OpsItem.
--
-- 'operationalData', 'createOpsItem_operationalData' - Operational data is custom data that provides useful reference details
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
-- 'actualStartTime', 'createOpsItem_actualStartTime' - The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
--
-- 'actualEndTime', 'createOpsItem_actualEndTime' - The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
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
-- The source name can\'t contain the following strings: @aws@, @amazon@,
-- and @amzn@.
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
    { tags = Prelude.Nothing,
      notifications = Prelude.Nothing,
      severity = Prelude.Nothing,
      plannedStartTime = Prelude.Nothing,
      plannedEndTime = Prelude.Nothing,
      accountId = Prelude.Nothing,
      priority = Prelude.Nothing,
      opsItemType = Prelude.Nothing,
      category = Prelude.Nothing,
      operationalData = Prelude.Nothing,
      actualStartTime = Prelude.Nothing,
      actualEndTime = Prelude.Nothing,
      relatedOpsItems = Prelude.Nothing,
      description = pDescription_,
      source = pSource_,
      title = pTitle_
    }

-- | Optional metadata that you assign to a resource. You can restrict access
-- to OpsItems by using an inline IAM policy that specifies tags. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- Tags use a key-value pair. For example:
--
-- @Key=Department,Value=Finance@
--
-- To add tags to a new OpsItem, a user must have IAM permissions for both
-- the @ssm:CreateOpsItems@ operation and the @ssm:AddTagsToResource@
-- operation. To add tags to an existing OpsItem, use the AddTagsToResource
-- operation.
createOpsItem_tags :: Lens.Lens' CreateOpsItem (Prelude.Maybe [Tag])
createOpsItem_tags = Lens.lens (\CreateOpsItem' {tags} -> tags) (\s@CreateOpsItem' {} a -> s {tags = a} :: CreateOpsItem) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
createOpsItem_notifications :: Lens.Lens' CreateOpsItem (Prelude.Maybe [OpsItemNotification])
createOpsItem_notifications = Lens.lens (\CreateOpsItem' {notifications} -> notifications) (\s@CreateOpsItem' {} a -> s {notifications = a} :: CreateOpsItem) Prelude.. Lens.mapping Lens.coerced

-- | Specify a severity to assign to an OpsItem.
createOpsItem_severity :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.Text)
createOpsItem_severity = Lens.lens (\CreateOpsItem' {severity} -> severity) (\s@CreateOpsItem' {} a -> s {severity = a} :: CreateOpsItem)

-- | The time specified in a change request for a runbook workflow to start.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
createOpsItem_plannedStartTime :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.UTCTime)
createOpsItem_plannedStartTime = Lens.lens (\CreateOpsItem' {plannedStartTime} -> plannedStartTime) (\s@CreateOpsItem' {} a -> s {plannedStartTime = a} :: CreateOpsItem) Prelude.. Lens.mapping Core._Time

-- | The time specified in a change request for a runbook workflow to end.
-- Currently supported only for the OpsItem type @\/aws\/changerequest@.
createOpsItem_plannedEndTime :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.UTCTime)
createOpsItem_plannedEndTime = Lens.lens (\CreateOpsItem' {plannedEndTime} -> plannedEndTime) (\s@CreateOpsItem' {} a -> s {plannedEndTime = a} :: CreateOpsItem) Prelude.. Lens.mapping Core._Time

-- | The target Amazon Web Services account where you want to create an
-- OpsItem. To make this call, your account must be configured to work with
-- OpsItems across accounts. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-OpsCenter-multiple-accounts.html Setting up OpsCenter to work with OpsItems across accounts>
-- in the /Amazon Web Services Systems Manager User Guide/.
createOpsItem_accountId :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.Text)
createOpsItem_accountId = Lens.lens (\CreateOpsItem' {accountId} -> accountId) (\s@CreateOpsItem' {} a -> s {accountId = a} :: CreateOpsItem)

-- | The importance of this OpsItem in relation to other OpsItems in the
-- system.
createOpsItem_priority :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.Natural)
createOpsItem_priority = Lens.lens (\CreateOpsItem' {priority} -> priority) (\s@CreateOpsItem' {} a -> s {priority = a} :: CreateOpsItem)

-- | The type of OpsItem to create. Systems Manager supports the following
-- types of OpsItems:
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
createOpsItem_opsItemType :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.Text)
createOpsItem_opsItemType = Lens.lens (\CreateOpsItem' {opsItemType} -> opsItemType) (\s@CreateOpsItem' {} a -> s {opsItemType = a} :: CreateOpsItem)

-- | Specify a category to assign to an OpsItem.
createOpsItem_category :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.Text)
createOpsItem_category = Lens.lens (\CreateOpsItem' {category} -> category) (\s@CreateOpsItem' {} a -> s {category = a} :: CreateOpsItem)

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
createOpsItem_operationalData :: Lens.Lens' CreateOpsItem (Prelude.Maybe (Prelude.HashMap Prelude.Text OpsItemDataValue))
createOpsItem_operationalData = Lens.lens (\CreateOpsItem' {operationalData} -> operationalData) (\s@CreateOpsItem' {} a -> s {operationalData = a} :: CreateOpsItem) Prelude.. Lens.mapping Lens.coerced

-- | The time a runbook workflow started. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
createOpsItem_actualStartTime :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.UTCTime)
createOpsItem_actualStartTime = Lens.lens (\CreateOpsItem' {actualStartTime} -> actualStartTime) (\s@CreateOpsItem' {} a -> s {actualStartTime = a} :: CreateOpsItem) Prelude.. Lens.mapping Core._Time

-- | The time a runbook workflow ended. Currently reported only for the
-- OpsItem type @\/aws\/changerequest@.
createOpsItem_actualEndTime :: Lens.Lens' CreateOpsItem (Prelude.Maybe Prelude.UTCTime)
createOpsItem_actualEndTime = Lens.lens (\CreateOpsItem' {actualEndTime} -> actualEndTime) (\s@CreateOpsItem' {} a -> s {actualEndTime = a} :: CreateOpsItem) Prelude.. Lens.mapping Core._Time

-- | One or more OpsItems that share something in common with the current
-- OpsItems. For example, related OpsItems can include OpsItems with
-- similar error messages, impacted resources, or statuses for the impacted
-- resource.
createOpsItem_relatedOpsItems :: Lens.Lens' CreateOpsItem (Prelude.Maybe [RelatedOpsItem])
createOpsItem_relatedOpsItems = Lens.lens (\CreateOpsItem' {relatedOpsItems} -> relatedOpsItems) (\s@CreateOpsItem' {} a -> s {relatedOpsItems = a} :: CreateOpsItem) Prelude.. Lens.mapping Lens.coerced

-- | Information about the OpsItem.
createOpsItem_description :: Lens.Lens' CreateOpsItem Prelude.Text
createOpsItem_description = Lens.lens (\CreateOpsItem' {description} -> description) (\s@CreateOpsItem' {} a -> s {description = a} :: CreateOpsItem)

-- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
--
-- The source name can\'t contain the following strings: @aws@, @amazon@,
-- and @amzn@.
createOpsItem_source :: Lens.Lens' CreateOpsItem Prelude.Text
createOpsItem_source = Lens.lens (\CreateOpsItem' {source} -> source) (\s@CreateOpsItem' {} a -> s {source = a} :: CreateOpsItem)

-- | A short heading that describes the nature of the OpsItem and the
-- impacted resource.
createOpsItem_title :: Lens.Lens' CreateOpsItem Prelude.Text
createOpsItem_title = Lens.lens (\CreateOpsItem' {title} -> title) (\s@CreateOpsItem' {} a -> s {title = a} :: CreateOpsItem)

instance Core.AWSRequest CreateOpsItem where
  type
    AWSResponse CreateOpsItem =
      CreateOpsItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOpsItemResponse'
            Prelude.<$> (x Core..?> "OpsItemArn")
            Prelude.<*> (x Core..?> "OpsItemId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOpsItem where
  hashWithSalt _salt CreateOpsItem' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` notifications
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` plannedStartTime
      `Prelude.hashWithSalt` plannedEndTime
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` opsItemType
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` operationalData
      `Prelude.hashWithSalt` actualStartTime
      `Prelude.hashWithSalt` actualEndTime
      `Prelude.hashWithSalt` relatedOpsItems
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` title

instance Prelude.NFData CreateOpsItem where
  rnf CreateOpsItem' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf notifications
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf plannedStartTime
      `Prelude.seq` Prelude.rnf plannedEndTime
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf opsItemType
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf operationalData
      `Prelude.seq` Prelude.rnf actualStartTime
      `Prelude.seq` Prelude.rnf actualEndTime
      `Prelude.seq` Prelude.rnf relatedOpsItems
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf title

instance Core.ToHeaders CreateOpsItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.CreateOpsItem" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateOpsItem where
  toJSON CreateOpsItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Notifications" Core..=) Prelude.<$> notifications,
            ("Severity" Core..=) Prelude.<$> severity,
            ("PlannedStartTime" Core..=)
              Prelude.<$> plannedStartTime,
            ("PlannedEndTime" Core..=)
              Prelude.<$> plannedEndTime,
            ("AccountId" Core..=) Prelude.<$> accountId,
            ("Priority" Core..=) Prelude.<$> priority,
            ("OpsItemType" Core..=) Prelude.<$> opsItemType,
            ("Category" Core..=) Prelude.<$> category,
            ("OperationalData" Core..=)
              Prelude.<$> operationalData,
            ("ActualStartTime" Core..=)
              Prelude.<$> actualStartTime,
            ("ActualEndTime" Core..=) Prelude.<$> actualEndTime,
            ("RelatedOpsItems" Core..=)
              Prelude.<$> relatedOpsItems,
            Prelude.Just ("Description" Core..= description),
            Prelude.Just ("Source" Core..= source),
            Prelude.Just ("Title" Core..= title)
          ]
      )

instance Core.ToPath CreateOpsItem where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateOpsItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOpsItemResponse' smart constructor.
data CreateOpsItemResponse = CreateOpsItemResponse'
  { -- | The OpsItem Amazon Resource Name (ARN).
    opsItemArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the OpsItem.
    opsItemId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOpsItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsItemArn', 'createOpsItemResponse_opsItemArn' - The OpsItem Amazon Resource Name (ARN).
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
    { opsItemArn =
        Prelude.Nothing,
      opsItemId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The OpsItem Amazon Resource Name (ARN).
createOpsItemResponse_opsItemArn :: Lens.Lens' CreateOpsItemResponse (Prelude.Maybe Prelude.Text)
createOpsItemResponse_opsItemArn = Lens.lens (\CreateOpsItemResponse' {opsItemArn} -> opsItemArn) (\s@CreateOpsItemResponse' {} a -> s {opsItemArn = a} :: CreateOpsItemResponse)

-- | The ID of the OpsItem.
createOpsItemResponse_opsItemId :: Lens.Lens' CreateOpsItemResponse (Prelude.Maybe Prelude.Text)
createOpsItemResponse_opsItemId = Lens.lens (\CreateOpsItemResponse' {opsItemId} -> opsItemId) (\s@CreateOpsItemResponse' {} a -> s {opsItemId = a} :: CreateOpsItemResponse)

-- | The response's http status code.
createOpsItemResponse_httpStatus :: Lens.Lens' CreateOpsItemResponse Prelude.Int
createOpsItemResponse_httpStatus = Lens.lens (\CreateOpsItemResponse' {httpStatus} -> httpStatus) (\s@CreateOpsItemResponse' {} a -> s {httpStatus = a} :: CreateOpsItemResponse)

instance Prelude.NFData CreateOpsItemResponse where
  rnf CreateOpsItemResponse' {..} =
    Prelude.rnf opsItemArn
      `Prelude.seq` Prelude.rnf opsItemId
      `Prelude.seq` Prelude.rnf httpStatus
