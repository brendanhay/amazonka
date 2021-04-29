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
-- Module      : Network.AWS.SQS.AddPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a permission to a queue for a specific
-- <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal>.
-- This allows sharing access to the queue.
--
-- When you create a queue, you have full control access rights for the
-- queue. Only you, the owner of the queue, can grant or deny permissions
-- to the queue. For more information about these permissions, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-writing-an-sqs-policy.html#write-messages-to-shared-queue Allow Developers to Write Messages to a Shared Queue>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- -   @AddPermission@ generates a policy for you. You can use
--     @ SetQueueAttributes @ to upload your policy. For more information,
--     see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-creating-custom-policies.html Using Custom Policies with the Amazon SQS Access Policy Language>
--     in the /Amazon Simple Queue Service Developer Guide/.
--
-- -   An Amazon SQS policy can have a maximum of 7 actions.
--
-- -   To remove the ability to change queue permissions, you must deny
--     permission to the @AddPermission@, @RemovePermission@, and
--     @SetQueueAttributes@ actions in your IAM policy.
--
-- Some actions take lists of parameters. These lists are specified using
-- the @param.n@ notation. Values of @n@ are integers starting from 1. For
-- example, a parameter list with two elements looks like this:
--
-- @&AttributeName.1=first@
--
-- @&AttributeName.2=second@
--
-- Cross-account permissions don\'t apply to this action. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-customer-managed-policy-examples.html#grant-cross-account-permissions-to-role-and-user-name Grant cross-account permissions to a role and a user name>
-- in the /Amazon Simple Queue Service Developer Guide/.
module Network.AWS.SQS.AddPermission
  ( -- * Creating a Request
    AddPermission (..),
    newAddPermission,

    -- * Request Lenses
    addPermission_queueUrl,
    addPermission_label,
    addPermission_aWSAccountIds,
    addPermission_actions,

    -- * Destructuring the Response
    AddPermissionResponse (..),
    newAddPermissionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'newAddPermission' smart constructor.
data AddPermission = AddPermission'
  { -- | The URL of the Amazon SQS queue to which permissions are added.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text,
    -- | The unique identification of the permission you\'re setting (for
    -- example, @AliceSendMessage@). Maximum 80 characters. Allowed characters
    -- include alphanumeric characters, hyphens (@-@), and underscores (@_@).
    label :: Prelude.Text,
    -- | The AWS account number of the
    -- <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal>
    -- who is given permission. The principal must have an AWS account, but
    -- does not need to be signed up for Amazon SQS. For information about
    -- locating the AWS account identification, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html#sqs-api-request-authentication Your AWS Identifiers>
    -- in the /Amazon Simple Queue Service Developer Guide/.
    aWSAccountIds :: [Prelude.Text],
    -- | The action the client wants to allow for the specified principal. Valid
    -- values: the name of any action or @*@.
    --
    -- For more information about these actions, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-overview-of-managing-access.html Overview of Managing Access Permissions to Your Amazon Simple Queue Service Resource>
    -- in the /Amazon Simple Queue Service Developer Guide/.
    --
    -- Specifying @SendMessage@, @DeleteMessage@, or @ChangeMessageVisibility@
    -- for @ActionName.n@ also grants permissions for the corresponding batch
    -- versions of those actions: @SendMessageBatch@, @DeleteMessageBatch@, and
    -- @ChangeMessageVisibilityBatch@.
    actions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'addPermission_queueUrl' - The URL of the Amazon SQS queue to which permissions are added.
--
-- Queue URLs and names are case-sensitive.
--
-- 'label', 'addPermission_label' - The unique identification of the permission you\'re setting (for
-- example, @AliceSendMessage@). Maximum 80 characters. Allowed characters
-- include alphanumeric characters, hyphens (@-@), and underscores (@_@).
--
-- 'aWSAccountIds', 'addPermission_aWSAccountIds' - The AWS account number of the
-- <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal>
-- who is given permission. The principal must have an AWS account, but
-- does not need to be signed up for Amazon SQS. For information about
-- locating the AWS account identification, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html#sqs-api-request-authentication Your AWS Identifiers>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- 'actions', 'addPermission_actions' - The action the client wants to allow for the specified principal. Valid
-- values: the name of any action or @*@.
--
-- For more information about these actions, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-overview-of-managing-access.html Overview of Managing Access Permissions to Your Amazon Simple Queue Service Resource>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- Specifying @SendMessage@, @DeleteMessage@, or @ChangeMessageVisibility@
-- for @ActionName.n@ also grants permissions for the corresponding batch
-- versions of those actions: @SendMessageBatch@, @DeleteMessageBatch@, and
-- @ChangeMessageVisibilityBatch@.
newAddPermission ::
  -- | 'queueUrl'
  Prelude.Text ->
  -- | 'label'
  Prelude.Text ->
  AddPermission
newAddPermission pQueueUrl_ pLabel_ =
  AddPermission'
    { queueUrl = pQueueUrl_,
      label = pLabel_,
      aWSAccountIds = Prelude.mempty,
      actions = Prelude.mempty
    }

-- | The URL of the Amazon SQS queue to which permissions are added.
--
-- Queue URLs and names are case-sensitive.
addPermission_queueUrl :: Lens.Lens' AddPermission Prelude.Text
addPermission_queueUrl = Lens.lens (\AddPermission' {queueUrl} -> queueUrl) (\s@AddPermission' {} a -> s {queueUrl = a} :: AddPermission)

-- | The unique identification of the permission you\'re setting (for
-- example, @AliceSendMessage@). Maximum 80 characters. Allowed characters
-- include alphanumeric characters, hyphens (@-@), and underscores (@_@).
addPermission_label :: Lens.Lens' AddPermission Prelude.Text
addPermission_label = Lens.lens (\AddPermission' {label} -> label) (\s@AddPermission' {} a -> s {label = a} :: AddPermission)

-- | The AWS account number of the
-- <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal>
-- who is given permission. The principal must have an AWS account, but
-- does not need to be signed up for Amazon SQS. For information about
-- locating the AWS account identification, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html#sqs-api-request-authentication Your AWS Identifiers>
-- in the /Amazon Simple Queue Service Developer Guide/.
addPermission_aWSAccountIds :: Lens.Lens' AddPermission [Prelude.Text]
addPermission_aWSAccountIds = Lens.lens (\AddPermission' {aWSAccountIds} -> aWSAccountIds) (\s@AddPermission' {} a -> s {aWSAccountIds = a} :: AddPermission) Prelude.. Prelude._Coerce

-- | The action the client wants to allow for the specified principal. Valid
-- values: the name of any action or @*@.
--
-- For more information about these actions, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-overview-of-managing-access.html Overview of Managing Access Permissions to Your Amazon Simple Queue Service Resource>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- Specifying @SendMessage@, @DeleteMessage@, or @ChangeMessageVisibility@
-- for @ActionName.n@ also grants permissions for the corresponding batch
-- versions of those actions: @SendMessageBatch@, @DeleteMessageBatch@, and
-- @ChangeMessageVisibilityBatch@.
addPermission_actions :: Lens.Lens' AddPermission [Prelude.Text]
addPermission_actions = Lens.lens (\AddPermission' {actions} -> actions) (\s@AddPermission' {} a -> s {actions = a} :: AddPermission) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AddPermission where
  type Rs AddPermission = AddPermissionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull AddPermissionResponse'

instance Prelude.Hashable AddPermission

instance Prelude.NFData AddPermission

instance Prelude.ToHeaders AddPermission where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AddPermission where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddPermission where
  toQuery AddPermission' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AddPermission" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Prelude.=: queueUrl,
        "Label" Prelude.=: label,
        Prelude.toQueryList "AWSAccountId" aWSAccountIds,
        Prelude.toQueryList "ActionName" actions
      ]

-- | /See:/ 'newAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddPermissionResponse ::
  AddPermissionResponse
newAddPermissionResponse = AddPermissionResponse'

instance Prelude.NFData AddPermissionResponse
