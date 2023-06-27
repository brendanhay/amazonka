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
-- Module      : Amazonka.SQS.AddPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- in the /Amazon SQS Developer Guide/.
--
-- -   @AddPermission@ generates a policy for you. You can use
--     @ @@SetQueueAttributes@@ @ to upload your policy. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-creating-custom-policies.html Using Custom Policies with the Amazon SQS Access Policy Language>
--     in the /Amazon SQS Developer Guide/.
--
-- -   An Amazon SQS policy can have a maximum of seven actions per
--     statement.
--
-- -   To remove the ability to change queue permissions, you must deny
--     permission to the @AddPermission@, @RemovePermission@, and
--     @SetQueueAttributes@ actions in your IAM policy.
--
-- -   Amazon SQS @AddPermission@ does not support adding a non-account
--     principal.
--
-- Cross-account permissions don\'t apply to this action. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-customer-managed-policy-examples.html#grant-cross-account-permissions-to-role-and-user-name Grant cross-account permissions to a role and a username>
-- in the /Amazon SQS Developer Guide/.
module Amazonka.SQS.AddPermission
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

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
    -- | The Amazon Web Services account numbers of the
    -- <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principals>
    -- who are to receive permission. For information about locating the Amazon
    -- Web Services account identification, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html#sqs-api-request-authentication Your Amazon Web Services Identifiers>
    -- in the /Amazon SQS Developer Guide/.
    aWSAccountIds :: [Prelude.Text],
    -- | The action the client wants to allow for the specified principal. Valid
    -- values: the name of any action or @*@.
    --
    -- For more information about these actions, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-overview-of-managing-access.html Overview of Managing Access Permissions to Your Amazon Simple Queue Service Resource>
    -- in the /Amazon SQS Developer Guide/.
    --
    -- Specifying @SendMessage@, @DeleteMessage@, or @ChangeMessageVisibility@
    -- for @ActionName.n@ also grants permissions for the corresponding batch
    -- versions of those actions: @SendMessageBatch@, @DeleteMessageBatch@, and
    -- @ChangeMessageVisibilityBatch@.
    actions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'aWSAccountIds', 'addPermission_aWSAccountIds' - The Amazon Web Services account numbers of the
-- <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principals>
-- who are to receive permission. For information about locating the Amazon
-- Web Services account identification, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html#sqs-api-request-authentication Your Amazon Web Services Identifiers>
-- in the /Amazon SQS Developer Guide/.
--
-- 'actions', 'addPermission_actions' - The action the client wants to allow for the specified principal. Valid
-- values: the name of any action or @*@.
--
-- For more information about these actions, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-overview-of-managing-access.html Overview of Managing Access Permissions to Your Amazon Simple Queue Service Resource>
-- in the /Amazon SQS Developer Guide/.
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

-- | The Amazon Web Services account numbers of the
-- <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principals>
-- who are to receive permission. For information about locating the Amazon
-- Web Services account identification, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html#sqs-api-request-authentication Your Amazon Web Services Identifiers>
-- in the /Amazon SQS Developer Guide/.
addPermission_aWSAccountIds :: Lens.Lens' AddPermission [Prelude.Text]
addPermission_aWSAccountIds = Lens.lens (\AddPermission' {aWSAccountIds} -> aWSAccountIds) (\s@AddPermission' {} a -> s {aWSAccountIds = a} :: AddPermission) Prelude.. Lens.coerced

-- | The action the client wants to allow for the specified principal. Valid
-- values: the name of any action or @*@.
--
-- For more information about these actions, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-overview-of-managing-access.html Overview of Managing Access Permissions to Your Amazon Simple Queue Service Resource>
-- in the /Amazon SQS Developer Guide/.
--
-- Specifying @SendMessage@, @DeleteMessage@, or @ChangeMessageVisibility@
-- for @ActionName.n@ also grants permissions for the corresponding batch
-- versions of those actions: @SendMessageBatch@, @DeleteMessageBatch@, and
-- @ChangeMessageVisibilityBatch@.
addPermission_actions :: Lens.Lens' AddPermission [Prelude.Text]
addPermission_actions = Lens.lens (\AddPermission' {actions} -> actions) (\s@AddPermission' {} a -> s {actions = a} :: AddPermission) Prelude.. Lens.coerced

instance Core.AWSRequest AddPermission where
  type
    AWSResponse AddPermission =
      AddPermissionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull AddPermissionResponse'

instance Prelude.Hashable AddPermission where
  hashWithSalt _salt AddPermission' {..} =
    _salt
      `Prelude.hashWithSalt` queueUrl
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` aWSAccountIds
      `Prelude.hashWithSalt` actions

instance Prelude.NFData AddPermission where
  rnf AddPermission' {..} =
    Prelude.rnf queueUrl
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf aWSAccountIds
      `Prelude.seq` Prelude.rnf actions

instance Data.ToHeaders AddPermission where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AddPermission where
  toPath = Prelude.const "/"

instance Data.ToQuery AddPermission where
  toQuery AddPermission' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AddPermission" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Data.=: queueUrl,
        "Label" Data.=: label,
        Data.toQueryList "AWSAccountId" aWSAccountIds,
        Data.toQueryList "ActionName" actions
      ]

-- | /See:/ 'newAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddPermissionResponse ::
  AddPermissionResponse
newAddPermissionResponse = AddPermissionResponse'

instance Prelude.NFData AddPermissionResponse where
  rnf _ = ()
