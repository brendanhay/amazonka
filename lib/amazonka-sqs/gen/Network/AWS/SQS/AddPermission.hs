{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.AddPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a permission to a queue for a specific <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal> . This allows sharing access to the queue.
--
-- When you create a queue, you have full control access rights for the queue. Only you, the owner of the queue, can grant or deny permissions to the queue. For more information about these permissions, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-writing-an-sqs-policy.html#write-messages-to-shared-queue Allow Developers to Write Messages to a Shared Queue> in the /Amazon Simple Queue Service Developer Guide/ .
-- Some actions take lists of parameters. These lists are specified using the @param.n@ notation. Values of @n@ are integers starting from 1. For example, a parameter list with two elements looks like this:
-- @&AttributeName.1=first@
-- @&AttributeName.2=second@
module Network.AWS.SQS.AddPermission
  ( -- * Creating a request
    AddPermission (..),
    mkAddPermission,

    -- ** Request lenses
    apAWSAccountIds,
    apActions,
    apQueueURL,
    apLabel,

    -- * Destructuring the response
    AddPermissionResponse (..),
    mkAddPermissionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'mkAddPermission' smart constructor.
data AddPermission = AddPermission'
  { -- | The AWS account number of the <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal> who is given permission. The principal must have an AWS account, but does not need to be signed up for Amazon SQS. For information about locating the AWS account identification, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html#sqs-api-request-authentication Your AWS Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
    awsAccountIds :: [Lude.Text],
    -- | The action the client wants to allow for the specified principal. Valid values: the name of any action or @*@ .
    --
    -- For more information about these actions, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-overview-of-managing-access.html Overview of Managing Access Permissions to Your Amazon Simple Queue Service Resource> in the /Amazon Simple Queue Service Developer Guide/ .
    -- Specifying @SendMessage@ , @DeleteMessage@ , or @ChangeMessageVisibility@ for @ActionName.n@ also grants permissions for the corresponding batch versions of those actions: @SendMessageBatch@ , @DeleteMessageBatch@ , and @ChangeMessageVisibilityBatch@ .
    actions :: [Lude.Text],
    -- | The URL of the Amazon SQS queue to which permissions are added.
    --
    -- Queue URLs and names are case-sensitive.
    queueURL :: Lude.Text,
    -- | The unique identification of the permission you're setting (for example, @AliceSendMessage@ ). Maximum 80 characters. Allowed characters include alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
    label :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddPermission' with the minimum fields required to make a request.
--
-- * 'awsAccountIds' - The AWS account number of the <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal> who is given permission. The principal must have an AWS account, but does not need to be signed up for Amazon SQS. For information about locating the AWS account identification, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html#sqs-api-request-authentication Your AWS Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
-- * 'actions' - The action the client wants to allow for the specified principal. Valid values: the name of any action or @*@ .
--
-- For more information about these actions, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-overview-of-managing-access.html Overview of Managing Access Permissions to Your Amazon Simple Queue Service Resource> in the /Amazon Simple Queue Service Developer Guide/ .
-- Specifying @SendMessage@ , @DeleteMessage@ , or @ChangeMessageVisibility@ for @ActionName.n@ also grants permissions for the corresponding batch versions of those actions: @SendMessageBatch@ , @DeleteMessageBatch@ , and @ChangeMessageVisibilityBatch@ .
-- * 'queueURL' - The URL of the Amazon SQS queue to which permissions are added.
--
-- Queue URLs and names are case-sensitive.
-- * 'label' - The unique identification of the permission you're setting (for example, @AliceSendMessage@ ). Maximum 80 characters. Allowed characters include alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
mkAddPermission ::
  -- | 'queueURL'
  Lude.Text ->
  -- | 'label'
  Lude.Text ->
  AddPermission
mkAddPermission pQueueURL_ pLabel_ =
  AddPermission'
    { awsAccountIds = Lude.mempty,
      actions = Lude.mempty,
      queueURL = pQueueURL_,
      label = pLabel_
    }

-- | The AWS account number of the <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal> who is given permission. The principal must have an AWS account, but does not need to be signed up for Amazon SQS. For information about locating the AWS account identification, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html#sqs-api-request-authentication Your AWS Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'awsAccountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAWSAccountIds :: Lens.Lens' AddPermission [Lude.Text]
apAWSAccountIds = Lens.lens (awsAccountIds :: AddPermission -> [Lude.Text]) (\s a -> s {awsAccountIds = a} :: AddPermission)
{-# DEPRECATED apAWSAccountIds "Use generic-lens or generic-optics with 'awsAccountIds' instead." #-}

-- | The action the client wants to allow for the specified principal. Valid values: the name of any action or @*@ .
--
-- For more information about these actions, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-overview-of-managing-access.html Overview of Managing Access Permissions to Your Amazon Simple Queue Service Resource> in the /Amazon Simple Queue Service Developer Guide/ .
-- Specifying @SendMessage@ , @DeleteMessage@ , or @ChangeMessageVisibility@ for @ActionName.n@ also grants permissions for the corresponding batch versions of those actions: @SendMessageBatch@ , @DeleteMessageBatch@ , and @ChangeMessageVisibilityBatch@ .
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apActions :: Lens.Lens' AddPermission [Lude.Text]
apActions = Lens.lens (actions :: AddPermission -> [Lude.Text]) (\s a -> s {actions = a} :: AddPermission)
{-# DEPRECATED apActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The URL of the Amazon SQS queue to which permissions are added.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apQueueURL :: Lens.Lens' AddPermission Lude.Text
apQueueURL = Lens.lens (queueURL :: AddPermission -> Lude.Text) (\s a -> s {queueURL = a} :: AddPermission)
{-# DEPRECATED apQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

-- | The unique identification of the permission you're setting (for example, @AliceSendMessage@ ). Maximum 80 characters. Allowed characters include alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apLabel :: Lens.Lens' AddPermission Lude.Text
apLabel = Lens.lens (label :: AddPermission -> Lude.Text) (\s a -> s {label = a} :: AddPermission)
{-# DEPRECATED apLabel "Use generic-lens or generic-optics with 'label' instead." #-}

instance Lude.AWSRequest AddPermission where
  type Rs AddPermission = AddPermissionResponse
  request = Req.postQuery sqsService
  response = Res.receiveNull AddPermissionResponse'

instance Lude.ToHeaders AddPermission where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AddPermission where
  toPath = Lude.const "/"

instance Lude.ToQuery AddPermission where
  toQuery AddPermission' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AddPermission" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        Lude.toQueryList "AWSAccountId" awsAccountIds,
        Lude.toQueryList "ActionName" actions,
        "QueueUrl" Lude.=: queueURL,
        "Label" Lude.=: label
      ]

-- | /See:/ 'mkAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddPermissionResponse' with the minimum fields required to make a request.
mkAddPermissionResponse ::
  AddPermissionResponse
mkAddPermissionResponse = AddPermissionResponse'
