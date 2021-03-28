{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AddPermission (..)
    , mkAddPermission
    -- ** Request lenses
    , apQueueUrl
    , apLabel
    , apAWSAccountIds
    , apActions

    -- * Destructuring the response
    , AddPermissionResponse (..)
    , mkAddPermissionResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | 
--
-- /See:/ 'mkAddPermission' smart constructor.
data AddPermission = AddPermission'
  { queueUrl :: Core.Text
    -- ^ The URL of the Amazon SQS queue to which permissions are added.
--
-- Queue URLs and names are case-sensitive.
  , label :: Core.Text
    -- ^ The unique identification of the permission you're setting (for example, @AliceSendMessage@ ). Maximum 80 characters. Allowed characters include alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
  , aWSAccountIds :: [Core.Text]
    -- ^ The AWS account number of the <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal> who is given permission. The principal must have an AWS account, but does not need to be signed up for Amazon SQS. For information about locating the AWS account identification, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html#sqs-api-request-authentication Your AWS Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
  , actions :: [Core.Text]
    -- ^ The action the client wants to allow for the specified principal. Valid values: the name of any action or @*@ .
--
-- For more information about these actions, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-overview-of-managing-access.html Overview of Managing Access Permissions to Your Amazon Simple Queue Service Resource> in the /Amazon Simple Queue Service Developer Guide/ .
-- Specifying @SendMessage@ , @DeleteMessage@ , or @ChangeMessageVisibility@ for @ActionName.n@ also grants permissions for the corresponding batch versions of those actions: @SendMessageBatch@ , @DeleteMessageBatch@ , and @ChangeMessageVisibilityBatch@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddPermission' value with any optional fields omitted.
mkAddPermission
    :: Core.Text -- ^ 'queueUrl'
    -> Core.Text -- ^ 'label'
    -> AddPermission
mkAddPermission queueUrl label
  = AddPermission'{queueUrl, label, aWSAccountIds = Core.mempty,
                   actions = Core.mempty}

-- | The URL of the Amazon SQS queue to which permissions are added.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apQueueUrl :: Lens.Lens' AddPermission Core.Text
apQueueUrl = Lens.field @"queueUrl"
{-# INLINEABLE apQueueUrl #-}
{-# DEPRECATED queueUrl "Use generic-lens or generic-optics with 'queueUrl' instead"  #-}

-- | The unique identification of the permission you're setting (for example, @AliceSendMessage@ ). Maximum 80 characters. Allowed characters include alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apLabel :: Lens.Lens' AddPermission Core.Text
apLabel = Lens.field @"label"
{-# INLINEABLE apLabel #-}
{-# DEPRECATED label "Use generic-lens or generic-optics with 'label' instead"  #-}

-- | The AWS account number of the <https://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal> who is given permission. The principal must have an AWS account, but does not need to be signed up for Amazon SQS. For information about locating the AWS account identification, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-making-api-requests.html#sqs-api-request-authentication Your AWS Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'aWSAccountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAWSAccountIds :: Lens.Lens' AddPermission [Core.Text]
apAWSAccountIds = Lens.field @"aWSAccountIds"
{-# INLINEABLE apAWSAccountIds #-}
{-# DEPRECATED aWSAccountIds "Use generic-lens or generic-optics with 'aWSAccountIds' instead"  #-}

-- | The action the client wants to allow for the specified principal. Valid values: the name of any action or @*@ .
--
-- For more information about these actions, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-overview-of-managing-access.html Overview of Managing Access Permissions to Your Amazon Simple Queue Service Resource> in the /Amazon Simple Queue Service Developer Guide/ .
-- Specifying @SendMessage@ , @DeleteMessage@ , or @ChangeMessageVisibility@ for @ActionName.n@ also grants permissions for the corresponding batch versions of those actions: @SendMessageBatch@ , @DeleteMessageBatch@ , and @ChangeMessageVisibilityBatch@ .
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apActions :: Lens.Lens' AddPermission [Core.Text]
apActions = Lens.field @"actions"
{-# INLINEABLE apActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

instance Core.ToQuery AddPermission where
        toQuery AddPermission{..}
          = Core.toQueryPair "Action" ("AddPermission" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-11-05" :: Core.Text)
              Core.<> Core.toQueryPair "QueueUrl" queueUrl
              Core.<> Core.toQueryPair "Label" label
              Core.<> Core.toQueryList "AWSAccountId" aWSAccountIds
              Core.<> Core.toQueryList "ActionName" actions

instance Core.ToHeaders AddPermission where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AddPermission where
        type Rs AddPermission = AddPermissionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull AddPermissionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddPermissionResponse' value with any optional fields omitted.
mkAddPermissionResponse
    :: AddPermissionResponse
mkAddPermissionResponse = AddPermissionResponse'
