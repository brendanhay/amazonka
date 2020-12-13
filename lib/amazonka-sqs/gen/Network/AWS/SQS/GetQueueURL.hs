{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.GetQueueURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the URL of an existing Amazon SQS queue.
--
-- To access a queue that belongs to another AWS account, use the @QueueOwnerAWSAccountId@ parameter to specify the account ID of the queue's owner. The queue's owner must grant you permission to access the queue. For more information about shared queue access, see @'AddPermission' @ or see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-writing-an-sqs-policy.html#write-messages-to-shared-queue Allow Developers to Write Messages to a Shared Queue> in the /Amazon Simple Queue Service Developer Guide/ .
module Network.AWS.SQS.GetQueueURL
  ( -- * Creating a request
    GetQueueURL (..),
    mkGetQueueURL,

    -- ** Request lenses
    gquQueueName,
    gquQueueOwnerAWSAccountId,

    -- * Destructuring the response
    GetQueueURLResponse (..),
    mkGetQueueURLResponse,

    -- ** Response lenses
    gqursQueueURL,
    gqursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'mkGetQueueURL' smart constructor.
data GetQueueURL = GetQueueURL'
  { -- | The name of the queue whose URL must be fetched. Maximum 80 characters. Valid values: alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
    --
    -- Queue URLs and names are case-sensitive.
    queueName :: Lude.Text,
    -- | The AWS account ID of the account that created the queue.
    queueOwnerAWSAccountId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQueueURL' with the minimum fields required to make a request.
--
-- * 'queueName' - The name of the queue whose URL must be fetched. Maximum 80 characters. Valid values: alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
--
-- Queue URLs and names are case-sensitive.
-- * 'queueOwnerAWSAccountId' - The AWS account ID of the account that created the queue.
mkGetQueueURL ::
  -- | 'queueName'
  Lude.Text ->
  GetQueueURL
mkGetQueueURL pQueueName_ =
  GetQueueURL'
    { queueName = pQueueName_,
      queueOwnerAWSAccountId = Lude.Nothing
    }

-- | The name of the queue whose URL must be fetched. Maximum 80 characters. Valid values: alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gquQueueName :: Lens.Lens' GetQueueURL Lude.Text
gquQueueName = Lens.lens (queueName :: GetQueueURL -> Lude.Text) (\s a -> s {queueName = a} :: GetQueueURL)
{-# DEPRECATED gquQueueName "Use generic-lens or generic-optics with 'queueName' instead." #-}

-- | The AWS account ID of the account that created the queue.
--
-- /Note:/ Consider using 'queueOwnerAWSAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gquQueueOwnerAWSAccountId :: Lens.Lens' GetQueueURL (Lude.Maybe Lude.Text)
gquQueueOwnerAWSAccountId = Lens.lens (queueOwnerAWSAccountId :: GetQueueURL -> Lude.Maybe Lude.Text) (\s a -> s {queueOwnerAWSAccountId = a} :: GetQueueURL)
{-# DEPRECATED gquQueueOwnerAWSAccountId "Use generic-lens or generic-optics with 'queueOwnerAWSAccountId' instead." #-}

instance Lude.AWSRequest GetQueueURL where
  type Rs GetQueueURL = GetQueueURLResponse
  request = Req.postQuery sqsService
  response =
    Res.receiveXMLWrapper
      "GetQueueUrlResult"
      ( \s h x ->
          GetQueueURLResponse'
            Lude.<$> (x Lude..@ "QueueUrl") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetQueueURL where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetQueueURL where
  toPath = Lude.const "/"

instance Lude.ToQuery GetQueueURL where
  toQuery GetQueueURL' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetQueueUrl" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        "QueueName" Lude.=: queueName,
        "QueueOwnerAWSAccountId" Lude.=: queueOwnerAWSAccountId
      ]

-- | For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-api-responses.html Interpreting Responses> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /See:/ 'mkGetQueueURLResponse' smart constructor.
data GetQueueURLResponse = GetQueueURLResponse'
  { -- | The URL of the queue.
    queueURL :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQueueURLResponse' with the minimum fields required to make a request.
--
-- * 'queueURL' - The URL of the queue.
-- * 'responseStatus' - The response status code.
mkGetQueueURLResponse ::
  -- | 'queueURL'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  GetQueueURLResponse
mkGetQueueURLResponse pQueueURL_ pResponseStatus_ =
  GetQueueURLResponse'
    { queueURL = pQueueURL_,
      responseStatus = pResponseStatus_
    }

-- | The URL of the queue.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqursQueueURL :: Lens.Lens' GetQueueURLResponse Lude.Text
gqursQueueURL = Lens.lens (queueURL :: GetQueueURLResponse -> Lude.Text) (\s a -> s {queueURL = a} :: GetQueueURLResponse)
{-# DEPRECATED gqursQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqursResponseStatus :: Lens.Lens' GetQueueURLResponse Lude.Int
gqursResponseStatus = Lens.lens (responseStatus :: GetQueueURLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetQueueURLResponse)
{-# DEPRECATED gqursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
