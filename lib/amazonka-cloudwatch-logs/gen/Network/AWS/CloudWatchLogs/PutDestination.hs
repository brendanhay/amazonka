{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a destination. This operation is used only to create destinations for cross-account subscriptions.
--
-- A destination encapsulates a physical resource (such as an Amazon Kinesis stream) and enables you to subscribe to a real-time stream of log events for a different account, ingested using <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html PutLogEvents> .
-- Through an access policy, a destination controls what is written to it. By default, @PutDestination@ does not set any access policy with the destination, which means a cross-account user cannot call <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutSubscriptionFilter.html PutSubscriptionFilter> against this destination. To enable this, the destination owner must call <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutDestinationPolicy.html PutDestinationPolicy> after @PutDestination@ .
-- To perform a @PutDestination@ operation, you must also have the @iam:PassRole@ permission.
module Network.AWS.CloudWatchLogs.PutDestination
  ( -- * Creating a request
    PutDestination (..),
    mkPutDestination,

    -- ** Request lenses
    pdTargetARN,
    pdDestinationName,
    pdRoleARN,

    -- * Destructuring the response
    PutDestinationResponse (..),
    mkPutDestinationResponse,

    -- ** Response lenses
    pdrsDestination,
    pdrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutDestination' smart constructor.
data PutDestination = PutDestination'
  { -- | The ARN of an Amazon Kinesis stream to which to deliver matching log events.
    targetARN :: Lude.Text,
    -- | A name for the destination.
    destinationName :: Lude.Text,
    -- | The ARN of an IAM role that grants CloudWatch Logs permissions to call the Amazon Kinesis @PutRecord@ operation on the destination stream.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutDestination' with the minimum fields required to make a request.
--
-- * 'targetARN' - The ARN of an Amazon Kinesis stream to which to deliver matching log events.
-- * 'destinationName' - A name for the destination.
-- * 'roleARN' - The ARN of an IAM role that grants CloudWatch Logs permissions to call the Amazon Kinesis @PutRecord@ operation on the destination stream.
mkPutDestination ::
  -- | 'targetARN'
  Lude.Text ->
  -- | 'destinationName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  PutDestination
mkPutDestination pTargetARN_ pDestinationName_ pRoleARN_ =
  PutDestination'
    { targetARN = pTargetARN_,
      destinationName = pDestinationName_,
      roleARN = pRoleARN_
    }

-- | The ARN of an Amazon Kinesis stream to which to deliver matching log events.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdTargetARN :: Lens.Lens' PutDestination Lude.Text
pdTargetARN = Lens.lens (targetARN :: PutDestination -> Lude.Text) (\s a -> s {targetARN = a} :: PutDestination)
{-# DEPRECATED pdTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | A name for the destination.
--
-- /Note:/ Consider using 'destinationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDestinationName :: Lens.Lens' PutDestination Lude.Text
pdDestinationName = Lens.lens (destinationName :: PutDestination -> Lude.Text) (\s a -> s {destinationName = a} :: PutDestination)
{-# DEPRECATED pdDestinationName "Use generic-lens or generic-optics with 'destinationName' instead." #-}

-- | The ARN of an IAM role that grants CloudWatch Logs permissions to call the Amazon Kinesis @PutRecord@ operation on the destination stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdRoleARN :: Lens.Lens' PutDestination Lude.Text
pdRoleARN = Lens.lens (roleARN :: PutDestination -> Lude.Text) (\s a -> s {roleARN = a} :: PutDestination)
{-# DEPRECATED pdRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest PutDestination where
  type Rs PutDestination = PutDestinationResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutDestinationResponse'
            Lude.<$> (x Lude..?> "destination") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutDestination where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.PutDestination" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutDestination where
  toJSON PutDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("targetArn" Lude..= targetARN),
            Lude.Just ("destinationName" Lude..= destinationName),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath PutDestination where
  toPath = Lude.const "/"

instance Lude.ToQuery PutDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutDestinationResponse' smart constructor.
data PutDestinationResponse = PutDestinationResponse'
  { -- | The destination.
    destination :: Lude.Maybe Destination,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutDestinationResponse' with the minimum fields required to make a request.
--
-- * 'destination' - The destination.
-- * 'responseStatus' - The response status code.
mkPutDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutDestinationResponse
mkPutDestinationResponse pResponseStatus_ =
  PutDestinationResponse'
    { destination = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The destination.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdrsDestination :: Lens.Lens' PutDestinationResponse (Lude.Maybe Destination)
pdrsDestination = Lens.lens (destination :: PutDestinationResponse -> Lude.Maybe Destination) (\s a -> s {destination = a} :: PutDestinationResponse)
{-# DEPRECATED pdrsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdrsResponseStatus :: Lens.Lens' PutDestinationResponse Lude.Int
pdrsResponseStatus = Lens.lens (responseStatus :: PutDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutDestinationResponse)
{-# DEPRECATED pdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
