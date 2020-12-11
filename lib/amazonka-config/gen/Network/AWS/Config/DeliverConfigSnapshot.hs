{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeliverConfigSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules delivery of a configuration snapshot to the Amazon S3 bucket in the specified delivery channel. After the delivery has started, AWS Config sends the following notifications using an Amazon SNS topic that you have specified.
--
--
--     * Notification of the start of the delivery.
--
--
--     * Notification of the completion of the delivery, if the delivery was successfully completed.
--
--
--     * Notification of delivery failure, if the delivery failed.
module Network.AWS.Config.DeliverConfigSnapshot
  ( -- * Creating a request
    DeliverConfigSnapshot (..),
    mkDeliverConfigSnapshot,

    -- ** Request lenses
    dcsDeliveryChannelName,

    -- * Destructuring the response
    DeliverConfigSnapshotResponse (..),
    mkDeliverConfigSnapshotResponse,

    -- ** Response lenses
    dcsrsConfigSnapshotId,
    dcsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'DeliverConfigSnapshot' action.
--
-- /See:/ 'mkDeliverConfigSnapshot' smart constructor.
newtype DeliverConfigSnapshot = DeliverConfigSnapshot'
  { deliveryChannelName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeliverConfigSnapshot' with the minimum fields required to make a request.
--
-- * 'deliveryChannelName' - The name of the delivery channel through which the snapshot is delivered.
mkDeliverConfigSnapshot ::
  -- | 'deliveryChannelName'
  Lude.Text ->
  DeliverConfigSnapshot
mkDeliverConfigSnapshot pDeliveryChannelName_ =
  DeliverConfigSnapshot'
    { deliveryChannelName =
        pDeliveryChannelName_
    }

-- | The name of the delivery channel through which the snapshot is delivered.
--
-- /Note:/ Consider using 'deliveryChannelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsDeliveryChannelName :: Lens.Lens' DeliverConfigSnapshot Lude.Text
dcsDeliveryChannelName = Lens.lens (deliveryChannelName :: DeliverConfigSnapshot -> Lude.Text) (\s a -> s {deliveryChannelName = a} :: DeliverConfigSnapshot)
{-# DEPRECATED dcsDeliveryChannelName "Use generic-lens or generic-optics with 'deliveryChannelName' instead." #-}

instance Lude.AWSRequest DeliverConfigSnapshot where
  type Rs DeliverConfigSnapshot = DeliverConfigSnapshotResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeliverConfigSnapshotResponse'
            Lude.<$> (x Lude..?> "configSnapshotId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeliverConfigSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.DeliverConfigSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeliverConfigSnapshot where
  toJSON DeliverConfigSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("deliveryChannelName" Lude..= deliveryChannelName)]
      )

instance Lude.ToPath DeliverConfigSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery DeliverConfigSnapshot where
  toQuery = Lude.const Lude.mempty

-- | The output for the 'DeliverConfigSnapshot' action, in JSON format.
--
-- /See:/ 'mkDeliverConfigSnapshotResponse' smart constructor.
data DeliverConfigSnapshotResponse = DeliverConfigSnapshotResponse'
  { configSnapshotId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeliverConfigSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'configSnapshotId' - The ID of the snapshot that is being created.
-- * 'responseStatus' - The response status code.
mkDeliverConfigSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeliverConfigSnapshotResponse
mkDeliverConfigSnapshotResponse pResponseStatus_ =
  DeliverConfigSnapshotResponse'
    { configSnapshotId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the snapshot that is being created.
--
-- /Note:/ Consider using 'configSnapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsConfigSnapshotId :: Lens.Lens' DeliverConfigSnapshotResponse (Lude.Maybe Lude.Text)
dcsrsConfigSnapshotId = Lens.lens (configSnapshotId :: DeliverConfigSnapshotResponse -> Lude.Maybe Lude.Text) (\s a -> s {configSnapshotId = a} :: DeliverConfigSnapshotResponse)
{-# DEPRECATED dcsrsConfigSnapshotId "Use generic-lens or generic-optics with 'configSnapshotId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsResponseStatus :: Lens.Lens' DeliverConfigSnapshotResponse Lude.Int
dcsrsResponseStatus = Lens.lens (responseStatus :: DeliverConfigSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeliverConfigSnapshotResponse)
{-# DEPRECATED dcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
