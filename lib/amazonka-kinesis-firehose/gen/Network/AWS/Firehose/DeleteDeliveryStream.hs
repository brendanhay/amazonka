{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.DeleteDeliveryStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a delivery stream and its data.
--
-- To check the state of a delivery stream, use 'DescribeDeliveryStream' . You can delete a delivery stream only if it is in one of the following states: @ACTIVE@ , @DELETING@ , @CREATING_FAILED@ , or @DELETING_FAILED@ . You can't delete a delivery stream that is in the @CREATING@ state. While the deletion request is in process, the delivery stream is in the @DELETING@ state.
-- While the delivery stream is in the @DELETING@ state, the service might continue to accept records, but it doesn't make any guarantees with respect to delivering the data. Therefore, as a best practice, first stop any applications that are sending records before you delete a delivery stream.
module Network.AWS.Firehose.DeleteDeliveryStream
  ( -- * Creating a request
    DeleteDeliveryStream (..),
    mkDeleteDeliveryStream,

    -- ** Request lenses
    dAllowForceDelete,
    dDeliveryStreamName,

    -- * Destructuring the response
    DeleteDeliveryStreamResponse (..),
    mkDeleteDeliveryStreamResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDeliveryStream' smart constructor.
data DeleteDeliveryStream = DeleteDeliveryStream'
  { allowForceDelete ::
      Lude.Maybe Lude.Bool,
    deliveryStreamName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeliveryStream' with the minimum fields required to make a request.
--
-- * 'allowForceDelete' - Set this to true if you want to delete the delivery stream even if Kinesis Data Firehose is unable to retire the grant for the CMK. Kinesis Data Firehose might be unable to retire the grant due to a customer error, such as when the CMK or the grant are in an invalid state. If you force deletion, you can then use the <https://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html RevokeGrant> operation to revoke the grant you gave to Kinesis Data Firehose. If a failure to retire the grant happens due to an AWS KMS issue, Kinesis Data Firehose keeps retrying the delete operation.
--
-- The default value is false.
-- * 'deliveryStreamName' - The name of the delivery stream.
mkDeleteDeliveryStream ::
  -- | 'deliveryStreamName'
  Lude.Text ->
  DeleteDeliveryStream
mkDeleteDeliveryStream pDeliveryStreamName_ =
  DeleteDeliveryStream'
    { allowForceDelete = Lude.Nothing,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | Set this to true if you want to delete the delivery stream even if Kinesis Data Firehose is unable to retire the grant for the CMK. Kinesis Data Firehose might be unable to retire the grant due to a customer error, such as when the CMK or the grant are in an invalid state. If you force deletion, you can then use the <https://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html RevokeGrant> operation to revoke the grant you gave to Kinesis Data Firehose. If a failure to retire the grant happens due to an AWS KMS issue, Kinesis Data Firehose keeps retrying the delete operation.
--
-- The default value is false.
--
-- /Note:/ Consider using 'allowForceDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAllowForceDelete :: Lens.Lens' DeleteDeliveryStream (Lude.Maybe Lude.Bool)
dAllowForceDelete = Lens.lens (allowForceDelete :: DeleteDeliveryStream -> Lude.Maybe Lude.Bool) (\s a -> s {allowForceDelete = a} :: DeleteDeliveryStream)
{-# DEPRECATED dAllowForceDelete "Use generic-lens or generic-optics with 'allowForceDelete' instead." #-}

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeliveryStreamName :: Lens.Lens' DeleteDeliveryStream Lude.Text
dDeliveryStreamName = Lens.lens (deliveryStreamName :: DeleteDeliveryStream -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: DeleteDeliveryStream)
{-# DEPRECATED dDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

instance Lude.AWSRequest DeleteDeliveryStream where
  type Rs DeleteDeliveryStream = DeleteDeliveryStreamResponse
  request = Req.postJSON firehoseService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDeliveryStreamResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDeliveryStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Firehose_20150804.DeleteDeliveryStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDeliveryStream where
  toJSON DeleteDeliveryStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AllowForceDelete" Lude..=) Lude.<$> allowForceDelete,
            Lude.Just ("DeliveryStreamName" Lude..= deliveryStreamName)
          ]
      )

instance Lude.ToPath DeleteDeliveryStream where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDeliveryStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDeliveryStreamResponse' smart constructor.
newtype DeleteDeliveryStreamResponse = DeleteDeliveryStreamResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDeliveryStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDeliveryStreamResponse
mkDeleteDeliveryStreamResponse pResponseStatus_ =
  DeleteDeliveryStreamResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteDeliveryStreamResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteDeliveryStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDeliveryStreamResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
