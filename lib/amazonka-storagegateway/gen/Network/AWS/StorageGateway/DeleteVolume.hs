{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified storage volume that you previously created using the 'CreateCachediSCSIVolume' or 'CreateStorediSCSIVolume' API. This operation is only supported in the cached volume and stored volume types. For stored volume gateways, the local disk that was configured as the storage volume is not deleted. You can reuse the local disk to create another storage volume.
--
-- Before you delete a volume, make sure there are no iSCSI connections to the volume you are deleting. You should also make sure there is no snapshot in progress. You can use the Amazon Elastic Compute Cloud (Amazon EC2) API to query snapshots on the volume you are deleting and check the snapshot status. For more information, go to <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
-- In the request, you must provide the Amazon Resource Name (ARN) of the storage volume you want to delete.
module Network.AWS.StorageGateway.DeleteVolume
  ( -- * Creating a request
    DeleteVolume (..),
    mkDeleteVolume,

    -- ** Request lenses
    delVolumeARN,

    -- * Destructuring the response
    DeleteVolumeResponse (..),
    mkDeleteVolumeResponse,

    -- ** Response lenses
    dvvrsVolumeARN,
    dvvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the 'DeleteVolumeInput$VolumeARN' to delete.
--
-- /See:/ 'mkDeleteVolume' smart constructor.
newtype DeleteVolume = DeleteVolume' {volumeARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVolume' with the minimum fields required to make a request.
--
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
mkDeleteVolume ::
  -- | 'volumeARN'
  Lude.Text ->
  DeleteVolume
mkDeleteVolume pVolumeARN_ = DeleteVolume' {volumeARN = pVolumeARN_}

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delVolumeARN :: Lens.Lens' DeleteVolume Lude.Text
delVolumeARN = Lens.lens (volumeARN :: DeleteVolume -> Lude.Text) (\s a -> s {volumeARN = a} :: DeleteVolume)
{-# DEPRECATED delVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

instance Lude.AWSRequest DeleteVolume where
  type Rs DeleteVolume = DeleteVolumeResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteVolumeResponse'
            Lude.<$> (x Lude..?> "VolumeARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteVolume where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.DeleteVolume" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteVolume where
  toJSON DeleteVolume' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("VolumeARN" Lude..= volumeARN)])

instance Lude.ToPath DeleteVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVolume where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the storage volume that was deleted.
--
-- /See:/ 'mkDeleteVolumeResponse' smart constructor.
data DeleteVolumeResponse = DeleteVolumeResponse'
  { volumeARN ::
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

-- | Creates a value of 'DeleteVolumeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the storage volume that was deleted. It is the same ARN you provided in the request.
mkDeleteVolumeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteVolumeResponse
mkDeleteVolumeResponse pResponseStatus_ =
  DeleteVolumeResponse'
    { volumeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the storage volume that was deleted. It is the same ARN you provided in the request.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvvrsVolumeARN :: Lens.Lens' DeleteVolumeResponse (Lude.Maybe Lude.Text)
dvvrsVolumeARN = Lens.lens (volumeARN :: DeleteVolumeResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: DeleteVolumeResponse)
{-# DEPRECATED dvvrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvvrsResponseStatus :: Lens.Lens' DeleteVolumeResponse Lude.Int
dvvrsResponseStatus = Lens.lens (responseStatus :: DeleteVolumeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVolumeResponse)
{-# DEPRECATED dvvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
