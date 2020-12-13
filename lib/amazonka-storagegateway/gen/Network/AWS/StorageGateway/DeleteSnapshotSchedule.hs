{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteSnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a snapshot of a volume.
--
-- You can take snapshots of your gateway volumes on a scheduled or ad hoc basis. This API action enables you to delete a snapshot schedule for a volume. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/backing-up-volumes.html Backing up your volumes> . In the @DeleteSnapshotSchedule@ request, you identify the volume by providing its Amazon Resource Name (ARN). This operation is only supported in stored and cached volume gateway types.
module Network.AWS.StorageGateway.DeleteSnapshotSchedule
  ( -- * Creating a request
    DeleteSnapshotSchedule (..),
    mkDeleteSnapshotSchedule,

    -- ** Request lenses
    dssfVolumeARN,

    -- * Destructuring the response
    DeleteSnapshotScheduleResponse (..),
    mkDeleteSnapshotScheduleResponse,

    -- ** Response lenses
    dssrsVolumeARN,
    dssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkDeleteSnapshotSchedule' smart constructor.
newtype DeleteSnapshotSchedule = DeleteSnapshotSchedule'
  { -- | The volume which snapshot schedule to delete.
    volumeARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSnapshotSchedule' with the minimum fields required to make a request.
--
-- * 'volumeARN' - The volume which snapshot schedule to delete.
mkDeleteSnapshotSchedule ::
  -- | 'volumeARN'
  Lude.Text ->
  DeleteSnapshotSchedule
mkDeleteSnapshotSchedule pVolumeARN_ =
  DeleteSnapshotSchedule' {volumeARN = pVolumeARN_}

-- | The volume which snapshot schedule to delete.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssfVolumeARN :: Lens.Lens' DeleteSnapshotSchedule Lude.Text
dssfVolumeARN = Lens.lens (volumeARN :: DeleteSnapshotSchedule -> Lude.Text) (\s a -> s {volumeARN = a} :: DeleteSnapshotSchedule)
{-# DEPRECATED dssfVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

instance Lude.AWSRequest DeleteSnapshotSchedule where
  type Rs DeleteSnapshotSchedule = DeleteSnapshotScheduleResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSnapshotScheduleResponse'
            Lude.<$> (x Lude..?> "VolumeARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSnapshotSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DeleteSnapshotSchedule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSnapshotSchedule where
  toJSON DeleteSnapshotSchedule' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("VolumeARN" Lude..= volumeARN)])

instance Lude.ToPath DeleteSnapshotSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSnapshotSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSnapshotScheduleResponse' smart constructor.
data DeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'
  { -- | The volume which snapshot schedule was deleted.
    volumeARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSnapshotScheduleResponse' with the minimum fields required to make a request.
--
-- * 'volumeARN' - The volume which snapshot schedule was deleted.
-- * 'responseStatus' - The response status code.
mkDeleteSnapshotScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSnapshotScheduleResponse
mkDeleteSnapshotScheduleResponse pResponseStatus_ =
  DeleteSnapshotScheduleResponse'
    { volumeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The volume which snapshot schedule was deleted.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsVolumeARN :: Lens.Lens' DeleteSnapshotScheduleResponse (Lude.Maybe Lude.Text)
dssrsVolumeARN = Lens.lens (volumeARN :: DeleteSnapshotScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: DeleteSnapshotScheduleResponse)
{-# DEPRECATED dssrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DeleteSnapshotScheduleResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DeleteSnapshotScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSnapshotScheduleResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
