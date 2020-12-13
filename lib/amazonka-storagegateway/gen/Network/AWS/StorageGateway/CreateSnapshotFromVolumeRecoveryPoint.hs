{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a snapshot of a gateway from a volume recovery point. This operation is only supported in the cached volume gateway type.
--
-- A volume recovery point is a point in time at which all data of the volume is consistent and from which you can create a snapshot. To get a list of volume recovery point for cached volume gateway, use 'ListVolumeRecoveryPoints' .
-- In the @CreateSnapshotFromVolumeRecoveryPoint@ request, you identify the volume by providing its Amazon Resource Name (ARN). You must also provide a description for the snapshot. When the gateway takes a snapshot of the specified volume, the snapshot and its description appear in the AWS Storage Gateway console. In response, the gateway returns you a snapshot ID. You can use this snapshot ID to check the snapshot progress or later use it when you want to create a volume from a snapshot.
module Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
  ( -- * Creating a request
    CreateSnapshotFromVolumeRecoveryPoint (..),
    mkCreateSnapshotFromVolumeRecoveryPoint,

    -- ** Request lenses
    csfvrpSnapshotDescription,
    csfvrpVolumeARN,
    csfvrpTags,

    -- * Destructuring the response
    CreateSnapshotFromVolumeRecoveryPointResponse (..),
    mkCreateSnapshotFromVolumeRecoveryPointResponse,

    -- ** Response lenses
    csfvrprsVolumeRecoveryPointTime,
    csfvrprsVolumeARN,
    csfvrprsSnapshotId,
    csfvrprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkCreateSnapshotFromVolumeRecoveryPoint' smart constructor.
data CreateSnapshotFromVolumeRecoveryPoint = CreateSnapshotFromVolumeRecoveryPoint'
  { -- | Textual description of the snapshot that appears in the Amazon EC2 console, Elastic Block Store snapshots panel in the __Description__ field, and in the AWS Storage Gateway snapshot __Details__ pane, __Description__ field.
    snapshotDescription :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
    volumeARN :: Lude.Text,
    -- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshotFromVolumeRecoveryPoint' with the minimum fields required to make a request.
--
-- * 'snapshotDescription' - Textual description of the snapshot that appears in the Amazon EC2 console, Elastic Block Store snapshots panel in the __Description__ field, and in the AWS Storage Gateway snapshot __Details__ pane, __Description__ field.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
-- * 'tags' - A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
mkCreateSnapshotFromVolumeRecoveryPoint ::
  -- | 'snapshotDescription'
  Lude.Text ->
  -- | 'volumeARN'
  Lude.Text ->
  CreateSnapshotFromVolumeRecoveryPoint
mkCreateSnapshotFromVolumeRecoveryPoint
  pSnapshotDescription_
  pVolumeARN_ =
    CreateSnapshotFromVolumeRecoveryPoint'
      { snapshotDescription =
          pSnapshotDescription_,
        volumeARN = pVolumeARN_,
        tags = Lude.Nothing
      }

-- | Textual description of the snapshot that appears in the Amazon EC2 console, Elastic Block Store snapshots panel in the __Description__ field, and in the AWS Storage Gateway snapshot __Details__ pane, __Description__ field.
--
-- /Note:/ Consider using 'snapshotDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrpSnapshotDescription :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPoint Lude.Text
csfvrpSnapshotDescription = Lens.lens (snapshotDescription :: CreateSnapshotFromVolumeRecoveryPoint -> Lude.Text) (\s a -> s {snapshotDescription = a} :: CreateSnapshotFromVolumeRecoveryPoint)
{-# DEPRECATED csfvrpSnapshotDescription "Use generic-lens or generic-optics with 'snapshotDescription' instead." #-}

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrpVolumeARN :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPoint Lude.Text
csfvrpVolumeARN = Lens.lens (volumeARN :: CreateSnapshotFromVolumeRecoveryPoint -> Lude.Text) (\s a -> s {volumeARN = a} :: CreateSnapshotFromVolumeRecoveryPoint)
{-# DEPRECATED csfvrpVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrpTags :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPoint (Lude.Maybe [Tag])
csfvrpTags = Lens.lens (tags :: CreateSnapshotFromVolumeRecoveryPoint -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateSnapshotFromVolumeRecoveryPoint)
{-# DEPRECATED csfvrpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateSnapshotFromVolumeRecoveryPoint where
  type
    Rs CreateSnapshotFromVolumeRecoveryPoint =
      CreateSnapshotFromVolumeRecoveryPointResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSnapshotFromVolumeRecoveryPointResponse'
            Lude.<$> (x Lude..?> "VolumeRecoveryPointTime")
            Lude.<*> (x Lude..?> "VolumeARN")
            Lude.<*> (x Lude..?> "SnapshotId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSnapshotFromVolumeRecoveryPoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.CreateSnapshotFromVolumeRecoveryPoint" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSnapshotFromVolumeRecoveryPoint where
  toJSON CreateSnapshotFromVolumeRecoveryPoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SnapshotDescription" Lude..= snapshotDescription),
            Lude.Just ("VolumeARN" Lude..= volumeARN),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateSnapshotFromVolumeRecoveryPoint where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSnapshotFromVolumeRecoveryPoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSnapshotFromVolumeRecoveryPointResponse' smart constructor.
data CreateSnapshotFromVolumeRecoveryPointResponse = CreateSnapshotFromVolumeRecoveryPointResponse'
  { -- | The time the volume was created from the recovery point.
    volumeRecoveryPointTime :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
    volumeARN :: Lude.Maybe Lude.Text,
    -- | The ID of the snapshot.
    snapshotId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshotFromVolumeRecoveryPointResponse' with the minimum fields required to make a request.
--
-- * 'volumeRecoveryPointTime' - The time the volume was created from the recovery point.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
-- * 'snapshotId' - The ID of the snapshot.
-- * 'responseStatus' - The response status code.
mkCreateSnapshotFromVolumeRecoveryPointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSnapshotFromVolumeRecoveryPointResponse
mkCreateSnapshotFromVolumeRecoveryPointResponse pResponseStatus_ =
  CreateSnapshotFromVolumeRecoveryPointResponse'
    { volumeRecoveryPointTime =
        Lude.Nothing,
      volumeARN = Lude.Nothing,
      snapshotId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time the volume was created from the recovery point.
--
-- /Note:/ Consider using 'volumeRecoveryPointTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrprsVolumeRecoveryPointTime :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Lude.Maybe Lude.Text)
csfvrprsVolumeRecoveryPointTime = Lens.lens (volumeRecoveryPointTime :: CreateSnapshotFromVolumeRecoveryPointResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeRecoveryPointTime = a} :: CreateSnapshotFromVolumeRecoveryPointResponse)
{-# DEPRECATED csfvrprsVolumeRecoveryPointTime "Use generic-lens or generic-optics with 'volumeRecoveryPointTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrprsVolumeARN :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Lude.Maybe Lude.Text)
csfvrprsVolumeARN = Lens.lens (volumeARN :: CreateSnapshotFromVolumeRecoveryPointResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: CreateSnapshotFromVolumeRecoveryPointResponse)
{-# DEPRECATED csfvrprsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrprsSnapshotId :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Lude.Maybe Lude.Text)
csfvrprsSnapshotId = Lens.lens (snapshotId :: CreateSnapshotFromVolumeRecoveryPointResponse -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: CreateSnapshotFromVolumeRecoveryPointResponse)
{-# DEPRECATED csfvrprsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrprsResponseStatus :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPointResponse Lude.Int
csfvrprsResponseStatus = Lens.lens (responseStatus :: CreateSnapshotFromVolumeRecoveryPointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSnapshotFromVolumeRecoveryPointResponse)
{-# DEPRECATED csfvrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
