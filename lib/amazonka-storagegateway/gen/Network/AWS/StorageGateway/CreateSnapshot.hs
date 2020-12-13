{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a snapshot of a volume.
--
-- AWS Storage Gateway provides the ability to back up point-in-time snapshots of your data to Amazon Simple Storage (Amazon S3) for durable off-site recovery, as well as import the data to an Amazon Elastic Block Store (EBS) volume in Amazon Elastic Compute Cloud (EC2). You can take snapshots of your gateway volume on a scheduled or ad hoc basis. This API enables you to take an ad hoc snapshot. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#SchedulingSnapshot Editing a snapshot schedule> .
-- In the @CreateSnapshot@ request, you identify the volume by providing its Amazon Resource Name (ARN). You must also provide description for the snapshot. When AWS Storage Gateway takes the snapshot of specified volume, the snapshot and description appears in the AWS Storage Gateway console. In response, AWS Storage Gateway returns you a snapshot ID. You can use this snapshot ID to check the snapshot progress or later use it when you want to create a volume from a snapshot. This operation is only supported in stored and cached volume gateway type.
-- /Important:/ Volume and snapshot IDs are changing to a longer length ID format. For more information, see the important note on the <https://docs.aws.amazon.com/storagegateway/latest/APIReference/Welcome.html Welcome> page.
module Network.AWS.StorageGateway.CreateSnapshot
  ( -- * Creating a request
    CreateSnapshot (..),
    mkCreateSnapshot,

    -- ** Request lenses
    csSnapshotDescription,
    csVolumeARN,
    csTags,

    -- * Destructuring the response
    CreateSnapshotResponse (..),
    mkCreateSnapshotResponse,

    -- ** Response lenses
    csrsVolumeARN,
    csrsSnapshotId,
    csrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'CreateSnapshotInput$SnapshotDescription'
--
--
--     * 'CreateSnapshotInput$VolumeARN'
--
--
--
-- /See:/ 'mkCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { -- | Textual description of the snapshot that appears in the Amazon EC2 console, Elastic Block Store snapshots panel in the __Description__ field, and in the AWS Storage Gateway snapshot __Details__ pane, __Description__ field.
    snapshotDescription :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
    volumeARN :: Lude.Text,
    -- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshot' with the minimum fields required to make a request.
--
-- * 'snapshotDescription' - Textual description of the snapshot that appears in the Amazon EC2 console, Elastic Block Store snapshots panel in the __Description__ field, and in the AWS Storage Gateway snapshot __Details__ pane, __Description__ field.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
-- * 'tags' - A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
mkCreateSnapshot ::
  -- | 'snapshotDescription'
  Lude.Text ->
  -- | 'volumeARN'
  Lude.Text ->
  CreateSnapshot
mkCreateSnapshot pSnapshotDescription_ pVolumeARN_ =
  CreateSnapshot'
    { snapshotDescription = pSnapshotDescription_,
      volumeARN = pVolumeARN_,
      tags = Lude.Nothing
    }

-- | Textual description of the snapshot that appears in the Amazon EC2 console, Elastic Block Store snapshots panel in the __Description__ field, and in the AWS Storage Gateway snapshot __Details__ pane, __Description__ field.
--
-- /Note:/ Consider using 'snapshotDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSnapshotDescription :: Lens.Lens' CreateSnapshot Lude.Text
csSnapshotDescription = Lens.lens (snapshotDescription :: CreateSnapshot -> Lude.Text) (\s a -> s {snapshotDescription = a} :: CreateSnapshot)
{-# DEPRECATED csSnapshotDescription "Use generic-lens or generic-optics with 'snapshotDescription' instead." #-}

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csVolumeARN :: Lens.Lens' CreateSnapshot Lude.Text
csVolumeARN = Lens.lens (volumeARN :: CreateSnapshot -> Lude.Text) (\s a -> s {volumeARN = a} :: CreateSnapshot)
{-# DEPRECATED csVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateSnapshot (Lude.Maybe [Tag])
csTags = Lens.lens (tags :: CreateSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateSnapshot)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateSnapshot where
  type Rs CreateSnapshot = CreateSnapshotResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSnapshotResponse'
            Lude.<$> (x Lude..?> "VolumeARN")
            Lude.<*> (x Lude..?> "SnapshotId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.CreateSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSnapshot where
  toJSON CreateSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SnapshotDescription" Lude..= snapshotDescription),
            Lude.Just ("VolumeARN" Lude..= volumeARN),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSnapshot where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { -- | The Amazon Resource Name (ARN) of the volume of which the snapshot was taken.
    volumeARN :: Lude.Maybe Lude.Text,
    -- | The snapshot ID that is used to refer to the snapshot in future operations such as describing snapshots (Amazon Elastic Compute Cloud API @DescribeSnapshots@ ) or creating a volume from a snapshot ('CreateStorediSCSIVolume' ).
    snapshotId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume of which the snapshot was taken.
-- * 'snapshotId' - The snapshot ID that is used to refer to the snapshot in future operations such as describing snapshots (Amazon Elastic Compute Cloud API @DescribeSnapshots@ ) or creating a volume from a snapshot ('CreateStorediSCSIVolume' ).
-- * 'responseStatus' - The response status code.
mkCreateSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSnapshotResponse
mkCreateSnapshotResponse pResponseStatus_ =
  CreateSnapshotResponse'
    { volumeARN = Lude.Nothing,
      snapshotId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the volume of which the snapshot was taken.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsVolumeARN :: Lens.Lens' CreateSnapshotResponse (Lude.Maybe Lude.Text)
csrsVolumeARN = Lens.lens (volumeARN :: CreateSnapshotResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: CreateSnapshotResponse)
{-# DEPRECATED csrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The snapshot ID that is used to refer to the snapshot in future operations such as describing snapshots (Amazon Elastic Compute Cloud API @DescribeSnapshots@ ) or creating a volume from a snapshot ('CreateStorediSCSIVolume' ).
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSnapshotId :: Lens.Lens' CreateSnapshotResponse (Lude.Maybe Lude.Text)
csrsSnapshotId = Lens.lens (snapshotId :: CreateSnapshotResponse -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: CreateSnapshotResponse)
{-# DEPRECATED csrsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateSnapshotResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSnapshotResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
