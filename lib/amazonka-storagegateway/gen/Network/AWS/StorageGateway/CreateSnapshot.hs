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
    csVolumeARN,
    csSnapshotDescription,
    csTags,

    -- * Destructuring the response
    CreateSnapshotResponse (..),
    mkCreateSnapshotResponse,

    -- ** Response lenses
    csrrsSnapshotId,
    csrrsVolumeARN,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

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
  { -- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
    volumeARN :: Types.VolumeARN,
    -- | Textual description of the snapshot that appears in the Amazon EC2 console, Elastic Block Store snapshots panel in the __Description__ field, and in the AWS Storage Gateway snapshot __Details__ pane, __Description__ field.
    snapshotDescription :: Types.SnapshotDescription,
    -- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshot' value with any optional fields omitted.
mkCreateSnapshot ::
  -- | 'volumeARN'
  Types.VolumeARN ->
  -- | 'snapshotDescription'
  Types.SnapshotDescription ->
  CreateSnapshot
mkCreateSnapshot volumeARN snapshotDescription =
  CreateSnapshot'
    { volumeARN,
      snapshotDescription,
      tags = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csVolumeARN :: Lens.Lens' CreateSnapshot Types.VolumeARN
csVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED csVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | Textual description of the snapshot that appears in the Amazon EC2 console, Elastic Block Store snapshots panel in the __Description__ field, and in the AWS Storage Gateway snapshot __Details__ pane, __Description__ field.
--
-- /Note:/ Consider using 'snapshotDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSnapshotDescription :: Lens.Lens' CreateSnapshot Types.SnapshotDescription
csSnapshotDescription = Lens.field @"snapshotDescription"
{-# DEPRECATED csSnapshotDescription "Use generic-lens or generic-optics with 'snapshotDescription' instead." #-}

-- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateSnapshot (Core.Maybe [Types.Tag])
csTags = Lens.field @"tags"
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateSnapshot where
  toJSON CreateSnapshot {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VolumeARN" Core..= volumeARN),
            Core.Just ("SnapshotDescription" Core..= snapshotDescription),
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateSnapshot where
  type Rs CreateSnapshot = CreateSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.CreateSnapshot")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSnapshotResponse'
            Core.<$> (x Core..:? "SnapshotId")
            Core.<*> (x Core..:? "VolumeARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { -- | The snapshot ID that is used to refer to the snapshot in future operations such as describing snapshots (Amazon Elastic Compute Cloud API @DescribeSnapshots@ ) or creating a volume from a snapshot ('CreateStorediSCSIVolume' ).
    snapshotId :: Core.Maybe Types.SnapshotId,
    -- | The Amazon Resource Name (ARN) of the volume of which the snapshot was taken.
    volumeARN :: Core.Maybe Types.VolumeARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshotResponse' value with any optional fields omitted.
mkCreateSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSnapshotResponse
mkCreateSnapshotResponse responseStatus =
  CreateSnapshotResponse'
    { snapshotId = Core.Nothing,
      volumeARN = Core.Nothing,
      responseStatus
    }

-- | The snapshot ID that is used to refer to the snapshot in future operations such as describing snapshots (Amazon Elastic Compute Cloud API @DescribeSnapshots@ ) or creating a volume from a snapshot ('CreateStorediSCSIVolume' ).
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSnapshotId :: Lens.Lens' CreateSnapshotResponse (Core.Maybe Types.SnapshotId)
csrrsSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED csrrsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The Amazon Resource Name (ARN) of the volume of which the snapshot was taken.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsVolumeARN :: Lens.Lens' CreateSnapshotResponse (Core.Maybe Types.VolumeARN)
csrrsVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED csrrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSnapshotResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
