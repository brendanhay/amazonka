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
    csfvrpVolumeARN,
    csfvrpSnapshotDescription,
    csfvrpTags,

    -- * Destructuring the response
    CreateSnapshotFromVolumeRecoveryPointResponse (..),
    mkCreateSnapshotFromVolumeRecoveryPointResponse,

    -- ** Response lenses
    csfvrprrsSnapshotId,
    csfvrprrsVolumeARN,
    csfvrprrsVolumeRecoveryPointTime,
    csfvrprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkCreateSnapshotFromVolumeRecoveryPoint' smart constructor.
data CreateSnapshotFromVolumeRecoveryPoint = CreateSnapshotFromVolumeRecoveryPoint'
  { -- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
    volumeARN :: Types.VolumeARN,
    -- | Textual description of the snapshot that appears in the Amazon EC2 console, Elastic Block Store snapshots panel in the __Description__ field, and in the AWS Storage Gateway snapshot __Details__ pane, __Description__ field.
    snapshotDescription :: Types.SnapshotDescription,
    -- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshotFromVolumeRecoveryPoint' value with any optional fields omitted.
mkCreateSnapshotFromVolumeRecoveryPoint ::
  -- | 'volumeARN'
  Types.VolumeARN ->
  -- | 'snapshotDescription'
  Types.SnapshotDescription ->
  CreateSnapshotFromVolumeRecoveryPoint
mkCreateSnapshotFromVolumeRecoveryPoint
  volumeARN
  snapshotDescription =
    CreateSnapshotFromVolumeRecoveryPoint'
      { volumeARN,
        snapshotDescription,
        tags = Core.Nothing
      }

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrpVolumeARN :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPoint Types.VolumeARN
csfvrpVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED csfvrpVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | Textual description of the snapshot that appears in the Amazon EC2 console, Elastic Block Store snapshots panel in the __Description__ field, and in the AWS Storage Gateway snapshot __Details__ pane, __Description__ field.
--
-- /Note:/ Consider using 'snapshotDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrpSnapshotDescription :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPoint Types.SnapshotDescription
csfvrpSnapshotDescription = Lens.field @"snapshotDescription"
{-# DEPRECATED csfvrpSnapshotDescription "Use generic-lens or generic-optics with 'snapshotDescription' instead." #-}

-- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrpTags :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPoint (Core.Maybe [Types.Tag])
csfvrpTags = Lens.field @"tags"
{-# DEPRECATED csfvrpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateSnapshotFromVolumeRecoveryPoint where
  toJSON CreateSnapshotFromVolumeRecoveryPoint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VolumeARN" Core..= volumeARN),
            Core.Just ("SnapshotDescription" Core..= snapshotDescription),
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateSnapshotFromVolumeRecoveryPoint where
  type
    Rs CreateSnapshotFromVolumeRecoveryPoint =
      CreateSnapshotFromVolumeRecoveryPointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.CreateSnapshotFromVolumeRecoveryPoint"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSnapshotFromVolumeRecoveryPointResponse'
            Core.<$> (x Core..:? "SnapshotId")
            Core.<*> (x Core..:? "VolumeARN")
            Core.<*> (x Core..:? "VolumeRecoveryPointTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSnapshotFromVolumeRecoveryPointResponse' smart constructor.
data CreateSnapshotFromVolumeRecoveryPointResponse = CreateSnapshotFromVolumeRecoveryPointResponse'
  { -- | The ID of the snapshot.
    snapshotId :: Core.Maybe Types.SnapshotId,
    -- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
    volumeARN :: Core.Maybe Types.VolumeARN,
    -- | The time the volume was created from the recovery point.
    volumeRecoveryPointTime :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshotFromVolumeRecoveryPointResponse' value with any optional fields omitted.
mkCreateSnapshotFromVolumeRecoveryPointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSnapshotFromVolumeRecoveryPointResponse
mkCreateSnapshotFromVolumeRecoveryPointResponse responseStatus =
  CreateSnapshotFromVolumeRecoveryPointResponse'
    { snapshotId =
        Core.Nothing,
      volumeARN = Core.Nothing,
      volumeRecoveryPointTime = Core.Nothing,
      responseStatus
    }

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrprrsSnapshotId :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Core.Maybe Types.SnapshotId)
csfvrprrsSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED csfvrprrsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrprrsVolumeARN :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Core.Maybe Types.VolumeARN)
csfvrprrsVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED csfvrprrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The time the volume was created from the recovery point.
--
-- /Note:/ Consider using 'volumeRecoveryPointTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrprrsVolumeRecoveryPointTime :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Core.Maybe Types.String)
csfvrprrsVolumeRecoveryPointTime = Lens.field @"volumeRecoveryPointTime"
{-# DEPRECATED csfvrprrsVolumeRecoveryPointTime "Use generic-lens or generic-optics with 'volumeRecoveryPointTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfvrprrsResponseStatus :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPointResponse Core.Int
csfvrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csfvrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
