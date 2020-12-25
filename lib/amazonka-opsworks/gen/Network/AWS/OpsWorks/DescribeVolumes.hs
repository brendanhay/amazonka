{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeVolumes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an instance's Amazon EBS volumes.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeVolumes
  ( -- * Creating a request
    DescribeVolumes (..),
    mkDescribeVolumes,

    -- ** Request lenses
    dvInstanceId,
    dvRaidArrayId,
    dvStackId,
    dvVolumeIds,

    -- * Destructuring the response
    DescribeVolumesResponse (..),
    mkDescribeVolumesResponse,

    -- ** Response lenses
    dvrrsVolumes,
    dvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVolumes' smart constructor.
data DescribeVolumes = DescribeVolumes'
  { -- | The instance ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified instance.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified RAID array.
    raidArrayId :: Core.Maybe Types.RaidArrayId,
    -- | A stack ID. The action describes the stack's registered Amazon EBS volumes.
    stackId :: Core.Maybe Types.StackId,
    -- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@ returns descriptions of the specified volumes. Otherwise, it returns a description of every volume.
    volumeIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVolumes' value with any optional fields omitted.
mkDescribeVolumes ::
  DescribeVolumes
mkDescribeVolumes =
  DescribeVolumes'
    { instanceId = Core.Nothing,
      raidArrayId = Core.Nothing,
      stackId = Core.Nothing,
      volumeIds = Core.Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvInstanceId :: Lens.Lens' DescribeVolumes (Core.Maybe Types.InstanceId)
dvInstanceId = Lens.field @"instanceId"
{-# DEPRECATED dvInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified RAID array.
--
-- /Note:/ Consider using 'raidArrayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvRaidArrayId :: Lens.Lens' DescribeVolumes (Core.Maybe Types.RaidArrayId)
dvRaidArrayId = Lens.field @"raidArrayId"
{-# DEPRECATED dvRaidArrayId "Use generic-lens or generic-optics with 'raidArrayId' instead." #-}

-- | A stack ID. The action describes the stack's registered Amazon EBS volumes.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvStackId :: Lens.Lens' DescribeVolumes (Core.Maybe Types.StackId)
dvStackId = Lens.field @"stackId"
{-# DEPRECATED dvStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@ returns descriptions of the specified volumes. Otherwise, it returns a description of every volume.
--
-- /Note:/ Consider using 'volumeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVolumeIds :: Lens.Lens' DescribeVolumes (Core.Maybe [Types.String])
dvVolumeIds = Lens.field @"volumeIds"
{-# DEPRECATED dvVolumeIds "Use generic-lens or generic-optics with 'volumeIds' instead." #-}

instance Core.FromJSON DescribeVolumes where
  toJSON DescribeVolumes {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceId" Core..=) Core.<$> instanceId,
            ("RaidArrayId" Core..=) Core.<$> raidArrayId,
            ("StackId" Core..=) Core.<$> stackId,
            ("VolumeIds" Core..=) Core.<$> volumeIds
          ]
      )

instance Core.AWSRequest DescribeVolumes where
  type Rs DescribeVolumes = DescribeVolumesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.DescribeVolumes")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVolumesResponse'
            Core.<$> (x Core..:? "Volumes") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribeVolumes@ request.
--
-- /See:/ 'mkDescribeVolumesResponse' smart constructor.
data DescribeVolumesResponse = DescribeVolumesResponse'
  { -- | An array of volume IDs.
    volumes :: Core.Maybe [Types.Volume],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVolumesResponse' value with any optional fields omitted.
mkDescribeVolumesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeVolumesResponse
mkDescribeVolumesResponse responseStatus =
  DescribeVolumesResponse' {volumes = Core.Nothing, responseStatus}

-- | An array of volume IDs.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrrsVolumes :: Lens.Lens' DescribeVolumesResponse (Core.Maybe [Types.Volume])
dvrrsVolumes = Lens.field @"volumes"
{-# DEPRECATED dvrrsVolumes "Use generic-lens or generic-optics with 'volumes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrrsResponseStatus :: Lens.Lens' DescribeVolumesResponse Core.Int
dvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
