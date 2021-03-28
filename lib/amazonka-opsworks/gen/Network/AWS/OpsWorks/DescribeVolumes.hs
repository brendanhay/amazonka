{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeVolumes (..)
    , mkDescribeVolumes
    -- ** Request lenses
    , dvInstanceId
    , dvRaidArrayId
    , dvStackId
    , dvVolumeIds

    -- * Destructuring the response
    , DescribeVolumesResponse (..)
    , mkDescribeVolumesResponse
    -- ** Response lenses
    , dvrrsVolumes
    , dvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVolumes' smart constructor.
data DescribeVolumes = DescribeVolumes'
  { instanceId :: Core.Maybe Core.Text
    -- ^ The instance ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified instance.
  , raidArrayId :: Core.Maybe Core.Text
    -- ^ The RAID array ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified RAID array.
  , stackId :: Core.Maybe Core.Text
    -- ^ A stack ID. The action describes the stack's registered Amazon EBS volumes.
  , volumeIds :: Core.Maybe [Core.Text]
    -- ^ Am array of volume IDs. If you use this parameter, @DescribeVolumes@ returns descriptions of the specified volumes. Otherwise, it returns a description of every volume.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVolumes' value with any optional fields omitted.
mkDescribeVolumes
    :: DescribeVolumes
mkDescribeVolumes
  = DescribeVolumes'{instanceId = Core.Nothing,
                     raidArrayId = Core.Nothing, stackId = Core.Nothing,
                     volumeIds = Core.Nothing}

-- | The instance ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvInstanceId :: Lens.Lens' DescribeVolumes (Core.Maybe Core.Text)
dvInstanceId = Lens.field @"instanceId"
{-# INLINEABLE dvInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified RAID array.
--
-- /Note:/ Consider using 'raidArrayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvRaidArrayId :: Lens.Lens' DescribeVolumes (Core.Maybe Core.Text)
dvRaidArrayId = Lens.field @"raidArrayId"
{-# INLINEABLE dvRaidArrayId #-}
{-# DEPRECATED raidArrayId "Use generic-lens or generic-optics with 'raidArrayId' instead"  #-}

-- | A stack ID. The action describes the stack's registered Amazon EBS volumes.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvStackId :: Lens.Lens' DescribeVolumes (Core.Maybe Core.Text)
dvStackId = Lens.field @"stackId"
{-# INLINEABLE dvStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@ returns descriptions of the specified volumes. Otherwise, it returns a description of every volume.
--
-- /Note:/ Consider using 'volumeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVolumeIds :: Lens.Lens' DescribeVolumes (Core.Maybe [Core.Text])
dvVolumeIds = Lens.field @"volumeIds"
{-# INLINEABLE dvVolumeIds #-}
{-# DEPRECATED volumeIds "Use generic-lens or generic-optics with 'volumeIds' instead"  #-}

instance Core.ToQuery DescribeVolumes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeVolumes where
        toHeaders DescribeVolumes{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.DescribeVolumes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeVolumes where
        toJSON DescribeVolumes{..}
          = Core.object
              (Core.catMaybes
                 [("InstanceId" Core..=) Core.<$> instanceId,
                  ("RaidArrayId" Core..=) Core.<$> raidArrayId,
                  ("StackId" Core..=) Core.<$> stackId,
                  ("VolumeIds" Core..=) Core.<$> volumeIds])

instance Core.AWSRequest DescribeVolumes where
        type Rs DescribeVolumes = DescribeVolumesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeVolumesResponse' Core.<$>
                   (x Core..:? "Volumes") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @DescribeVolumes@ request.
--
-- /See:/ 'mkDescribeVolumesResponse' smart constructor.
data DescribeVolumesResponse = DescribeVolumesResponse'
  { volumes :: Core.Maybe [Types.Volume]
    -- ^ An array of volume IDs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVolumesResponse' value with any optional fields omitted.
mkDescribeVolumesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVolumesResponse
mkDescribeVolumesResponse responseStatus
  = DescribeVolumesResponse'{volumes = Core.Nothing, responseStatus}

-- | An array of volume IDs.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrrsVolumes :: Lens.Lens' DescribeVolumesResponse (Core.Maybe [Types.Volume])
dvrrsVolumes = Lens.field @"volumes"
{-# INLINEABLE dvrrsVolumes #-}
{-# DEPRECATED volumes "Use generic-lens or generic-optics with 'volumes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrrsResponseStatus :: Lens.Lens' DescribeVolumesResponse Core.Int
dvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
