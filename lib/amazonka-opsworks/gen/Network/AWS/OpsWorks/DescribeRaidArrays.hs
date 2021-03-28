{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeRaidArrays
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an instance's RAID arrays.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeRaidArrays
    (
    -- * Creating a request
      DescribeRaidArrays (..)
    , mkDescribeRaidArrays
    -- ** Request lenses
    , draInstanceId
    , draRaidArrayIds
    , draStackId

    -- * Destructuring the response
    , DescribeRaidArraysResponse (..)
    , mkDescribeRaidArraysResponse
    -- ** Response lenses
    , drarrsRaidArrays
    , drarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRaidArrays' smart constructor.
data DescribeRaidArrays = DescribeRaidArrays'
  { instanceId :: Core.Maybe Core.Text
    -- ^ The instance ID. If you use this parameter, @DescribeRaidArrays@ returns descriptions of the RAID arrays associated with the specified instance. 
  , raidArrayIds :: Core.Maybe [Core.Text]
    -- ^ An array of RAID array IDs. If you use this parameter, @DescribeRaidArrays@ returns descriptions of the specified arrays. Otherwise, it returns a description of every array.
  , stackId :: Core.Maybe Core.Text
    -- ^ The stack ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRaidArrays' value with any optional fields omitted.
mkDescribeRaidArrays
    :: DescribeRaidArrays
mkDescribeRaidArrays
  = DescribeRaidArrays'{instanceId = Core.Nothing,
                        raidArrayIds = Core.Nothing, stackId = Core.Nothing}

-- | The instance ID. If you use this parameter, @DescribeRaidArrays@ returns descriptions of the RAID arrays associated with the specified instance. 
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draInstanceId :: Lens.Lens' DescribeRaidArrays (Core.Maybe Core.Text)
draInstanceId = Lens.field @"instanceId"
{-# INLINEABLE draInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | An array of RAID array IDs. If you use this parameter, @DescribeRaidArrays@ returns descriptions of the specified arrays. Otherwise, it returns a description of every array.
--
-- /Note:/ Consider using 'raidArrayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draRaidArrayIds :: Lens.Lens' DescribeRaidArrays (Core.Maybe [Core.Text])
draRaidArrayIds = Lens.field @"raidArrayIds"
{-# INLINEABLE draRaidArrayIds #-}
{-# DEPRECATED raidArrayIds "Use generic-lens or generic-optics with 'raidArrayIds' instead"  #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draStackId :: Lens.Lens' DescribeRaidArrays (Core.Maybe Core.Text)
draStackId = Lens.field @"stackId"
{-# INLINEABLE draStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

instance Core.ToQuery DescribeRaidArrays where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeRaidArrays where
        toHeaders DescribeRaidArrays{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.DescribeRaidArrays")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeRaidArrays where
        toJSON DescribeRaidArrays{..}
          = Core.object
              (Core.catMaybes
                 [("InstanceId" Core..=) Core.<$> instanceId,
                  ("RaidArrayIds" Core..=) Core.<$> raidArrayIds,
                  ("StackId" Core..=) Core.<$> stackId])

instance Core.AWSRequest DescribeRaidArrays where
        type Rs DescribeRaidArrays = DescribeRaidArraysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeRaidArraysResponse' Core.<$>
                   (x Core..:? "RaidArrays") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @DescribeRaidArrays@ request.
--
-- /See:/ 'mkDescribeRaidArraysResponse' smart constructor.
data DescribeRaidArraysResponse = DescribeRaidArraysResponse'
  { raidArrays :: Core.Maybe [Types.RaidArray]
    -- ^ A @RaidArrays@ object that describes the specified RAID arrays.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRaidArraysResponse' value with any optional fields omitted.
mkDescribeRaidArraysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRaidArraysResponse
mkDescribeRaidArraysResponse responseStatus
  = DescribeRaidArraysResponse'{raidArrays = Core.Nothing,
                                responseStatus}

-- | A @RaidArrays@ object that describes the specified RAID arrays.
--
-- /Note:/ Consider using 'raidArrays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drarrsRaidArrays :: Lens.Lens' DescribeRaidArraysResponse (Core.Maybe [Types.RaidArray])
drarrsRaidArrays = Lens.field @"raidArrays"
{-# INLINEABLE drarrsRaidArrays #-}
{-# DEPRECATED raidArrays "Use generic-lens or generic-optics with 'raidArrays' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drarrsResponseStatus :: Lens.Lens' DescribeRaidArraysResponse Core.Int
drarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
