{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more on-premises instances. The maximum number of on-premises instances that can be returned is 25.
module Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
    (
    -- * Creating a request
      BatchGetOnPremisesInstances (..)
    , mkBatchGetOnPremisesInstances
    -- ** Request lenses
    , bgopiInstanceNames

    -- * Destructuring the response
    , BatchGetOnPremisesInstancesResponse (..)
    , mkBatchGetOnPremisesInstancesResponse
    -- ** Response lenses
    , bgopirrsInstanceInfos
    , bgopirrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @BatchGetOnPremisesInstances@ operation.
--
-- /See:/ 'mkBatchGetOnPremisesInstances' smart constructor.
newtype BatchGetOnPremisesInstances = BatchGetOnPremisesInstances'
  { instanceNames :: [Types.InstanceName]
    -- ^ The names of the on-premises instances about which to get information. The maximum number of instance names you can specify is 25.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetOnPremisesInstances' value with any optional fields omitted.
mkBatchGetOnPremisesInstances
    :: BatchGetOnPremisesInstances
mkBatchGetOnPremisesInstances
  = BatchGetOnPremisesInstances'{instanceNames = Core.mempty}

-- | The names of the on-premises instances about which to get information. The maximum number of instance names you can specify is 25.
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgopiInstanceNames :: Lens.Lens' BatchGetOnPremisesInstances [Types.InstanceName]
bgopiInstanceNames = Lens.field @"instanceNames"
{-# INLINEABLE bgopiInstanceNames #-}
{-# DEPRECATED instanceNames "Use generic-lens or generic-optics with 'instanceNames' instead"  #-}

instance Core.ToQuery BatchGetOnPremisesInstances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetOnPremisesInstances where
        toHeaders BatchGetOnPremisesInstances{..}
          = Core.pure
              ("X-Amz-Target", "CodeDeploy_20141006.BatchGetOnPremisesInstances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetOnPremisesInstances where
        toJSON BatchGetOnPremisesInstances{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("instanceNames" Core..= instanceNames)])

instance Core.AWSRequest BatchGetOnPremisesInstances where
        type Rs BatchGetOnPremisesInstances =
             BatchGetOnPremisesInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetOnPremisesInstancesResponse' Core.<$>
                   (x Core..:? "instanceInfos") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @BatchGetOnPremisesInstances@ operation.
--
-- /See:/ 'mkBatchGetOnPremisesInstancesResponse' smart constructor.
data BatchGetOnPremisesInstancesResponse = BatchGetOnPremisesInstancesResponse'
  { instanceInfos :: Core.Maybe [Types.InstanceInfo]
    -- ^ Information about the on-premises instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetOnPremisesInstancesResponse' value with any optional fields omitted.
mkBatchGetOnPremisesInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetOnPremisesInstancesResponse
mkBatchGetOnPremisesInstancesResponse responseStatus
  = BatchGetOnPremisesInstancesResponse'{instanceInfos =
                                           Core.Nothing,
                                         responseStatus}

-- | Information about the on-premises instances.
--
-- /Note:/ Consider using 'instanceInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgopirrsInstanceInfos :: Lens.Lens' BatchGetOnPremisesInstancesResponse (Core.Maybe [Types.InstanceInfo])
bgopirrsInstanceInfos = Lens.field @"instanceInfos"
{-# INLINEABLE bgopirrsInstanceInfos #-}
{-# DEPRECATED instanceInfos "Use generic-lens or generic-optics with 'instanceInfos' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgopirrsResponseStatus :: Lens.Lens' BatchGetOnPremisesInstancesResponse Core.Int
bgopirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgopirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
