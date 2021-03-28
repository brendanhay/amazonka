{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified services running in your cluster.
module Network.AWS.ECS.DescribeServices
    (
    -- * Creating a request
      DescribeServices (..)
    , mkDescribeServices
    -- ** Request lenses
    , dServices
    , dCluster
    , dInclude

    -- * Destructuring the response
    , DescribeServicesResponse (..)
    , mkDescribeServicesResponse
    -- ** Response lenses
    , dsrfrsFailures
    , dsrfrsServices
    , dsrfrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { services :: [Core.Text]
    -- ^ A list of services to describe. You may specify up to 10 services to describe in a single operation.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN)the cluster that hosts the service to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the service or services you are describing were launched in any cluster other than the default cluster.
  , include :: Core.Maybe [Types.ServiceField]
    -- ^ Specifies whether you want to see the resource tags for the service. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServices' value with any optional fields omitted.
mkDescribeServices
    :: DescribeServices
mkDescribeServices
  = DescribeServices'{services = Core.mempty, cluster = Core.Nothing,
                      include = Core.Nothing}

-- | A list of services to describe. You may specify up to 10 services to describe in a single operation.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dServices :: Lens.Lens' DescribeServices [Core.Text]
dServices = Lens.field @"services"
{-# INLINEABLE dServices #-}
{-# DEPRECATED services "Use generic-lens or generic-optics with 'services' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN)the cluster that hosts the service to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the service or services you are describing were launched in any cluster other than the default cluster.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCluster :: Lens.Lens' DescribeServices (Core.Maybe Core.Text)
dCluster = Lens.field @"cluster"
{-# INLINEABLE dCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | Specifies whether you want to see the resource tags for the service. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInclude :: Lens.Lens' DescribeServices (Core.Maybe [Types.ServiceField])
dInclude = Lens.field @"include"
{-# INLINEABLE dInclude #-}
{-# DEPRECATED include "Use generic-lens or generic-optics with 'include' instead"  #-}

instance Core.ToQuery DescribeServices where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeServices where
        toHeaders DescribeServices{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.DescribeServices")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeServices where
        toJSON DescribeServices{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("services" Core..= services),
                  ("cluster" Core..=) Core.<$> cluster,
                  ("include" Core..=) Core.<$> include])

instance Core.AWSRequest DescribeServices where
        type Rs DescribeServices = DescribeServicesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeServicesResponse' Core.<$>
                   (x Core..:? "failures") Core.<*> x Core..:? "services" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { failures :: Core.Maybe [Types.Failure]
    -- ^ Any failures associated with the call.
  , services :: Core.Maybe [Types.ContainerService]
    -- ^ The list of services described.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeServicesResponse' value with any optional fields omitted.
mkDescribeServicesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeServicesResponse
mkDescribeServicesResponse responseStatus
  = DescribeServicesResponse'{failures = Core.Nothing,
                              services = Core.Nothing, responseStatus}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsFailures :: Lens.Lens' DescribeServicesResponse (Core.Maybe [Types.Failure])
dsrfrsFailures = Lens.field @"failures"
{-# INLINEABLE dsrfrsFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | The list of services described.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsServices :: Lens.Lens' DescribeServicesResponse (Core.Maybe [Types.ContainerService])
dsrfrsServices = Lens.field @"services"
{-# INLINEABLE dsrfrsServices #-}
{-# DEPRECATED services "Use generic-lens or generic-optics with 'services' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DescribeServicesResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
