{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListVPCEConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all Amazon Virtual Private Cloud (VPC) endpoint configurations in the AWS account.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListVPCEConfigurations
    (
    -- * Creating a request
      ListVPCEConfigurations (..)
    , mkListVPCEConfigurations
    -- ** Request lenses
    , lvpcecMaxResults
    , lvpcecNextToken

    -- * Destructuring the response
    , ListVPCEConfigurationsResponse (..)
    , mkListVPCEConfigurationsResponse
    -- ** Response lenses
    , lvpcecrrsNextToken
    , lvpcecrrsVpceConfigurations
    , lvpcecrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListVPCEConfigurations' smart constructor.
data ListVPCEConfigurations = ListVPCEConfigurations'
  { maxResults :: Core.Maybe Core.Int
    -- ^ An integer that specifies the maximum number of items you want to return in the API response.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVPCEConfigurations' value with any optional fields omitted.
mkListVPCEConfigurations
    :: ListVPCEConfigurations
mkListVPCEConfigurations
  = ListVPCEConfigurations'{maxResults = Core.Nothing,
                            nextToken = Core.Nothing}

-- | An integer that specifies the maximum number of items you want to return in the API response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvpcecMaxResults :: Lens.Lens' ListVPCEConfigurations (Core.Maybe Core.Int)
lvpcecMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lvpcecMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvpcecNextToken :: Lens.Lens' ListVPCEConfigurations (Core.Maybe Types.PaginationToken)
lvpcecNextToken = Lens.field @"nextToken"
{-# INLINEABLE lvpcecNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListVPCEConfigurations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListVPCEConfigurations where
        toHeaders ListVPCEConfigurations{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.ListVPCEConfigurations")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListVPCEConfigurations where
        toJSON ListVPCEConfigurations{..}
          = Core.object
              (Core.catMaybes
                 [("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListVPCEConfigurations where
        type Rs ListVPCEConfigurations = ListVPCEConfigurationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListVPCEConfigurationsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "vpceConfigurations"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListVPCEConfigurations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"vpceConfigurations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListVPCEConfigurationsResponse' smart constructor.
data ListVPCEConfigurationsResponse = ListVPCEConfigurationsResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  , vpceConfigurations :: Core.Maybe [Types.VPCEConfiguration]
    -- ^ An array of @VPCEConfiguration@ objects that contain information about your VPC endpoint configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVPCEConfigurationsResponse' value with any optional fields omitted.
mkListVPCEConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListVPCEConfigurationsResponse
mkListVPCEConfigurationsResponse responseStatus
  = ListVPCEConfigurationsResponse'{nextToken = Core.Nothing,
                                    vpceConfigurations = Core.Nothing, responseStatus}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvpcecrrsNextToken :: Lens.Lens' ListVPCEConfigurationsResponse (Core.Maybe Types.PaginationToken)
lvpcecrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lvpcecrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of @VPCEConfiguration@ objects that contain information about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvpcecrrsVpceConfigurations :: Lens.Lens' ListVPCEConfigurationsResponse (Core.Maybe [Types.VPCEConfiguration])
lvpcecrrsVpceConfigurations = Lens.field @"vpceConfigurations"
{-# INLINEABLE lvpcecrrsVpceConfigurations #-}
{-# DEPRECATED vpceConfigurations "Use generic-lens or generic-optics with 'vpceConfigurations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvpcecrrsResponseStatus :: Lens.Lens' ListVPCEConfigurationsResponse Core.Int
lvpcecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lvpcecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
