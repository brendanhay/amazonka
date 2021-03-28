{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListDomainConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of domain configurations for the user. This list is sorted alphabetically by domain configuration name.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListDomainConfigurations
    (
    -- * Creating a request
      ListDomainConfigurations (..)
    , mkListDomainConfigurations
    -- ** Request lenses
    , ldcMarker
    , ldcPageSize
    , ldcServiceType

    -- * Destructuring the response
    , ListDomainConfigurationsResponse (..)
    , mkListDomainConfigurationsResponse
    -- ** Response lenses
    , ldcrrsDomainConfigurations
    , ldcrrsNextMarker
    , ldcrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDomainConfigurations' smart constructor.
data ListDomainConfigurations = ListDomainConfigurations'
  { marker :: Core.Maybe Types.Marker
    -- ^ The marker for the next set of results.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The result page size.
  , serviceType :: Core.Maybe Types.ServiceType
    -- ^ The type of service delivered by the endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomainConfigurations' value with any optional fields omitted.
mkListDomainConfigurations
    :: ListDomainConfigurations
mkListDomainConfigurations
  = ListDomainConfigurations'{marker = Core.Nothing,
                              pageSize = Core.Nothing, serviceType = Core.Nothing}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcMarker :: Lens.Lens' ListDomainConfigurations (Core.Maybe Types.Marker)
ldcMarker = Lens.field @"marker"
{-# INLINEABLE ldcMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcPageSize :: Lens.Lens' ListDomainConfigurations (Core.Maybe Core.Natural)
ldcPageSize = Lens.field @"pageSize"
{-# INLINEABLE ldcPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The type of service delivered by the endpoint.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcServiceType :: Lens.Lens' ListDomainConfigurations (Core.Maybe Types.ServiceType)
ldcServiceType = Lens.field @"serviceType"
{-# INLINEABLE ldcServiceType #-}
{-# DEPRECATED serviceType "Use generic-lens or generic-optics with 'serviceType' instead"  #-}

instance Core.ToQuery ListDomainConfigurations where
        toQuery ListDomainConfigurations{..}
          = Core.maybe Core.mempty (Core.toQueryPair "marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "pageSize") pageSize
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "serviceType") serviceType

instance Core.ToHeaders ListDomainConfigurations where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListDomainConfigurations where
        type Rs ListDomainConfigurations = ListDomainConfigurationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/domainConfigurations",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDomainConfigurationsResponse' Core.<$>
                   (x Core..:? "domainConfigurations") Core.<*>
                     x Core..:? "nextMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDomainConfigurations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"domainConfigurations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListDomainConfigurationsResponse' smart constructor.
data ListDomainConfigurationsResponse = ListDomainConfigurationsResponse'
  { domainConfigurations :: Core.Maybe [Types.DomainConfigurationSummary]
    -- ^ A list of objects that contain summary information about the user's domain configurations.
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ The marker for the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomainConfigurationsResponse' value with any optional fields omitted.
mkListDomainConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDomainConfigurationsResponse
mkListDomainConfigurationsResponse responseStatus
  = ListDomainConfigurationsResponse'{domainConfigurations =
                                        Core.Nothing,
                                      nextMarker = Core.Nothing, responseStatus}

-- | A list of objects that contain summary information about the user's domain configurations.
--
-- /Note:/ Consider using 'domainConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsDomainConfigurations :: Lens.Lens' ListDomainConfigurationsResponse (Core.Maybe [Types.DomainConfigurationSummary])
ldcrrsDomainConfigurations = Lens.field @"domainConfigurations"
{-# INLINEABLE ldcrrsDomainConfigurations #-}
{-# DEPRECATED domainConfigurations "Use generic-lens or generic-optics with 'domainConfigurations' instead"  #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsNextMarker :: Lens.Lens' ListDomainConfigurationsResponse (Core.Maybe Types.NextMarker)
ldcrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE ldcrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsResponseStatus :: Lens.Lens' ListDomainConfigurationsResponse Core.Int
ldcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
