{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListDomainConfigurations (..),
    mkListDomainConfigurations,

    -- ** Request lenses
    ldcMarker,
    ldcPageSize,
    ldcServiceType,

    -- * Destructuring the response
    ListDomainConfigurationsResponse (..),
    mkListDomainConfigurationsResponse,

    -- ** Response lenses
    ldcrrsDomainConfigurations,
    ldcrrsNextMarker,
    ldcrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDomainConfigurations' smart constructor.
data ListDomainConfigurations = ListDomainConfigurations'
  { -- | The marker for the next set of results.
    marker :: Core.Maybe Types.Marker,
    -- | The result page size.
    pageSize :: Core.Maybe Core.Natural,
    -- | The type of service delivered by the endpoint.
    serviceType :: Core.Maybe Types.ServiceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomainConfigurations' value with any optional fields omitted.
mkListDomainConfigurations ::
  ListDomainConfigurations
mkListDomainConfigurations =
  ListDomainConfigurations'
    { marker = Core.Nothing,
      pageSize = Core.Nothing,
      serviceType = Core.Nothing
    }

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcMarker :: Lens.Lens' ListDomainConfigurations (Core.Maybe Types.Marker)
ldcMarker = Lens.field @"marker"
{-# DEPRECATED ldcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcPageSize :: Lens.Lens' ListDomainConfigurations (Core.Maybe Core.Natural)
ldcPageSize = Lens.field @"pageSize"
{-# DEPRECATED ldcPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The type of service delivered by the endpoint.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcServiceType :: Lens.Lens' ListDomainConfigurations (Core.Maybe Types.ServiceType)
ldcServiceType = Lens.field @"serviceType"
{-# DEPRECATED ldcServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

instance Core.AWSRequest ListDomainConfigurations where
  type Rs ListDomainConfigurations = ListDomainConfigurationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/domainConfigurations",
        Core._rqQuery =
          Core.toQueryValue "marker" Core.<$> marker
            Core.<> (Core.toQueryValue "pageSize" Core.<$> pageSize)
            Core.<> (Core.toQueryValue "serviceType" Core.<$> serviceType),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainConfigurationsResponse'
            Core.<$> (x Core..:? "domainConfigurations")
            Core.<*> (x Core..:? "nextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDomainConfigurations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"domainConfigurations" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListDomainConfigurationsResponse' smart constructor.
data ListDomainConfigurationsResponse = ListDomainConfigurationsResponse'
  { -- | A list of objects that contain summary information about the user's domain configurations.
    domainConfigurations :: Core.Maybe [Types.DomainConfigurationSummary],
    -- | The marker for the next set of results.
    nextMarker :: Core.Maybe Types.NextMarker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomainConfigurationsResponse' value with any optional fields omitted.
mkListDomainConfigurationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDomainConfigurationsResponse
mkListDomainConfigurationsResponse responseStatus =
  ListDomainConfigurationsResponse'
    { domainConfigurations =
        Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | A list of objects that contain summary information about the user's domain configurations.
--
-- /Note:/ Consider using 'domainConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsDomainConfigurations :: Lens.Lens' ListDomainConfigurationsResponse (Core.Maybe [Types.DomainConfigurationSummary])
ldcrrsDomainConfigurations = Lens.field @"domainConfigurations"
{-# DEPRECATED ldcrrsDomainConfigurations "Use generic-lens or generic-optics with 'domainConfigurations' instead." #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsNextMarker :: Lens.Lens' ListDomainConfigurationsResponse (Core.Maybe Types.NextMarker)
ldcrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED ldcrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsResponseStatus :: Lens.Lens' ListDomainConfigurationsResponse Core.Int
ldcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
