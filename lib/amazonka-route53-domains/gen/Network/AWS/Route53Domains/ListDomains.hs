{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.ListDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns all the domain names registered with Amazon Route 53 for the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.Route53Domains.ListDomains
  ( -- * Creating a request
    ListDomains (..),
    mkListDomains,

    -- ** Request lenses
    ldMarker,
    ldMaxItems,

    -- * Destructuring the response
    ListDomainsResponse (..),
    mkListDomainsResponse,

    -- ** Response lenses
    ldrrsDomains,
    ldrrsNextPageMarker,
    ldrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The ListDomains request includes the following elements.
--
-- /See:/ 'mkListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | For an initial request for a list of domains, omit this element. If the number of domains that are associated with the current AWS account is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional domains. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
    --
    -- Constraints: The marker must match the value specified in the previous request.
    marker :: Core.Maybe Types.Marker,
    -- | Number of domains to be returned.
    --
    -- Default: 20
    maxItems :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomains' value with any optional fields omitted.
mkListDomains ::
  ListDomains
mkListDomains =
  ListDomains' {marker = Core.Nothing, maxItems = Core.Nothing}

-- | For an initial request for a list of domains, omit this element. If the number of domains that are associated with the current AWS account is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional domains. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
--
-- Constraints: The marker must match the value specified in the previous request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMarker :: Lens.Lens' ListDomains (Core.Maybe Types.Marker)
ldMarker = Lens.field @"marker"
{-# DEPRECATED ldMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Number of domains to be returned.
--
-- Default: 20
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxItems :: Lens.Lens' ListDomains (Core.Maybe Core.Int)
ldMaxItems = Lens.field @"maxItems"
{-# DEPRECATED ldMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.FromJSON ListDomains where
  toJSON ListDomains {..} =
    Core.object
      ( Core.catMaybes
          [ ("Marker" Core..=) Core.<$> marker,
            ("MaxItems" Core..=) Core.<$> maxItems
          ]
      )

instance Core.AWSRequest ListDomains where
  type Rs ListDomains = ListDomainsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Route53Domains_v20140515.ListDomains")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainsResponse'
            Core.<$> (x Core..:? "Domains" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextPageMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDomains where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageMarker") =
      Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"domains") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker"
            Lens..~ rs Lens.^. Lens.field @"nextPageMarker"
        )

-- | The ListDomains response includes the following elements.
--
-- /See:/ 'mkListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { -- | A summary of domains.
    domains :: [Types.DomainSummary],
    -- | If there are more domains than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
    nextPageMarker :: Core.Maybe Types.NextPageMarker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDomainsResponse' value with any optional fields omitted.
mkListDomainsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDomainsResponse
mkListDomainsResponse responseStatus =
  ListDomainsResponse'
    { domains = Core.mempty,
      nextPageMarker = Core.Nothing,
      responseStatus
    }

-- | A summary of domains.
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDomains :: Lens.Lens' ListDomainsResponse [Types.DomainSummary]
ldrrsDomains = Lens.field @"domains"
{-# DEPRECATED ldrrsDomains "Use generic-lens or generic-optics with 'domains' instead." #-}

-- | If there are more domains than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
--
-- /Note:/ Consider using 'nextPageMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextPageMarker :: Lens.Lens' ListDomainsResponse (Core.Maybe Types.NextPageMarker)
ldrrsNextPageMarker = Lens.field @"nextPageMarker"
{-# DEPRECATED ldrrsNextPageMarker "Use generic-lens or generic-optics with 'nextPageMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDomainsResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
