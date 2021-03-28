{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.ListCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of certificate ARNs and domain names. You can request that only certificates that match a specific status be listed. You can also filter by specific attributes of the certificate. Default filtering returns only @RSA_2048@ certificates. For more information, see 'Filters' .
--
-- This operation returns paginated results.
module Network.AWS.CertificateManager.ListCertificates
    (
    -- * Creating a request
      ListCertificates (..)
    , mkListCertificates
    -- ** Request lenses
    , lcCertificateStatuses
    , lcIncludes
    , lcMaxItems
    , lcNextToken

    -- * Destructuring the response
    , ListCertificatesResponse (..)
    , mkListCertificatesResponse
    -- ** Response lenses
    , lcrrsCertificateSummaryList
    , lcrrsNextToken
    , lcrrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { certificateStatuses :: Core.Maybe [Types.CertificateStatus]
    -- ^ Filter the certificate list by status value.
  , includes :: Core.Maybe Types.Filters
    -- ^ Filter the certificate list. For more information, see the 'Filters' structure.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ Use this parameter when paginating results to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Use this parameter only when paginating results and only in a subsequent request after you receive a response with truncated results. Set it to the value of @NextToken@ from the response you just received.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCertificates' value with any optional fields omitted.
mkListCertificates
    :: ListCertificates
mkListCertificates
  = ListCertificates'{certificateStatuses = Core.Nothing,
                      includes = Core.Nothing, maxItems = Core.Nothing,
                      nextToken = Core.Nothing}

-- | Filter the certificate list by status value.
--
-- /Note:/ Consider using 'certificateStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCertificateStatuses :: Lens.Lens' ListCertificates (Core.Maybe [Types.CertificateStatus])
lcCertificateStatuses = Lens.field @"certificateStatuses"
{-# INLINEABLE lcCertificateStatuses #-}
{-# DEPRECATED certificateStatuses "Use generic-lens or generic-optics with 'certificateStatuses' instead"  #-}

-- | Filter the certificate list. For more information, see the 'Filters' structure.
--
-- /Note:/ Consider using 'includes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcIncludes :: Lens.Lens' ListCertificates (Core.Maybe Types.Filters)
lcIncludes = Lens.field @"includes"
{-# INLINEABLE lcIncludes #-}
{-# DEPRECATED includes "Use generic-lens or generic-optics with 'includes' instead"  #-}

-- | Use this parameter when paginating results to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxItems :: Lens.Lens' ListCertificates (Core.Maybe Core.Natural)
lcMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lcMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | Use this parameter only when paginating results and only in a subsequent request after you receive a response with truncated results. Set it to the value of @NextToken@ from the response you just received.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListCertificates (Core.Maybe Types.NextToken)
lcNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListCertificates where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListCertificates where
        toHeaders ListCertificates{..}
          = Core.pure ("X-Amz-Target", "CertificateManager.ListCertificates")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListCertificates where
        toJSON ListCertificates{..}
          = Core.object
              (Core.catMaybes
                 [("CertificateStatuses" Core..=) Core.<$> certificateStatuses,
                  ("Includes" Core..=) Core.<$> includes,
                  ("MaxItems" Core..=) Core.<$> maxItems,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListCertificates where
        type Rs ListCertificates = ListCertificatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListCertificatesResponse' Core.<$>
                   (x Core..:? "CertificateSummaryList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListCertificates where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"certificateSummaryList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { certificateSummaryList :: Core.Maybe [Types.CertificateSummary]
    -- ^ A list of ACM certificates.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ When the list is truncated, this value is present and contains the value to use for the @NextToken@ parameter in a subsequent pagination request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCertificatesResponse' value with any optional fields omitted.
mkListCertificatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListCertificatesResponse
mkListCertificatesResponse responseStatus
  = ListCertificatesResponse'{certificateSummaryList = Core.Nothing,
                              nextToken = Core.Nothing, responseStatus}

-- | A list of ACM certificates.
--
-- /Note:/ Consider using 'certificateSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsCertificateSummaryList :: Lens.Lens' ListCertificatesResponse (Core.Maybe [Types.CertificateSummary])
lcrrsCertificateSummaryList = Lens.field @"certificateSummaryList"
{-# INLINEABLE lcrrsCertificateSummaryList #-}
{-# DEPRECATED certificateSummaryList "Use generic-lens or generic-optics with 'certificateSummaryList' instead"  #-}

-- | When the list is truncated, this value is present and contains the value to use for the @NextToken@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListCertificatesResponse (Core.Maybe Types.NextToken)
lcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListCertificatesResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
