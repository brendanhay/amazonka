{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.ListCertificateAuthorities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the private certificate authorities that you created by using the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action.
--
-- This operation returns paginated results.
module Network.AWS.CertificateManagerPCA.ListCertificateAuthorities
    (
    -- * Creating a request
      ListCertificateAuthorities (..)
    , mkListCertificateAuthorities
    -- ** Request lenses
    , lcaMaxResults
    , lcaNextToken
    , lcaResourceOwner

    -- * Destructuring the response
    , ListCertificateAuthoritiesResponse (..)
    , mkListCertificateAuthoritiesResponse
    -- ** Response lenses
    , lcarrsCertificateAuthorities
    , lcarrsNextToken
    , lcarrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCertificateAuthorities' smart constructor.
data ListCertificateAuthorities = ListCertificateAuthorities'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ Use this parameter when paginating results to specify the maximum number of items to return in the response on each page. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of the @NextToken@ parameter from the response you just received.
  , resourceOwner :: Core.Maybe Types.ResourceOwner
    -- ^ Use this parameter to filter the returned set of certificate authorities based on their owner. The default is SELF.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCertificateAuthorities' value with any optional fields omitted.
mkListCertificateAuthorities
    :: ListCertificateAuthorities
mkListCertificateAuthorities
  = ListCertificateAuthorities'{maxResults = Core.Nothing,
                                nextToken = Core.Nothing, resourceOwner = Core.Nothing}

-- | Use this parameter when paginating results to specify the maximum number of items to return in the response on each page. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaMaxResults :: Lens.Lens' ListCertificateAuthorities (Core.Maybe Core.Natural)
lcaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of the @NextToken@ parameter from the response you just received.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaNextToken :: Lens.Lens' ListCertificateAuthorities (Core.Maybe Types.NextToken)
lcaNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Use this parameter to filter the returned set of certificate authorities based on their owner. The default is SELF.
--
-- /Note:/ Consider using 'resourceOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaResourceOwner :: Lens.Lens' ListCertificateAuthorities (Core.Maybe Types.ResourceOwner)
lcaResourceOwner = Lens.field @"resourceOwner"
{-# INLINEABLE lcaResourceOwner #-}
{-# DEPRECATED resourceOwner "Use generic-lens or generic-optics with 'resourceOwner' instead"  #-}

instance Core.ToQuery ListCertificateAuthorities where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListCertificateAuthorities where
        toHeaders ListCertificateAuthorities{..}
          = Core.pure
              ("X-Amz-Target", "ACMPrivateCA.ListCertificateAuthorities")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListCertificateAuthorities where
        toJSON ListCertificateAuthorities{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ResourceOwner" Core..=) Core.<$> resourceOwner])

instance Core.AWSRequest ListCertificateAuthorities where
        type Rs ListCertificateAuthorities =
             ListCertificateAuthoritiesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListCertificateAuthoritiesResponse' Core.<$>
                   (x Core..:? "CertificateAuthorities") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListCertificateAuthorities where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"certificateAuthorities" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListCertificateAuthoritiesResponse' smart constructor.
data ListCertificateAuthoritiesResponse = ListCertificateAuthoritiesResponse'
  { certificateAuthorities :: Core.Maybe [Types.CertificateAuthority]
    -- ^ Summary information about each certificate authority you have created.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ When the list is truncated, this value is present and should be used for the @NextToken@ parameter in a subsequent pagination request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListCertificateAuthoritiesResponse' value with any optional fields omitted.
mkListCertificateAuthoritiesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListCertificateAuthoritiesResponse
mkListCertificateAuthoritiesResponse responseStatus
  = ListCertificateAuthoritiesResponse'{certificateAuthorities =
                                          Core.Nothing,
                                        nextToken = Core.Nothing, responseStatus}

-- | Summary information about each certificate authority you have created.
--
-- /Note:/ Consider using 'certificateAuthorities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarrsCertificateAuthorities :: Lens.Lens' ListCertificateAuthoritiesResponse (Core.Maybe [Types.CertificateAuthority])
lcarrsCertificateAuthorities = Lens.field @"certificateAuthorities"
{-# INLINEABLE lcarrsCertificateAuthorities #-}
{-# DEPRECATED certificateAuthorities "Use generic-lens or generic-optics with 'certificateAuthorities' instead"  #-}

-- | When the list is truncated, this value is present and should be used for the @NextToken@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarrsNextToken :: Lens.Lens' ListCertificateAuthoritiesResponse (Core.Maybe Types.NextToken)
lcarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarrsResponseStatus :: Lens.Lens' ListCertificateAuthoritiesResponse Core.Int
lcarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
