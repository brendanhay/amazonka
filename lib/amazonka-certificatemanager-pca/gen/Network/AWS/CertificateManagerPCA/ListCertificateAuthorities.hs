{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListCertificateAuthorities (..),
    mkListCertificateAuthorities,

    -- ** Request lenses
    lcaMaxResults,
    lcaNextToken,
    lcaResourceOwner,

    -- * Destructuring the response
    ListCertificateAuthoritiesResponse (..),
    mkListCertificateAuthoritiesResponse,

    -- ** Response lenses
    lcarrsCertificateAuthorities,
    lcarrsNextToken,
    lcarrsResponseStatus,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCertificateAuthorities' smart constructor.
data ListCertificateAuthorities = ListCertificateAuthorities'
  { -- | Use this parameter when paginating results to specify the maximum number of items to return in the response on each page. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
    maxResults :: Core.Maybe Core.Natural,
    -- | Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of the @NextToken@ parameter from the response you just received.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Use this parameter to filter the returned set of certificate authorities based on their owner. The default is SELF.
    resourceOwner :: Core.Maybe Types.ResourceOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCertificateAuthorities' value with any optional fields omitted.
mkListCertificateAuthorities ::
  ListCertificateAuthorities
mkListCertificateAuthorities =
  ListCertificateAuthorities'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      resourceOwner = Core.Nothing
    }

-- | Use this parameter when paginating results to specify the maximum number of items to return in the response on each page. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaMaxResults :: Lens.Lens' ListCertificateAuthorities (Core.Maybe Core.Natural)
lcaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of the @NextToken@ parameter from the response you just received.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaNextToken :: Lens.Lens' ListCertificateAuthorities (Core.Maybe Types.NextToken)
lcaNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Use this parameter to filter the returned set of certificate authorities based on their owner. The default is SELF.
--
-- /Note:/ Consider using 'resourceOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaResourceOwner :: Lens.Lens' ListCertificateAuthorities (Core.Maybe Types.ResourceOwner)
lcaResourceOwner = Lens.field @"resourceOwner"
{-# DEPRECATED lcaResourceOwner "Use generic-lens or generic-optics with 'resourceOwner' instead." #-}

instance Core.FromJSON ListCertificateAuthorities where
  toJSON ListCertificateAuthorities {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ResourceOwner" Core..=) Core.<$> resourceOwner
          ]
      )

instance Core.AWSRequest ListCertificateAuthorities where
  type
    Rs ListCertificateAuthorities =
      ListCertificateAuthoritiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ACMPrivateCA.ListCertificateAuthorities")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCertificateAuthoritiesResponse'
            Core.<$> (x Core..:? "CertificateAuthorities")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCertificateAuthorities where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"certificateAuthorities" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListCertificateAuthoritiesResponse' smart constructor.
data ListCertificateAuthoritiesResponse = ListCertificateAuthoritiesResponse'
  { -- | Summary information about each certificate authority you have created.
    certificateAuthorities :: Core.Maybe [Types.CertificateAuthority],
    -- | When the list is truncated, this value is present and should be used for the @NextToken@ parameter in a subsequent pagination request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCertificateAuthoritiesResponse' value with any optional fields omitted.
mkListCertificateAuthoritiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCertificateAuthoritiesResponse
mkListCertificateAuthoritiesResponse responseStatus =
  ListCertificateAuthoritiesResponse'
    { certificateAuthorities =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Summary information about each certificate authority you have created.
--
-- /Note:/ Consider using 'certificateAuthorities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarrsCertificateAuthorities :: Lens.Lens' ListCertificateAuthoritiesResponse (Core.Maybe [Types.CertificateAuthority])
lcarrsCertificateAuthorities = Lens.field @"certificateAuthorities"
{-# DEPRECATED lcarrsCertificateAuthorities "Use generic-lens or generic-optics with 'certificateAuthorities' instead." #-}

-- | When the list is truncated, this value is present and should be used for the @NextToken@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarrsNextToken :: Lens.Lens' ListCertificateAuthoritiesResponse (Core.Maybe Types.NextToken)
lcarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarrsResponseStatus :: Lens.Lens' ListCertificateAuthoritiesResponse Core.Int
lcarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
