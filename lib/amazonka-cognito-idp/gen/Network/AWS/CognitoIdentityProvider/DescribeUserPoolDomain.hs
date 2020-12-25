{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPoolDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a domain.
module Network.AWS.CognitoIdentityProvider.DescribeUserPoolDomain
  ( -- * Creating a request
    DescribeUserPoolDomain (..),
    mkDescribeUserPoolDomain,

    -- ** Request lenses
    dDomain,

    -- * Destructuring the response
    DescribeUserPoolDomainResponse (..),
    mkDescribeUserPoolDomainResponse,

    -- ** Response lenses
    drsDomainDescription,
    drsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUserPoolDomain' smart constructor.
newtype DescribeUserPoolDomain = DescribeUserPoolDomain'
  { -- | The domain string.
    domain :: Types.DomainType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserPoolDomain' value with any optional fields omitted.
mkDescribeUserPoolDomain ::
  -- | 'domain'
  Types.DomainType ->
  DescribeUserPoolDomain
mkDescribeUserPoolDomain domain = DescribeUserPoolDomain' {domain}

-- | The domain string.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomain :: Lens.Lens' DescribeUserPoolDomain Types.DomainType
dDomain = Lens.field @"domain"
{-# DEPRECATED dDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

instance Core.FromJSON DescribeUserPoolDomain where
  toJSON DescribeUserPoolDomain {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Domain" Core..= domain)])

instance Core.AWSRequest DescribeUserPoolDomain where
  type Rs DescribeUserPoolDomain = DescribeUserPoolDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.DescribeUserPoolDomain"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserPoolDomainResponse'
            Core.<$> (x Core..:? "DomainDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeUserPoolDomainResponse' smart constructor.
data DescribeUserPoolDomainResponse = DescribeUserPoolDomainResponse'
  { -- | A domain description object containing information about the domain.
    domainDescription :: Core.Maybe Types.DomainDescriptionType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserPoolDomainResponse' value with any optional fields omitted.
mkDescribeUserPoolDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUserPoolDomainResponse
mkDescribeUserPoolDomainResponse responseStatus =
  DescribeUserPoolDomainResponse'
    { domainDescription = Core.Nothing,
      responseStatus
    }

-- | A domain description object containing information about the domain.
--
-- /Note:/ Consider using 'domainDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDomainDescription :: Lens.Lens' DescribeUserPoolDomainResponse (Core.Maybe Types.DomainDescriptionType)
drsDomainDescription = Lens.field @"domainDescription"
{-# DEPRECATED drsDomainDescription "Use generic-lens or generic-optics with 'domainDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeUserPoolDomainResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
