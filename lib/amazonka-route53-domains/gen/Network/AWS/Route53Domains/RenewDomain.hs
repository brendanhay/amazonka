{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.RenewDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation renews a domain for the specified number of years. The cost of renewing your domain is billed to your AWS account.
--
-- We recommend that you renew your domain several weeks before the expiration date. Some TLD registries delete domains before the expiration date if you haven't renewed far enough in advance. For more information about renewing domain registration, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-renew.html Renewing Registration for a Domain> in the /Amazon Route 53 Developer Guide/ .
module Network.AWS.Route53Domains.RenewDomain
  ( -- * Creating a request
    RenewDomain (..),
    mkRenewDomain,

    -- ** Request lenses
    rdDomainName,
    rdCurrentExpiryYear,
    rdDurationInYears,

    -- * Destructuring the response
    RenewDomainResponse (..),
    mkRenewDomainResponse,

    -- ** Response lenses
    rrsOperationId,
    rrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | A @RenewDomain@ request includes the number of years that you want to renew for and the current expiration year.
--
-- /See:/ 'mkRenewDomain' smart constructor.
data RenewDomain = RenewDomain'
  { -- | The name of the domain that you want to renew.
    domainName :: Types.DomainName,
    -- | The year when the registration for the domain is set to expire. This value must match the current expiration date for the domain.
    currentExpiryYear :: Core.Int,
    -- | The number of years that you want to renew the domain for. The maximum number of years depends on the top-level domain. For the range of valid values for your domain, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
    --
    -- Default: 1
    durationInYears :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RenewDomain' value with any optional fields omitted.
mkRenewDomain ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'currentExpiryYear'
  Core.Int ->
  RenewDomain
mkRenewDomain domainName currentExpiryYear =
  RenewDomain'
    { domainName,
      currentExpiryYear,
      durationInYears = Core.Nothing
    }

-- | The name of the domain that you want to renew.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDomainName :: Lens.Lens' RenewDomain Types.DomainName
rdDomainName = Lens.field @"domainName"
{-# DEPRECATED rdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The year when the registration for the domain is set to expire. This value must match the current expiration date for the domain.
--
-- /Note:/ Consider using 'currentExpiryYear' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCurrentExpiryYear :: Lens.Lens' RenewDomain Core.Int
rdCurrentExpiryYear = Lens.field @"currentExpiryYear"
{-# DEPRECATED rdCurrentExpiryYear "Use generic-lens or generic-optics with 'currentExpiryYear' instead." #-}

-- | The number of years that you want to renew the domain for. The maximum number of years depends on the top-level domain. For the range of valid values for your domain, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- Default: 1
--
-- /Note:/ Consider using 'durationInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDurationInYears :: Lens.Lens' RenewDomain (Core.Maybe Core.Natural)
rdDurationInYears = Lens.field @"durationInYears"
{-# DEPRECATED rdDurationInYears "Use generic-lens or generic-optics with 'durationInYears' instead." #-}

instance Core.FromJSON RenewDomain where
  toJSON RenewDomain {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainName" Core..= domainName),
            Core.Just ("CurrentExpiryYear" Core..= currentExpiryYear),
            ("DurationInYears" Core..=) Core.<$> durationInYears
          ]
      )

instance Core.AWSRequest RenewDomain where
  type Rs RenewDomain = RenewDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Route53Domains_v20140515.RenewDomain")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RenewDomainResponse'
            Core.<$> (x Core..: "OperationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRenewDomainResponse' smart constructor.
data RenewDomainResponse = RenewDomainResponse'
  { -- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
    operationId :: Types.OperationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RenewDomainResponse' value with any optional fields omitted.
mkRenewDomainResponse ::
  -- | 'operationId'
  Types.OperationId ->
  -- | 'responseStatus'
  Core.Int ->
  RenewDomainResponse
mkRenewDomainResponse operationId responseStatus =
  RenewDomainResponse' {operationId, responseStatus}

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsOperationId :: Lens.Lens' RenewDomainResponse Types.OperationId
rrsOperationId = Lens.field @"operationId"
{-# DEPRECATED rrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RenewDomainResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
