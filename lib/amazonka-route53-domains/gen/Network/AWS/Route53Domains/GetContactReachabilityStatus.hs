{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.GetContactReachabilityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For operations that require confirmation that the email address for the registrant contact is valid, such as registering a new domain, this operation returns information about whether the registrant contact has responded.
--
-- If you want us to resend the email, use the @ResendContactReachabilityEmail@ operation.
module Network.AWS.Route53Domains.GetContactReachabilityStatus
  ( -- * Creating a request
    GetContactReachabilityStatus (..),
    mkGetContactReachabilityStatus,

    -- ** Request lenses
    gcrsDomainName,

    -- * Destructuring the response
    GetContactReachabilityStatusResponse (..),
    mkGetContactReachabilityStatusResponse,

    -- ** Response lenses
    gcrsrrsDomainName,
    gcrsrrsStatus,
    gcrsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | /See:/ 'mkGetContactReachabilityStatus' smart constructor.
newtype GetContactReachabilityStatus = GetContactReachabilityStatus'
  { -- | The name of the domain for which you want to know whether the registrant contact has confirmed that the email address is valid.
    domainName :: Core.Maybe Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetContactReachabilityStatus' value with any optional fields omitted.
mkGetContactReachabilityStatus ::
  GetContactReachabilityStatus
mkGetContactReachabilityStatus =
  GetContactReachabilityStatus' {domainName = Core.Nothing}

-- | The name of the domain for which you want to know whether the registrant contact has confirmed that the email address is valid.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsDomainName :: Lens.Lens' GetContactReachabilityStatus (Core.Maybe Types.DomainName)
gcrsDomainName = Lens.field @"domainName"
{-# DEPRECATED gcrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.FromJSON GetContactReachabilityStatus where
  toJSON GetContactReachabilityStatus {..} =
    Core.object
      (Core.catMaybes [("domainName" Core..=) Core.<$> domainName])

instance Core.AWSRequest GetContactReachabilityStatus where
  type
    Rs GetContactReachabilityStatus =
      GetContactReachabilityStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Route53Domains_v20140515.GetContactReachabilityStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactReachabilityStatusResponse'
            Core.<$> (x Core..:? "domainName")
            Core.<*> (x Core..:? "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetContactReachabilityStatusResponse' smart constructor.
data GetContactReachabilityStatusResponse = GetContactReachabilityStatusResponse'
  { -- | The domain name for which you requested the reachability status.
    domainName :: Core.Maybe Types.DomainName,
    -- | Whether the registrant contact has responded. Values include the following:
    --
    --
    --     * PENDING
    --
    --     * We sent the confirmation email and haven't received a response yet.
    --
    --
    --     * DONE
    --
    --     * We sent the email and got confirmation from the registrant contact.
    --
    --
    --     * EXPIRED
    --
    --     * The time limit expired before the registrant contact responded.
    status :: Core.Maybe Types.ReachabilityStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContactReachabilityStatusResponse' value with any optional fields omitted.
mkGetContactReachabilityStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetContactReachabilityStatusResponse
mkGetContactReachabilityStatusResponse responseStatus =
  GetContactReachabilityStatusResponse'
    { domainName = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | The domain name for which you requested the reachability status.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsrrsDomainName :: Lens.Lens' GetContactReachabilityStatusResponse (Core.Maybe Types.DomainName)
gcrsrrsDomainName = Lens.field @"domainName"
{-# DEPRECATED gcrsrrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Whether the registrant contact has responded. Values include the following:
--
--
--     * PENDING
--
--     * We sent the confirmation email and haven't received a response yet.
--
--
--     * DONE
--
--     * We sent the email and got confirmation from the registrant contact.
--
--
--     * EXPIRED
--
--     * The time limit expired before the registrant contact responded.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsrrsStatus :: Lens.Lens' GetContactReachabilityStatusResponse (Core.Maybe Types.ReachabilityStatus)
gcrsrrsStatus = Lens.field @"status"
{-# DEPRECATED gcrsrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsrrsResponseStatus :: Lens.Lens' GetContactReachabilityStatusResponse Core.Int
gcrsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
