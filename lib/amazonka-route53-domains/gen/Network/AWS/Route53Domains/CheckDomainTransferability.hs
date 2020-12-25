{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.CheckDomainTransferability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks whether a domain name can be transferred to Amazon Route 53.
module Network.AWS.Route53Domains.CheckDomainTransferability
  ( -- * Creating a request
    CheckDomainTransferability (..),
    mkCheckDomainTransferability,

    -- ** Request lenses
    cdtDomainName,
    cdtAuthCode,

    -- * Destructuring the response
    CheckDomainTransferabilityResponse (..),
    mkCheckDomainTransferabilityResponse,

    -- ** Response lenses
    cdtrrsTransferability,
    cdtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The CheckDomainTransferability request contains the following elements.
--
-- /See:/ 'mkCheckDomainTransferability' smart constructor.
data CheckDomainTransferability = CheckDomainTransferability'
  { -- | The name of the domain that you want to transfer to Route 53. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
    --
    -- The domain name can contain only the following characters:
    --
    --     * Letters a through z. Domain names are not case sensitive.
    --
    --
    --     * Numbers 0 through 9.
    --
    --
    --     * Hyphen (-). You can't specify a hyphen at the beginning or end of a label.
    --
    --
    --     * Period (.) to separate the labels in the name, such as the @.@ in @example.com@ .
    domainName :: Types.DomainName,
    -- | If the registrar for the top-level domain (TLD) requires an authorization code to transfer the domain, the code that you got from the current registrar for the domain.
    authCode :: Core.Maybe Types.DomainAuthCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CheckDomainTransferability' value with any optional fields omitted.
mkCheckDomainTransferability ::
  -- | 'domainName'
  Types.DomainName ->
  CheckDomainTransferability
mkCheckDomainTransferability domainName =
  CheckDomainTransferability' {domainName, authCode = Core.Nothing}

-- | The name of the domain that you want to transfer to Route 53. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- The domain name can contain only the following characters:
--
--     * Letters a through z. Domain names are not case sensitive.
--
--
--     * Numbers 0 through 9.
--
--
--     * Hyphen (-). You can't specify a hyphen at the beginning or end of a label.
--
--
--     * Period (.) to separate the labels in the name, such as the @.@ in @example.com@ .
--
--
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtDomainName :: Lens.Lens' CheckDomainTransferability Types.DomainName
cdtDomainName = Lens.field @"domainName"
{-# DEPRECATED cdtDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | If the registrar for the top-level domain (TLD) requires an authorization code to transfer the domain, the code that you got from the current registrar for the domain.
--
-- /Note:/ Consider using 'authCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtAuthCode :: Lens.Lens' CheckDomainTransferability (Core.Maybe Types.DomainAuthCode)
cdtAuthCode = Lens.field @"authCode"
{-# DEPRECATED cdtAuthCode "Use generic-lens or generic-optics with 'authCode' instead." #-}

instance Core.FromJSON CheckDomainTransferability where
  toJSON CheckDomainTransferability {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainName" Core..= domainName),
            ("AuthCode" Core..=) Core.<$> authCode
          ]
      )

instance Core.AWSRequest CheckDomainTransferability where
  type
    Rs CheckDomainTransferability =
      CheckDomainTransferabilityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Route53Domains_v20140515.CheckDomainTransferability"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CheckDomainTransferabilityResponse'
            Core.<$> (x Core..: "Transferability")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The CheckDomainTransferability response includes the following elements.
--
-- /See:/ 'mkCheckDomainTransferabilityResponse' smart constructor.
data CheckDomainTransferabilityResponse = CheckDomainTransferabilityResponse'
  { -- | A complex type that contains information about whether the specified domain can be transferred to Route 53.
    transferability :: Types.DomainTransferability,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CheckDomainTransferabilityResponse' value with any optional fields omitted.
mkCheckDomainTransferabilityResponse ::
  -- | 'transferability'
  Types.DomainTransferability ->
  -- | 'responseStatus'
  Core.Int ->
  CheckDomainTransferabilityResponse
mkCheckDomainTransferabilityResponse transferability responseStatus =
  CheckDomainTransferabilityResponse'
    { transferability,
      responseStatus
    }

-- | A complex type that contains information about whether the specified domain can be transferred to Route 53.
--
-- /Note:/ Consider using 'transferability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtrrsTransferability :: Lens.Lens' CheckDomainTransferabilityResponse Types.DomainTransferability
cdtrrsTransferability = Lens.field @"transferability"
{-# DEPRECATED cdtrrsTransferability "Use generic-lens or generic-optics with 'transferability' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtrrsResponseStatus :: Lens.Lens' CheckDomainTransferabilityResponse Core.Int
cdtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
