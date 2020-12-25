{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.CheckDomainAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation checks the availability of one domain name. Note that if the availability status of a domain is pending, you must submit another request to determine the availability of the domain name.
module Network.AWS.Route53Domains.CheckDomainAvailability
  ( -- * Creating a request
    CheckDomainAvailability (..),
    mkCheckDomainAvailability,

    -- ** Request lenses
    cdaDomainName,
    cdaIdnLangCode,

    -- * Destructuring the response
    CheckDomainAvailabilityResponse (..),
    mkCheckDomainAvailabilityResponse,

    -- ** Response lenses
    cdarrsAvailability,
    cdarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The CheckDomainAvailability request contains the following elements.
--
-- /See:/ 'mkCheckDomainAvailability' smart constructor.
data CheckDomainAvailability = CheckDomainAvailability'
  { -- | The name of the domain that you want to get availability for. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
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
    -- Internationalized domain names are not supported for some top-level domains. To determine whether the TLD that you want to use supports internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> . For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html#domain-name-format-idns Formatting Internationalized Domain Names> .
    domainName :: Types.DomainName,
    -- | Reserved for future use.
    idnLangCode :: Core.Maybe Types.IdnLangCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CheckDomainAvailability' value with any optional fields omitted.
mkCheckDomainAvailability ::
  -- | 'domainName'
  Types.DomainName ->
  CheckDomainAvailability
mkCheckDomainAvailability domainName =
  CheckDomainAvailability' {domainName, idnLangCode = Core.Nothing}

-- | The name of the domain that you want to get availability for. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
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
-- Internationalized domain names are not supported for some top-level domains. To determine whether the TLD that you want to use supports internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> . For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html#domain-name-format-idns Formatting Internationalized Domain Names> .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaDomainName :: Lens.Lens' CheckDomainAvailability Types.DomainName
cdaDomainName = Lens.field @"domainName"
{-# DEPRECATED cdaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'idnLangCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaIdnLangCode :: Lens.Lens' CheckDomainAvailability (Core.Maybe Types.IdnLangCode)
cdaIdnLangCode = Lens.field @"idnLangCode"
{-# DEPRECATED cdaIdnLangCode "Use generic-lens or generic-optics with 'idnLangCode' instead." #-}

instance Core.FromJSON CheckDomainAvailability where
  toJSON CheckDomainAvailability {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainName" Core..= domainName),
            ("IdnLangCode" Core..=) Core.<$> idnLangCode
          ]
      )

instance Core.AWSRequest CheckDomainAvailability where
  type Rs CheckDomainAvailability = CheckDomainAvailabilityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Route53Domains_v20140515.CheckDomainAvailability"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CheckDomainAvailabilityResponse'
            Core.<$> (x Core..: "Availability") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The CheckDomainAvailability response includes the following elements.
--
-- /See:/ 'mkCheckDomainAvailabilityResponse' smart constructor.
data CheckDomainAvailabilityResponse = CheckDomainAvailabilityResponse'
  { -- | Whether the domain name is available for registering.
    --
    -- Valid values:
    --
    --     * AVAILABLE
    --
    --     * The domain name is available.
    --
    --
    --     * AVAILABLE_RESERVED
    --
    --     * The domain name is reserved under specific conditions.
    --
    --
    --     * AVAILABLE_PREORDER
    --
    --     * The domain name is available and can be preordered.
    --
    --
    --     * DONT_KNOW
    --
    --     * The TLD registry didn't reply with a definitive answer about whether the domain name is available. Route 53 can return this response for a variety of reasons, for example, the registry is performing maintenance. Try again later.
    --
    --
    --     * PENDING
    --
    --     * The TLD registry didn't return a response in the expected amount of time. When the response is delayed, it usually takes just a few extra seconds. You can resubmit the request immediately.
    --
    --
    --     * RESERVED
    --
    --     * The domain name has been reserved for another person or organization.
    --
    --
    --     * UNAVAILABLE
    --
    --     * The domain name is not available.
    --
    --
    --     * UNAVAILABLE_PREMIUM
    --
    --     * The domain name is not available.
    --
    --
    --     * UNAVAILABLE_RESTRICTED
    --
    --     * The domain name is forbidden.
    availability :: Types.DomainAvailability,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CheckDomainAvailabilityResponse' value with any optional fields omitted.
mkCheckDomainAvailabilityResponse ::
  -- | 'availability'
  Types.DomainAvailability ->
  -- | 'responseStatus'
  Core.Int ->
  CheckDomainAvailabilityResponse
mkCheckDomainAvailabilityResponse availability responseStatus =
  CheckDomainAvailabilityResponse' {availability, responseStatus}

-- | Whether the domain name is available for registering.
--
-- Valid values:
--
--     * AVAILABLE
--
--     * The domain name is available.
--
--
--     * AVAILABLE_RESERVED
--
--     * The domain name is reserved under specific conditions.
--
--
--     * AVAILABLE_PREORDER
--
--     * The domain name is available and can be preordered.
--
--
--     * DONT_KNOW
--
--     * The TLD registry didn't reply with a definitive answer about whether the domain name is available. Route 53 can return this response for a variety of reasons, for example, the registry is performing maintenance. Try again later.
--
--
--     * PENDING
--
--     * The TLD registry didn't return a response in the expected amount of time. When the response is delayed, it usually takes just a few extra seconds. You can resubmit the request immediately.
--
--
--     * RESERVED
--
--     * The domain name has been reserved for another person or organization.
--
--
--     * UNAVAILABLE
--
--     * The domain name is not available.
--
--
--     * UNAVAILABLE_PREMIUM
--
--     * The domain name is not available.
--
--
--     * UNAVAILABLE_RESTRICTED
--
--     * The domain name is forbidden.
--
--
--
-- /Note:/ Consider using 'availability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdarrsAvailability :: Lens.Lens' CheckDomainAvailabilityResponse Types.DomainAvailability
cdarrsAvailability = Lens.field @"availability"
{-# DEPRECATED cdarrsAvailability "Use generic-lens or generic-optics with 'availability' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdarrsResponseStatus :: Lens.Lens' CheckDomainAvailabilityResponse Core.Int
cdarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
