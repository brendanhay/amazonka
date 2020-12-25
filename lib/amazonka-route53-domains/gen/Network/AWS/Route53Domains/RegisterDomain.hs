{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.RegisterDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation registers a domain. Domains are registered either by Amazon Registrar (for .com, .net, and .org domains) or by our registrar associate, Gandi (for all other domains). For some top-level domains (TLDs), this operation requires extra parameters.
--
-- When you register a domain, Amazon Route 53 does the following:
--
--     * Creates a Route 53 hosted zone that has the same name as the domain. Route 53 assigns four name servers to your hosted zone and automatically updates your domain registration with the names of these name servers.
--
--
--     * Enables autorenew, so your domain registration will renew automatically each year. We'll notify you in advance of the renewal date so you can choose whether to renew the registration.
--
--
--     * Optionally enables privacy protection, so WHOIS queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you don't enable privacy protection, WHOIS queries return the information that you entered for the registrant, admin, and tech contacts.
--
--
--     * If registration is successful, returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant is notified by email.
--
--
--     * Charges your AWS account an amount based on the top-level domain. For more information, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
module Network.AWS.Route53Domains.RegisterDomain
  ( -- * Creating a request
    RegisterDomain (..),
    mkRegisterDomain,

    -- ** Request lenses
    rDomainName,
    rDurationInYears,
    rAdminContact,
    rRegistrantContact,
    rTechContact,
    rAutoRenew,
    rIdnLangCode,
    rPrivacyProtectAdminContact,
    rPrivacyProtectRegistrantContact,
    rPrivacyProtectTechContact,

    -- * Destructuring the response
    RegisterDomainResponse (..),
    mkRegisterDomainResponse,

    -- ** Response lenses
    rdrrsOperationId,
    rdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The RegisterDomain request includes the following elements.
--
-- /See:/ 'mkRegisterDomain' smart constructor.
data RegisterDomain = RegisterDomain'
  { -- | The domain name that you want to register. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
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
    -- | The number of years that you want to register the domain for. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain. For the range of valid values for your domain, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
    --
    -- Default: 1
    durationInYears :: Core.Natural,
    -- | Provides detailed contact information. For information about the values that you specify for each element, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail> .
    adminContact :: Types.ContactDetail,
    -- | Provides detailed contact information. For information about the values that you specify for each element, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail> .
    registrantContact :: Types.ContactDetail,
    -- | Provides detailed contact information. For information about the values that you specify for each element, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail> .
    techContact :: Types.ContactDetail,
    -- | Indicates whether the domain will be automatically renewed (@true@ ) or not (@false@ ). Autorenewal only takes effect after the account is charged.
    --
    -- Default: @true@
    autoRenew :: Core.Maybe Core.Bool,
    -- | Reserved for future use.
    idnLangCode :: Core.Maybe Types.IdnLangCode,
    -- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
    --
    -- Default: @true@
    privacyProtectAdminContact :: Core.Maybe Core.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (the domain owner).
    --
    -- Default: @true@
    privacyProtectRegistrantContact :: Core.Maybe Core.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact.
    --
    -- Default: @true@
    privacyProtectTechContact :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDomain' value with any optional fields omitted.
mkRegisterDomain ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'durationInYears'
  Core.Natural ->
  -- | 'adminContact'
  Types.ContactDetail ->
  -- | 'registrantContact'
  Types.ContactDetail ->
  -- | 'techContact'
  Types.ContactDetail ->
  RegisterDomain
mkRegisterDomain
  domainName
  durationInYears
  adminContact
  registrantContact
  techContact =
    RegisterDomain'
      { domainName,
        durationInYears,
        adminContact,
        registrantContact,
        techContact,
        autoRenew = Core.Nothing,
        idnLangCode = Core.Nothing,
        privacyProtectAdminContact = Core.Nothing,
        privacyProtectRegistrantContact = Core.Nothing,
        privacyProtectTechContact = Core.Nothing
      }

-- | The domain name that you want to register. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
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
rDomainName :: Lens.Lens' RegisterDomain Types.DomainName
rDomainName = Lens.field @"domainName"
{-# DEPRECATED rDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The number of years that you want to register the domain for. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain. For the range of valid values for your domain, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- Default: 1
--
-- /Note:/ Consider using 'durationInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDurationInYears :: Lens.Lens' RegisterDomain Core.Natural
rDurationInYears = Lens.field @"durationInYears"
{-# DEPRECATED rDurationInYears "Use generic-lens or generic-optics with 'durationInYears' instead." #-}

-- | Provides detailed contact information. For information about the values that you specify for each element, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail> .
--
-- /Note:/ Consider using 'adminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAdminContact :: Lens.Lens' RegisterDomain Types.ContactDetail
rAdminContact = Lens.field @"adminContact"
{-# DEPRECATED rAdminContact "Use generic-lens or generic-optics with 'adminContact' instead." #-}

-- | Provides detailed contact information. For information about the values that you specify for each element, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail> .
--
-- /Note:/ Consider using 'registrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRegistrantContact :: Lens.Lens' RegisterDomain Types.ContactDetail
rRegistrantContact = Lens.field @"registrantContact"
{-# DEPRECATED rRegistrantContact "Use generic-lens or generic-optics with 'registrantContact' instead." #-}

-- | Provides detailed contact information. For information about the values that you specify for each element, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail> .
--
-- /Note:/ Consider using 'techContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTechContact :: Lens.Lens' RegisterDomain Types.ContactDetail
rTechContact = Lens.field @"techContact"
{-# DEPRECATED rTechContact "Use generic-lens or generic-optics with 'techContact' instead." #-}

-- | Indicates whether the domain will be automatically renewed (@true@ ) or not (@false@ ). Autorenewal only takes effect after the account is charged.
--
-- Default: @true@
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAutoRenew :: Lens.Lens' RegisterDomain (Core.Maybe Core.Bool)
rAutoRenew = Lens.field @"autoRenew"
{-# DEPRECATED rAutoRenew "Use generic-lens or generic-optics with 'autoRenew' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'idnLangCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rIdnLangCode :: Lens.Lens' RegisterDomain (Core.Maybe Types.IdnLangCode)
rIdnLangCode = Lens.field @"idnLangCode"
{-# DEPRECATED rIdnLangCode "Use generic-lens or generic-optics with 'idnLangCode' instead." #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
--
-- Default: @true@
--
-- /Note:/ Consider using 'privacyProtectAdminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPrivacyProtectAdminContact :: Lens.Lens' RegisterDomain (Core.Maybe Core.Bool)
rPrivacyProtectAdminContact = Lens.field @"privacyProtectAdminContact"
{-# DEPRECATED rPrivacyProtectAdminContact "Use generic-lens or generic-optics with 'privacyProtectAdminContact' instead." #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (the domain owner).
--
-- Default: @true@
--
-- /Note:/ Consider using 'privacyProtectRegistrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPrivacyProtectRegistrantContact :: Lens.Lens' RegisterDomain (Core.Maybe Core.Bool)
rPrivacyProtectRegistrantContact = Lens.field @"privacyProtectRegistrantContact"
{-# DEPRECATED rPrivacyProtectRegistrantContact "Use generic-lens or generic-optics with 'privacyProtectRegistrantContact' instead." #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact.
--
-- Default: @true@
--
-- /Note:/ Consider using 'privacyProtectTechContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPrivacyProtectTechContact :: Lens.Lens' RegisterDomain (Core.Maybe Core.Bool)
rPrivacyProtectTechContact = Lens.field @"privacyProtectTechContact"
{-# DEPRECATED rPrivacyProtectTechContact "Use generic-lens or generic-optics with 'privacyProtectTechContact' instead." #-}

instance Core.FromJSON RegisterDomain where
  toJSON RegisterDomain {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainName" Core..= domainName),
            Core.Just ("DurationInYears" Core..= durationInYears),
            Core.Just ("AdminContact" Core..= adminContact),
            Core.Just ("RegistrantContact" Core..= registrantContact),
            Core.Just ("TechContact" Core..= techContact),
            ("AutoRenew" Core..=) Core.<$> autoRenew,
            ("IdnLangCode" Core..=) Core.<$> idnLangCode,
            ("PrivacyProtectAdminContact" Core..=)
              Core.<$> privacyProtectAdminContact,
            ("PrivacyProtectRegistrantContact" Core..=)
              Core.<$> privacyProtectRegistrantContact,
            ("PrivacyProtectTechContact" Core..=)
              Core.<$> privacyProtectTechContact
          ]
      )

instance Core.AWSRequest RegisterDomain where
  type Rs RegisterDomain = RegisterDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53Domains_v20140515.RegisterDomain")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterDomainResponse'
            Core.<$> (x Core..: "OperationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The RegisterDomain response includes the following element.
--
-- /See:/ 'mkRegisterDomainResponse' smart constructor.
data RegisterDomainResponse = RegisterDomainResponse'
  { -- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
    operationId :: Types.OperationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDomainResponse' value with any optional fields omitted.
mkRegisterDomainResponse ::
  -- | 'operationId'
  Types.OperationId ->
  -- | 'responseStatus'
  Core.Int ->
  RegisterDomainResponse
mkRegisterDomainResponse operationId responseStatus =
  RegisterDomainResponse' {operationId, responseStatus}

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrrsOperationId :: Lens.Lens' RegisterDomainResponse Types.OperationId
rdrrsOperationId = Lens.field @"operationId"
{-# DEPRECATED rdrrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrrsResponseStatus :: Lens.Lens' RegisterDomainResponse Core.Int
rdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
