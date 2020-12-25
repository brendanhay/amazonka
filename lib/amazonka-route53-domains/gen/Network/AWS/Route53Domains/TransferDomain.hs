{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.TransferDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transfers a domain from another registrar to Amazon Route 53. When the transfer is complete, the domain is registered either with Amazon Registrar (for .com, .net, and .org domains) or with our registrar associate, Gandi (for all other TLDs).
--
-- For more information about transferring domains, see the following topics:
--
--     * For transfer requirements, a detailed procedure, and information about viewing the status of a domain that you're transferring to Route 53, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-transfer-to-route-53.html Transferring Registration for a Domain to Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
--
--     * For information about how to transfer a domain from one AWS account to another, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> .
--
--
--     * For information about how to transfer a domain to another domain registrar, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-transfer-from-route-53.html Transferring a Domain from Amazon Route 53 to Another Registrar> in the /Amazon Route 53 Developer Guide/ .
--
--
-- If the registrar for your domain is also the DNS service provider for the domain, we highly recommend that you transfer your DNS service to Route 53 or to another DNS service provider before you transfer your registration. Some registrars provide free DNS service when you purchase a domain registration. When you transfer the registration, the previous registrar will not renew your domain registration and could end your DNS service at any time.
-- /Important:/ If the registrar for your domain is also the DNS service provider for the domain and you don't transfer DNS service to another provider, your website, email, and the web applications associated with the domain might become unavailable.
-- If the transfer is successful, this method returns an operation ID that you can use to track the progress and completion of the action. If the transfer doesn't complete successfully, the domain registrant will be notified by email.
module Network.AWS.Route53Domains.TransferDomain
  ( -- * Creating a request
    TransferDomain (..),
    mkTransferDomain,

    -- ** Request lenses
    tdDomainName,
    tdDurationInYears,
    tdAdminContact,
    tdRegistrantContact,
    tdTechContact,
    tdAuthCode,
    tdAutoRenew,
    tdIdnLangCode,
    tdNameservers,
    tdPrivacyProtectAdminContact,
    tdPrivacyProtectRegistrantContact,
    tdPrivacyProtectTechContact,

    -- * Destructuring the response
    TransferDomainResponse (..),
    mkTransferDomainResponse,

    -- ** Response lenses
    tdrrsOperationId,
    tdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The TransferDomain request includes the following elements.
--
-- /See:/ 'mkTransferDomain' smart constructor.
data TransferDomain = TransferDomain'
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
    -- | The number of years that you want to register the domain for. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain.
    --
    -- Default: 1
    durationInYears :: Core.Natural,
    -- | Provides detailed contact information.
    adminContact :: Types.ContactDetail,
    -- | Provides detailed contact information.
    registrantContact :: Types.ContactDetail,
    -- | Provides detailed contact information.
    techContact :: Types.ContactDetail,
    -- | The authorization code for the domain. You get this value from the current registrar.
    authCode :: Core.Maybe Types.AuthCode,
    -- | Indicates whether the domain will be automatically renewed (true) or not (false). Autorenewal only takes effect after the account is charged.
    --
    -- Default: true
    autoRenew :: Core.Maybe Core.Bool,
    -- | Reserved for future use.
    idnLangCode :: Core.Maybe Types.LangCode,
    -- | Contains details for the host and glue IP addresses.
    nameservers :: Core.Maybe [Types.Nameserver],
    -- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
    --
    -- Default: @true@
    privacyProtectAdminContact :: Core.Maybe Core.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
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

-- | Creates a 'TransferDomain' value with any optional fields omitted.
mkTransferDomain ::
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
  TransferDomain
mkTransferDomain
  domainName
  durationInYears
  adminContact
  registrantContact
  techContact =
    TransferDomain'
      { domainName,
        durationInYears,
        adminContact,
        registrantContact,
        techContact,
        authCode = Core.Nothing,
        autoRenew = Core.Nothing,
        idnLangCode = Core.Nothing,
        nameservers = Core.Nothing,
        privacyProtectAdminContact = Core.Nothing,
        privacyProtectRegistrantContact = Core.Nothing,
        privacyProtectTechContact = Core.Nothing
      }

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
tdDomainName :: Lens.Lens' TransferDomain Types.DomainName
tdDomainName = Lens.field @"domainName"
{-# DEPRECATED tdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The number of years that you want to register the domain for. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain.
--
-- Default: 1
--
-- /Note:/ Consider using 'durationInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdDurationInYears :: Lens.Lens' TransferDomain Core.Natural
tdDurationInYears = Lens.field @"durationInYears"
{-# DEPRECATED tdDurationInYears "Use generic-lens or generic-optics with 'durationInYears' instead." #-}

-- | Provides detailed contact information.
--
-- /Note:/ Consider using 'adminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAdminContact :: Lens.Lens' TransferDomain Types.ContactDetail
tdAdminContact = Lens.field @"adminContact"
{-# DEPRECATED tdAdminContact "Use generic-lens or generic-optics with 'adminContact' instead." #-}

-- | Provides detailed contact information.
--
-- /Note:/ Consider using 'registrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRegistrantContact :: Lens.Lens' TransferDomain Types.ContactDetail
tdRegistrantContact = Lens.field @"registrantContact"
{-# DEPRECATED tdRegistrantContact "Use generic-lens or generic-optics with 'registrantContact' instead." #-}

-- | Provides detailed contact information.
--
-- /Note:/ Consider using 'techContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTechContact :: Lens.Lens' TransferDomain Types.ContactDetail
tdTechContact = Lens.field @"techContact"
{-# DEPRECATED tdTechContact "Use generic-lens or generic-optics with 'techContact' instead." #-}

-- | The authorization code for the domain. You get this value from the current registrar.
--
-- /Note:/ Consider using 'authCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAuthCode :: Lens.Lens' TransferDomain (Core.Maybe Types.AuthCode)
tdAuthCode = Lens.field @"authCode"
{-# DEPRECATED tdAuthCode "Use generic-lens or generic-optics with 'authCode' instead." #-}

-- | Indicates whether the domain will be automatically renewed (true) or not (false). Autorenewal only takes effect after the account is charged.
--
-- Default: true
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAutoRenew :: Lens.Lens' TransferDomain (Core.Maybe Core.Bool)
tdAutoRenew = Lens.field @"autoRenew"
{-# DEPRECATED tdAutoRenew "Use generic-lens or generic-optics with 'autoRenew' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'idnLangCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdIdnLangCode :: Lens.Lens' TransferDomain (Core.Maybe Types.LangCode)
tdIdnLangCode = Lens.field @"idnLangCode"
{-# DEPRECATED tdIdnLangCode "Use generic-lens or generic-optics with 'idnLangCode' instead." #-}

-- | Contains details for the host and glue IP addresses.
--
-- /Note:/ Consider using 'nameservers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdNameservers :: Lens.Lens' TransferDomain (Core.Maybe [Types.Nameserver])
tdNameservers = Lens.field @"nameservers"
{-# DEPRECATED tdNameservers "Use generic-lens or generic-optics with 'nameservers' instead." #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
--
-- Default: @true@
--
-- /Note:/ Consider using 'privacyProtectAdminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPrivacyProtectAdminContact :: Lens.Lens' TransferDomain (Core.Maybe Core.Bool)
tdPrivacyProtectAdminContact = Lens.field @"privacyProtectAdminContact"
{-# DEPRECATED tdPrivacyProtectAdminContact "Use generic-lens or generic-optics with 'privacyProtectAdminContact' instead." #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
--
-- Default: @true@
--
-- /Note:/ Consider using 'privacyProtectRegistrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPrivacyProtectRegistrantContact :: Lens.Lens' TransferDomain (Core.Maybe Core.Bool)
tdPrivacyProtectRegistrantContact = Lens.field @"privacyProtectRegistrantContact"
{-# DEPRECATED tdPrivacyProtectRegistrantContact "Use generic-lens or generic-optics with 'privacyProtectRegistrantContact' instead." #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact.
--
-- Default: @true@
--
-- /Note:/ Consider using 'privacyProtectTechContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPrivacyProtectTechContact :: Lens.Lens' TransferDomain (Core.Maybe Core.Bool)
tdPrivacyProtectTechContact = Lens.field @"privacyProtectTechContact"
{-# DEPRECATED tdPrivacyProtectTechContact "Use generic-lens or generic-optics with 'privacyProtectTechContact' instead." #-}

instance Core.FromJSON TransferDomain where
  toJSON TransferDomain {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainName" Core..= domainName),
            Core.Just ("DurationInYears" Core..= durationInYears),
            Core.Just ("AdminContact" Core..= adminContact),
            Core.Just ("RegistrantContact" Core..= registrantContact),
            Core.Just ("TechContact" Core..= techContact),
            ("AuthCode" Core..=) Core.<$> authCode,
            ("AutoRenew" Core..=) Core.<$> autoRenew,
            ("IdnLangCode" Core..=) Core.<$> idnLangCode,
            ("Nameservers" Core..=) Core.<$> nameservers,
            ("PrivacyProtectAdminContact" Core..=)
              Core.<$> privacyProtectAdminContact,
            ("PrivacyProtectRegistrantContact" Core..=)
              Core.<$> privacyProtectRegistrantContact,
            ("PrivacyProtectTechContact" Core..=)
              Core.<$> privacyProtectTechContact
          ]
      )

instance Core.AWSRequest TransferDomain where
  type Rs TransferDomain = TransferDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53Domains_v20140515.TransferDomain")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          TransferDomainResponse'
            Core.<$> (x Core..: "OperationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The TransferDomain response includes the following element.
--
-- /See:/ 'mkTransferDomainResponse' smart constructor.
data TransferDomainResponse = TransferDomainResponse'
  { -- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
    operationId :: Types.OperationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransferDomainResponse' value with any optional fields omitted.
mkTransferDomainResponse ::
  -- | 'operationId'
  Types.OperationId ->
  -- | 'responseStatus'
  Core.Int ->
  TransferDomainResponse
mkTransferDomainResponse operationId responseStatus =
  TransferDomainResponse' {operationId, responseStatus}

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrrsOperationId :: Lens.Lens' TransferDomainResponse Types.OperationId
tdrrsOperationId = Lens.field @"operationId"
{-# DEPRECATED tdrrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrrsResponseStatus :: Lens.Lens' TransferDomainResponse Core.Int
tdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
