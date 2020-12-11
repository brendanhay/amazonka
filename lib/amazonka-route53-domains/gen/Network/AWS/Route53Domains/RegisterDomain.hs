{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rPrivacyProtectTechContact,
    rPrivacyProtectRegistrantContact,
    rAutoRenew,
    rPrivacyProtectAdminContact,
    rIDNLangCode,
    rDomainName,
    rDurationInYears,
    rAdminContact,
    rRegistrantContact,
    rTechContact,

    -- * Destructuring the response
    RegisterDomainResponse (..),
    mkRegisterDomainResponse,

    -- ** Response lenses
    rdrsResponseStatus,
    rdrsOperationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The RegisterDomain request includes the following elements.
--
-- /See:/ 'mkRegisterDomain' smart constructor.
data RegisterDomain = RegisterDomain'
  { privacyProtectTechContact ::
      Lude.Maybe Lude.Bool,
    privacyProtectRegistrantContact :: Lude.Maybe Lude.Bool,
    autoRenew :: Lude.Maybe Lude.Bool,
    privacyProtectAdminContact :: Lude.Maybe Lude.Bool,
    idNLangCode :: Lude.Maybe Lude.Text,
    domainName :: Lude.Text,
    durationInYears :: Lude.Natural,
    adminContact :: ContactDetail,
    registrantContact :: ContactDetail,
    techContact :: ContactDetail
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterDomain' with the minimum fields required to make a request.
--
-- * 'adminContact' - Provides detailed contact information. For information about the values that you specify for each element, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail> .
-- * 'autoRenew' - Indicates whether the domain will be automatically renewed (@true@ ) or not (@false@ ). Autorenewal only takes effect after the account is charged.
--
-- Default: @true@
-- * 'domainName' - The domain name that you want to register. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
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
-- * 'durationInYears' - The number of years that you want to register the domain for. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain. For the range of valid values for your domain, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- Default: 1
-- * 'idNLangCode' - Reserved for future use.
-- * 'privacyProtectAdminContact' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
--
-- Default: @true@
-- * 'privacyProtectRegistrantContact' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (the domain owner).
--
-- Default: @true@
-- * 'privacyProtectTechContact' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact.
--
-- Default: @true@
-- * 'registrantContact' - Provides detailed contact information. For information about the values that you specify for each element, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail> .
-- * 'techContact' - Provides detailed contact information. For information about the values that you specify for each element, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail> .
mkRegisterDomain ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'durationInYears'
  Lude.Natural ->
  -- | 'adminContact'
  ContactDetail ->
  -- | 'registrantContact'
  ContactDetail ->
  -- | 'techContact'
  ContactDetail ->
  RegisterDomain
mkRegisterDomain
  pDomainName_
  pDurationInYears_
  pAdminContact_
  pRegistrantContact_
  pTechContact_ =
    RegisterDomain'
      { privacyProtectTechContact = Lude.Nothing,
        privacyProtectRegistrantContact = Lude.Nothing,
        autoRenew = Lude.Nothing,
        privacyProtectAdminContact = Lude.Nothing,
        idNLangCode = Lude.Nothing,
        domainName = pDomainName_,
        durationInYears = pDurationInYears_,
        adminContact = pAdminContact_,
        registrantContact = pRegistrantContact_,
        techContact = pTechContact_
      }

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact.
--
-- Default: @true@
--
-- /Note:/ Consider using 'privacyProtectTechContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPrivacyProtectTechContact :: Lens.Lens' RegisterDomain (Lude.Maybe Lude.Bool)
rPrivacyProtectTechContact = Lens.lens (privacyProtectTechContact :: RegisterDomain -> Lude.Maybe Lude.Bool) (\s a -> s {privacyProtectTechContact = a} :: RegisterDomain)
{-# DEPRECATED rPrivacyProtectTechContact "Use generic-lens or generic-optics with 'privacyProtectTechContact' instead." #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (the domain owner).
--
-- Default: @true@
--
-- /Note:/ Consider using 'privacyProtectRegistrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPrivacyProtectRegistrantContact :: Lens.Lens' RegisterDomain (Lude.Maybe Lude.Bool)
rPrivacyProtectRegistrantContact = Lens.lens (privacyProtectRegistrantContact :: RegisterDomain -> Lude.Maybe Lude.Bool) (\s a -> s {privacyProtectRegistrantContact = a} :: RegisterDomain)
{-# DEPRECATED rPrivacyProtectRegistrantContact "Use generic-lens or generic-optics with 'privacyProtectRegistrantContact' instead." #-}

-- | Indicates whether the domain will be automatically renewed (@true@ ) or not (@false@ ). Autorenewal only takes effect after the account is charged.
--
-- Default: @true@
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAutoRenew :: Lens.Lens' RegisterDomain (Lude.Maybe Lude.Bool)
rAutoRenew = Lens.lens (autoRenew :: RegisterDomain -> Lude.Maybe Lude.Bool) (\s a -> s {autoRenew = a} :: RegisterDomain)
{-# DEPRECATED rAutoRenew "Use generic-lens or generic-optics with 'autoRenew' instead." #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
--
-- Default: @true@
--
-- /Note:/ Consider using 'privacyProtectAdminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPrivacyProtectAdminContact :: Lens.Lens' RegisterDomain (Lude.Maybe Lude.Bool)
rPrivacyProtectAdminContact = Lens.lens (privacyProtectAdminContact :: RegisterDomain -> Lude.Maybe Lude.Bool) (\s a -> s {privacyProtectAdminContact = a} :: RegisterDomain)
{-# DEPRECATED rPrivacyProtectAdminContact "Use generic-lens or generic-optics with 'privacyProtectAdminContact' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'idNLangCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rIDNLangCode :: Lens.Lens' RegisterDomain (Lude.Maybe Lude.Text)
rIDNLangCode = Lens.lens (idNLangCode :: RegisterDomain -> Lude.Maybe Lude.Text) (\s a -> s {idNLangCode = a} :: RegisterDomain)
{-# DEPRECATED rIDNLangCode "Use generic-lens or generic-optics with 'idNLangCode' instead." #-}

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
rDomainName :: Lens.Lens' RegisterDomain Lude.Text
rDomainName = Lens.lens (domainName :: RegisterDomain -> Lude.Text) (\s a -> s {domainName = a} :: RegisterDomain)
{-# DEPRECATED rDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The number of years that you want to register the domain for. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain. For the range of valid values for your domain, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- Default: 1
--
-- /Note:/ Consider using 'durationInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDurationInYears :: Lens.Lens' RegisterDomain Lude.Natural
rDurationInYears = Lens.lens (durationInYears :: RegisterDomain -> Lude.Natural) (\s a -> s {durationInYears = a} :: RegisterDomain)
{-# DEPRECATED rDurationInYears "Use generic-lens or generic-optics with 'durationInYears' instead." #-}

-- | Provides detailed contact information. For information about the values that you specify for each element, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail> .
--
-- /Note:/ Consider using 'adminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAdminContact :: Lens.Lens' RegisterDomain ContactDetail
rAdminContact = Lens.lens (adminContact :: RegisterDomain -> ContactDetail) (\s a -> s {adminContact = a} :: RegisterDomain)
{-# DEPRECATED rAdminContact "Use generic-lens or generic-optics with 'adminContact' instead." #-}

-- | Provides detailed contact information. For information about the values that you specify for each element, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail> .
--
-- /Note:/ Consider using 'registrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRegistrantContact :: Lens.Lens' RegisterDomain ContactDetail
rRegistrantContact = Lens.lens (registrantContact :: RegisterDomain -> ContactDetail) (\s a -> s {registrantContact = a} :: RegisterDomain)
{-# DEPRECATED rRegistrantContact "Use generic-lens or generic-optics with 'registrantContact' instead." #-}

-- | Provides detailed contact information. For information about the values that you specify for each element, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ContactDetail.html ContactDetail> .
--
-- /Note:/ Consider using 'techContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTechContact :: Lens.Lens' RegisterDomain ContactDetail
rTechContact = Lens.lens (techContact :: RegisterDomain -> ContactDetail) (\s a -> s {techContact = a} :: RegisterDomain)
{-# DEPRECATED rTechContact "Use generic-lens or generic-optics with 'techContact' instead." #-}

instance Lude.AWSRequest RegisterDomain where
  type Rs RegisterDomain = RegisterDomainResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterDomainResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "OperationId")
      )

instance Lude.ToHeaders RegisterDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53Domains_v20140515.RegisterDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterDomain where
  toJSON RegisterDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PrivacyProtectTechContact" Lude..=)
              Lude.<$> privacyProtectTechContact,
            ("PrivacyProtectRegistrantContact" Lude..=)
              Lude.<$> privacyProtectRegistrantContact,
            ("AutoRenew" Lude..=) Lude.<$> autoRenew,
            ("PrivacyProtectAdminContact" Lude..=)
              Lude.<$> privacyProtectAdminContact,
            ("IdnLangCode" Lude..=) Lude.<$> idNLangCode,
            Lude.Just ("DomainName" Lude..= domainName),
            Lude.Just ("DurationInYears" Lude..= durationInYears),
            Lude.Just ("AdminContact" Lude..= adminContact),
            Lude.Just ("RegistrantContact" Lude..= registrantContact),
            Lude.Just ("TechContact" Lude..= techContact)
          ]
      )

instance Lude.ToPath RegisterDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterDomain where
  toQuery = Lude.const Lude.mempty

-- | The RegisterDomain response includes the following element.
--
-- /See:/ 'mkRegisterDomainResponse' smart constructor.
data RegisterDomainResponse = RegisterDomainResponse'
  { responseStatus ::
      Lude.Int,
    operationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterDomainResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
-- * 'responseStatus' - The response status code.
mkRegisterDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'operationId'
  Lude.Text ->
  RegisterDomainResponse
mkRegisterDomainResponse pResponseStatus_ pOperationId_ =
  RegisterDomainResponse'
    { responseStatus = pResponseStatus_,
      operationId = pOperationId_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrsResponseStatus :: Lens.Lens' RegisterDomainResponse Lude.Int
rdrsResponseStatus = Lens.lens (responseStatus :: RegisterDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterDomainResponse)
{-# DEPRECATED rdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrsOperationId :: Lens.Lens' RegisterDomainResponse Lude.Text
rdrsOperationId = Lens.lens (operationId :: RegisterDomainResponse -> Lude.Text) (\s a -> s {operationId = a} :: RegisterDomainResponse)
{-# DEPRECATED rdrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}
