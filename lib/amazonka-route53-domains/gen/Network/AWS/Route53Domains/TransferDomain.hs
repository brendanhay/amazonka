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
    tdPrivacyProtectTechContact,
    tdPrivacyProtectRegistrantContact,
    tdAutoRenew,
    tdRegistrantContact,
    tdDomainName,
    tdAdminContact,
    tdPrivacyProtectAdminContact,
    tdIDNLangCode,
    tdTechContact,
    tdDurationInYears,
    tdAuthCode,
    tdNameservers,

    -- * Destructuring the response
    TransferDomainResponse (..),
    mkTransferDomainResponse,

    -- ** Response lenses
    tdrsOperationId,
    tdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The TransferDomain request includes the following elements.
--
-- /See:/ 'mkTransferDomain' smart constructor.
data TransferDomain = TransferDomain'
  { -- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact.
    --
    -- Default: @true@
    privacyProtectTechContact :: Lude.Maybe Lude.Bool,
    -- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
    --
    -- Default: @true@
    privacyProtectRegistrantContact :: Lude.Maybe Lude.Bool,
    -- | Indicates whether the domain will be automatically renewed (true) or not (false). Autorenewal only takes effect after the account is charged.
    --
    -- Default: true
    autoRenew :: Lude.Maybe Lude.Bool,
    -- | Provides detailed contact information.
    registrantContact :: ContactDetail,
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
    domainName :: Lude.Text,
    -- | Provides detailed contact information.
    adminContact :: ContactDetail,
    -- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
    --
    -- Default: @true@
    privacyProtectAdminContact :: Lude.Maybe Lude.Bool,
    -- | Reserved for future use.
    idNLangCode :: Lude.Maybe Lude.Text,
    -- | Provides detailed contact information.
    techContact :: ContactDetail,
    -- | The number of years that you want to register the domain for. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain.
    --
    -- Default: 1
    durationInYears :: Lude.Natural,
    -- | The authorization code for the domain. You get this value from the current registrar.
    authCode :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Contains details for the host and glue IP addresses.
    nameservers :: Lude.Maybe [Nameserver]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransferDomain' with the minimum fields required to make a request.
--
-- * 'privacyProtectTechContact' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact.
--
-- Default: @true@
-- * 'privacyProtectRegistrantContact' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
--
-- Default: @true@
-- * 'autoRenew' - Indicates whether the domain will be automatically renewed (true) or not (false). Autorenewal only takes effect after the account is charged.
--
-- Default: true
-- * 'registrantContact' - Provides detailed contact information.
-- * 'domainName' - The name of the domain that you want to transfer to Route 53. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
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
-- * 'adminContact' - Provides detailed contact information.
-- * 'privacyProtectAdminContact' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
--
-- Default: @true@
-- * 'idNLangCode' - Reserved for future use.
-- * 'techContact' - Provides detailed contact information.
-- * 'durationInYears' - The number of years that you want to register the domain for. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain.
--
-- Default: 1
-- * 'authCode' - The authorization code for the domain. You get this value from the current registrar.
-- * 'nameservers' - Contains details for the host and glue IP addresses.
mkTransferDomain ::
  -- | 'registrantContact'
  ContactDetail ->
  -- | 'domainName'
  Lude.Text ->
  -- | 'adminContact'
  ContactDetail ->
  -- | 'techContact'
  ContactDetail ->
  -- | 'durationInYears'
  Lude.Natural ->
  TransferDomain
mkTransferDomain
  pRegistrantContact_
  pDomainName_
  pAdminContact_
  pTechContact_
  pDurationInYears_ =
    TransferDomain'
      { privacyProtectTechContact = Lude.Nothing,
        privacyProtectRegistrantContact = Lude.Nothing,
        autoRenew = Lude.Nothing,
        registrantContact = pRegistrantContact_,
        domainName = pDomainName_,
        adminContact = pAdminContact_,
        privacyProtectAdminContact = Lude.Nothing,
        idNLangCode = Lude.Nothing,
        techContact = pTechContact_,
        durationInYears = pDurationInYears_,
        authCode = Lude.Nothing,
        nameservers = Lude.Nothing
      }

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact.
--
-- Default: @true@
--
-- /Note:/ Consider using 'privacyProtectTechContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPrivacyProtectTechContact :: Lens.Lens' TransferDomain (Lude.Maybe Lude.Bool)
tdPrivacyProtectTechContact = Lens.lens (privacyProtectTechContact :: TransferDomain -> Lude.Maybe Lude.Bool) (\s a -> s {privacyProtectTechContact = a} :: TransferDomain)
{-# DEPRECATED tdPrivacyProtectTechContact "Use generic-lens or generic-optics with 'privacyProtectTechContact' instead." #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
--
-- Default: @true@
--
-- /Note:/ Consider using 'privacyProtectRegistrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPrivacyProtectRegistrantContact :: Lens.Lens' TransferDomain (Lude.Maybe Lude.Bool)
tdPrivacyProtectRegistrantContact = Lens.lens (privacyProtectRegistrantContact :: TransferDomain -> Lude.Maybe Lude.Bool) (\s a -> s {privacyProtectRegistrantContact = a} :: TransferDomain)
{-# DEPRECATED tdPrivacyProtectRegistrantContact "Use generic-lens or generic-optics with 'privacyProtectRegistrantContact' instead." #-}

-- | Indicates whether the domain will be automatically renewed (true) or not (false). Autorenewal only takes effect after the account is charged.
--
-- Default: true
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAutoRenew :: Lens.Lens' TransferDomain (Lude.Maybe Lude.Bool)
tdAutoRenew = Lens.lens (autoRenew :: TransferDomain -> Lude.Maybe Lude.Bool) (\s a -> s {autoRenew = a} :: TransferDomain)
{-# DEPRECATED tdAutoRenew "Use generic-lens or generic-optics with 'autoRenew' instead." #-}

-- | Provides detailed contact information.
--
-- /Note:/ Consider using 'registrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRegistrantContact :: Lens.Lens' TransferDomain ContactDetail
tdRegistrantContact = Lens.lens (registrantContact :: TransferDomain -> ContactDetail) (\s a -> s {registrantContact = a} :: TransferDomain)
{-# DEPRECATED tdRegistrantContact "Use generic-lens or generic-optics with 'registrantContact' instead." #-}

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
tdDomainName :: Lens.Lens' TransferDomain Lude.Text
tdDomainName = Lens.lens (domainName :: TransferDomain -> Lude.Text) (\s a -> s {domainName = a} :: TransferDomain)
{-# DEPRECATED tdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Provides detailed contact information.
--
-- /Note:/ Consider using 'adminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAdminContact :: Lens.Lens' TransferDomain ContactDetail
tdAdminContact = Lens.lens (adminContact :: TransferDomain -> ContactDetail) (\s a -> s {adminContact = a} :: TransferDomain)
{-# DEPRECATED tdAdminContact "Use generic-lens or generic-optics with 'adminContact' instead." #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
--
-- Default: @true@
--
-- /Note:/ Consider using 'privacyProtectAdminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPrivacyProtectAdminContact :: Lens.Lens' TransferDomain (Lude.Maybe Lude.Bool)
tdPrivacyProtectAdminContact = Lens.lens (privacyProtectAdminContact :: TransferDomain -> Lude.Maybe Lude.Bool) (\s a -> s {privacyProtectAdminContact = a} :: TransferDomain)
{-# DEPRECATED tdPrivacyProtectAdminContact "Use generic-lens or generic-optics with 'privacyProtectAdminContact' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'idNLangCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdIDNLangCode :: Lens.Lens' TransferDomain (Lude.Maybe Lude.Text)
tdIDNLangCode = Lens.lens (idNLangCode :: TransferDomain -> Lude.Maybe Lude.Text) (\s a -> s {idNLangCode = a} :: TransferDomain)
{-# DEPRECATED tdIDNLangCode "Use generic-lens or generic-optics with 'idNLangCode' instead." #-}

-- | Provides detailed contact information.
--
-- /Note:/ Consider using 'techContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTechContact :: Lens.Lens' TransferDomain ContactDetail
tdTechContact = Lens.lens (techContact :: TransferDomain -> ContactDetail) (\s a -> s {techContact = a} :: TransferDomain)
{-# DEPRECATED tdTechContact "Use generic-lens or generic-optics with 'techContact' instead." #-}

-- | The number of years that you want to register the domain for. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain.
--
-- Default: 1
--
-- /Note:/ Consider using 'durationInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdDurationInYears :: Lens.Lens' TransferDomain Lude.Natural
tdDurationInYears = Lens.lens (durationInYears :: TransferDomain -> Lude.Natural) (\s a -> s {durationInYears = a} :: TransferDomain)
{-# DEPRECATED tdDurationInYears "Use generic-lens or generic-optics with 'durationInYears' instead." #-}

-- | The authorization code for the domain. You get this value from the current registrar.
--
-- /Note:/ Consider using 'authCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAuthCode :: Lens.Lens' TransferDomain (Lude.Maybe (Lude.Sensitive Lude.Text))
tdAuthCode = Lens.lens (authCode :: TransferDomain -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authCode = a} :: TransferDomain)
{-# DEPRECATED tdAuthCode "Use generic-lens or generic-optics with 'authCode' instead." #-}

-- | Contains details for the host and glue IP addresses.
--
-- /Note:/ Consider using 'nameservers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdNameservers :: Lens.Lens' TransferDomain (Lude.Maybe [Nameserver])
tdNameservers = Lens.lens (nameservers :: TransferDomain -> Lude.Maybe [Nameserver]) (\s a -> s {nameservers = a} :: TransferDomain)
{-# DEPRECATED tdNameservers "Use generic-lens or generic-optics with 'nameservers' instead." #-}

instance Lude.AWSRequest TransferDomain where
  type Rs TransferDomain = TransferDomainResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          TransferDomainResponse'
            Lude.<$> (x Lude..:> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TransferDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53Domains_v20140515.TransferDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TransferDomain where
  toJSON TransferDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PrivacyProtectTechContact" Lude..=)
              Lude.<$> privacyProtectTechContact,
            ("PrivacyProtectRegistrantContact" Lude..=)
              Lude.<$> privacyProtectRegistrantContact,
            ("AutoRenew" Lude..=) Lude.<$> autoRenew,
            Lude.Just ("RegistrantContact" Lude..= registrantContact),
            Lude.Just ("DomainName" Lude..= domainName),
            Lude.Just ("AdminContact" Lude..= adminContact),
            ("PrivacyProtectAdminContact" Lude..=)
              Lude.<$> privacyProtectAdminContact,
            ("IdnLangCode" Lude..=) Lude.<$> idNLangCode,
            Lude.Just ("TechContact" Lude..= techContact),
            Lude.Just ("DurationInYears" Lude..= durationInYears),
            ("AuthCode" Lude..=) Lude.<$> authCode,
            ("Nameservers" Lude..=) Lude.<$> nameservers
          ]
      )

instance Lude.ToPath TransferDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery TransferDomain where
  toQuery = Lude.const Lude.mempty

-- | The TransferDomain response includes the following element.
--
-- /See:/ 'mkTransferDomainResponse' smart constructor.
data TransferDomainResponse = TransferDomainResponse'
  { -- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
    operationId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransferDomainResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
-- * 'responseStatus' - The response status code.
mkTransferDomainResponse ::
  -- | 'operationId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  TransferDomainResponse
mkTransferDomainResponse pOperationId_ pResponseStatus_ =
  TransferDomainResponse'
    { operationId = pOperationId_,
      responseStatus = pResponseStatus_
    }

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrsOperationId :: Lens.Lens' TransferDomainResponse Lude.Text
tdrsOperationId = Lens.lens (operationId :: TransferDomainResponse -> Lude.Text) (\s a -> s {operationId = a} :: TransferDomainResponse)
{-# DEPRECATED tdrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrsResponseStatus :: Lens.Lens' TransferDomainResponse Lude.Int
tdrsResponseStatus = Lens.lens (responseStatus :: TransferDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TransferDomainResponse)
{-# DEPRECATED tdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
