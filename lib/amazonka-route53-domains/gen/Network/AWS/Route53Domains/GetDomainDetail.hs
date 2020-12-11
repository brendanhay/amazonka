{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.GetDomainDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns detailed information about a specified domain that is associated with the current AWS account. Contact information for the domain is also returned as part of the output.
module Network.AWS.Route53Domains.GetDomainDetail
  ( -- * Creating a request
    GetDomainDetail (..),
    mkGetDomainDetail,

    -- ** Request lenses
    gddDomainName,

    -- * Destructuring the response
    GetDomainDetailResponse (..),
    mkGetDomainDetailResponse,

    -- ** Response lenses
    gddrsTechPrivacy,
    gddrsDNSSec,
    gddrsWhoIsServer,
    gddrsRegistryDomainId,
    gddrsRegistrantPrivacy,
    gddrsUpdatedDate,
    gddrsAdminPrivacy,
    gddrsAutoRenew,
    gddrsAbuseContactPhone,
    gddrsRegistrarURL,
    gddrsAbuseContactEmail,
    gddrsExpirationDate,
    gddrsCreationDate,
    gddrsRegistrarName,
    gddrsReseller,
    gddrsStatusList,
    gddrsResponseStatus,
    gddrsDomainName,
    gddrsNameservers,
    gddrsAdminContact,
    gddrsRegistrantContact,
    gddrsTechContact,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The GetDomainDetail request includes the following element.
--
-- /See:/ 'mkGetDomainDetail' smart constructor.
newtype GetDomainDetail = GetDomainDetail' {domainName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDomainDetail' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that you want to get detailed information about.
mkGetDomainDetail ::
  -- | 'domainName'
  Lude.Text ->
  GetDomainDetail
mkGetDomainDetail pDomainName_ =
  GetDomainDetail' {domainName = pDomainName_}

-- | The name of the domain that you want to get detailed information about.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddDomainName :: Lens.Lens' GetDomainDetail Lude.Text
gddDomainName = Lens.lens (domainName :: GetDomainDetail -> Lude.Text) (\s a -> s {domainName = a} :: GetDomainDetail)
{-# DEPRECATED gddDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest GetDomainDetail where
  type Rs GetDomainDetail = GetDomainDetailResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDomainDetailResponse'
            Lude.<$> (x Lude..?> "TechPrivacy")
            Lude.<*> (x Lude..?> "DnsSec")
            Lude.<*> (x Lude..?> "WhoIsServer")
            Lude.<*> (x Lude..?> "RegistryDomainId")
            Lude.<*> (x Lude..?> "RegistrantPrivacy")
            Lude.<*> (x Lude..?> "UpdatedDate")
            Lude.<*> (x Lude..?> "AdminPrivacy")
            Lude.<*> (x Lude..?> "AutoRenew")
            Lude.<*> (x Lude..?> "AbuseContactPhone")
            Lude.<*> (x Lude..?> "RegistrarUrl")
            Lude.<*> (x Lude..?> "AbuseContactEmail")
            Lude.<*> (x Lude..?> "ExpirationDate")
            Lude.<*> (x Lude..?> "CreationDate")
            Lude.<*> (x Lude..?> "RegistrarName")
            Lude.<*> (x Lude..?> "Reseller")
            Lude.<*> (x Lude..?> "StatusList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "DomainName")
            Lude.<*> (x Lude..?> "Nameservers" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "AdminContact")
            Lude.<*> (x Lude..:> "RegistrantContact")
            Lude.<*> (x Lude..:> "TechContact")
      )

instance Lude.ToHeaders GetDomainDetail where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53Domains_v20140515.GetDomainDetail" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDomainDetail where
  toJSON GetDomainDetail' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainName" Lude..= domainName)])

instance Lude.ToPath GetDomainDetail where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDomainDetail where
  toQuery = Lude.const Lude.mempty

-- | The GetDomainDetail response includes the following elements.
--
-- /See:/ 'mkGetDomainDetailResponse' smart constructor.
data GetDomainDetailResponse = GetDomainDetailResponse'
  { techPrivacy ::
      Lude.Maybe Lude.Bool,
    dnsSec :: Lude.Maybe Lude.Text,
    whoIsServer :: Lude.Maybe Lude.Text,
    registryDomainId :: Lude.Maybe Lude.Text,
    registrantPrivacy :: Lude.Maybe Lude.Bool,
    updatedDate :: Lude.Maybe Lude.Timestamp,
    adminPrivacy :: Lude.Maybe Lude.Bool,
    autoRenew :: Lude.Maybe Lude.Bool,
    abuseContactPhone :: Lude.Maybe Lude.Text,
    registrarURL :: Lude.Maybe Lude.Text,
    abuseContactEmail :: Lude.Maybe Lude.Text,
    expirationDate :: Lude.Maybe Lude.Timestamp,
    creationDate :: Lude.Maybe Lude.Timestamp,
    registrarName :: Lude.Maybe Lude.Text,
    reseller :: Lude.Maybe Lude.Text,
    statusList :: Lude.Maybe [Lude.Text],
    responseStatus :: Lude.Int,
    domainName :: Lude.Text,
    nameservers :: [Nameserver],
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

-- | Creates a value of 'GetDomainDetailResponse' with the minimum fields required to make a request.
--
-- * 'abuseContactEmail' - Email address to contact to report incorrect contact information for a domain, to report that the domain is being used to send spam, to report that someone is cybersquatting on a domain name, or report some other type of abuse.
-- * 'abuseContactPhone' - Phone number for reporting abuse.
-- * 'adminContact' - Provides details about the domain administrative contact.
-- * 'adminPrivacy' - Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the admin contact.
-- * 'autoRenew' - Specifies whether the domain registration is set to renew automatically.
-- * 'creationDate' - The date when the domain was created as found in the response to a WHOIS query. The date and time is in Unix time format and Coordinated Universal time (UTC).
-- * 'dnsSec' - Reserved for future use.
-- * 'domainName' - The name of a domain.
-- * 'expirationDate' - The date when the registration for the domain is set to expire. The date and time is in Unix time format and Coordinated Universal time (UTC).
-- * 'nameservers' - The name of the domain.
-- * 'registrantContact' - Provides details about the domain registrant.
-- * 'registrantPrivacy' - Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
-- * 'registrarName' - Name of the registrar of the domain as identified in the registry. Domains with a .com, .net, or .org TLD are registered by Amazon Registrar. All other domains are registered by our registrar associate, Gandi. The value for domains that are registered by Gandi is @"GANDI SAS"@ .
-- * 'registrarURL' - Web address of the registrar.
-- * 'registryDomainId' - Reserved for future use.
-- * 'reseller' - Reseller of the domain. Domains registered or transferred using Route 53 domains will have @"Amazon"@ as the reseller.
-- * 'responseStatus' - The response status code.
-- * 'statusList' - An array of domain name status codes, also known as Extensible Provisioning Protocol (EPP) status codes.
--
-- ICANN, the organization that maintains a central database of domain names, has developed a set of domain name status codes that tell you the status of a variety of operations on a domain name, for example, registering a domain name, transferring a domain name to another registrar, renewing the registration for a domain name, and so on. All registrars use this same set of status codes.
-- For a current list of domain name status codes and an explanation of what each code means, go to the <https://www.icann.org/ ICANN website> and search for @epp status codes@ . (Search on the ICANN website; web searches sometimes return an old version of the document.)
-- * 'techContact' - Provides details about the domain technical contact.
-- * 'techPrivacy' - Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the technical contact.
-- * 'updatedDate' - The last updated date of the domain as found in the response to a WHOIS query. The date and time is in Unix time format and Coordinated Universal time (UTC).
-- * 'whoIsServer' - The fully qualified name of the WHOIS server that can answer the WHOIS query for the domain.
mkGetDomainDetailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'domainName'
  Lude.Text ->
  -- | 'adminContact'
  ContactDetail ->
  -- | 'registrantContact'
  ContactDetail ->
  -- | 'techContact'
  ContactDetail ->
  GetDomainDetailResponse
mkGetDomainDetailResponse
  pResponseStatus_
  pDomainName_
  pAdminContact_
  pRegistrantContact_
  pTechContact_ =
    GetDomainDetailResponse'
      { techPrivacy = Lude.Nothing,
        dnsSec = Lude.Nothing,
        whoIsServer = Lude.Nothing,
        registryDomainId = Lude.Nothing,
        registrantPrivacy = Lude.Nothing,
        updatedDate = Lude.Nothing,
        adminPrivacy = Lude.Nothing,
        autoRenew = Lude.Nothing,
        abuseContactPhone = Lude.Nothing,
        registrarURL = Lude.Nothing,
        abuseContactEmail = Lude.Nothing,
        expirationDate = Lude.Nothing,
        creationDate = Lude.Nothing,
        registrarName = Lude.Nothing,
        reseller = Lude.Nothing,
        statusList = Lude.Nothing,
        responseStatus = pResponseStatus_,
        domainName = pDomainName_,
        nameservers = Lude.mempty,
        adminContact = pAdminContact_,
        registrantContact = pRegistrantContact_,
        techContact = pTechContact_
      }

-- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the technical contact.
--
-- /Note:/ Consider using 'techPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsTechPrivacy :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Bool)
gddrsTechPrivacy = Lens.lens (techPrivacy :: GetDomainDetailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {techPrivacy = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsTechPrivacy "Use generic-lens or generic-optics with 'techPrivacy' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'dnsSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsDNSSec :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Text)
gddrsDNSSec = Lens.lens (dnsSec :: GetDomainDetailResponse -> Lude.Maybe Lude.Text) (\s a -> s {dnsSec = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsDNSSec "Use generic-lens or generic-optics with 'dnsSec' instead." #-}

-- | The fully qualified name of the WHOIS server that can answer the WHOIS query for the domain.
--
-- /Note:/ Consider using 'whoIsServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsWhoIsServer :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Text)
gddrsWhoIsServer = Lens.lens (whoIsServer :: GetDomainDetailResponse -> Lude.Maybe Lude.Text) (\s a -> s {whoIsServer = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsWhoIsServer "Use generic-lens or generic-optics with 'whoIsServer' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'registryDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsRegistryDomainId :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Text)
gddrsRegistryDomainId = Lens.lens (registryDomainId :: GetDomainDetailResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryDomainId = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsRegistryDomainId "Use generic-lens or generic-optics with 'registryDomainId' instead." #-}

-- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
--
-- /Note:/ Consider using 'registrantPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsRegistrantPrivacy :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Bool)
gddrsRegistrantPrivacy = Lens.lens (registrantPrivacy :: GetDomainDetailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {registrantPrivacy = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsRegistrantPrivacy "Use generic-lens or generic-optics with 'registrantPrivacy' instead." #-}

-- | The last updated date of the domain as found in the response to a WHOIS query. The date and time is in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'updatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsUpdatedDate :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Timestamp)
gddrsUpdatedDate = Lens.lens (updatedDate :: GetDomainDetailResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {updatedDate = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsUpdatedDate "Use generic-lens or generic-optics with 'updatedDate' instead." #-}

-- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the admin contact.
--
-- /Note:/ Consider using 'adminPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsAdminPrivacy :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Bool)
gddrsAdminPrivacy = Lens.lens (adminPrivacy :: GetDomainDetailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {adminPrivacy = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsAdminPrivacy "Use generic-lens or generic-optics with 'adminPrivacy' instead." #-}

-- | Specifies whether the domain registration is set to renew automatically.
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsAutoRenew :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Bool)
gddrsAutoRenew = Lens.lens (autoRenew :: GetDomainDetailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {autoRenew = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsAutoRenew "Use generic-lens or generic-optics with 'autoRenew' instead." #-}

-- | Phone number for reporting abuse.
--
-- /Note:/ Consider using 'abuseContactPhone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsAbuseContactPhone :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Text)
gddrsAbuseContactPhone = Lens.lens (abuseContactPhone :: GetDomainDetailResponse -> Lude.Maybe Lude.Text) (\s a -> s {abuseContactPhone = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsAbuseContactPhone "Use generic-lens or generic-optics with 'abuseContactPhone' instead." #-}

-- | Web address of the registrar.
--
-- /Note:/ Consider using 'registrarURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsRegistrarURL :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Text)
gddrsRegistrarURL = Lens.lens (registrarURL :: GetDomainDetailResponse -> Lude.Maybe Lude.Text) (\s a -> s {registrarURL = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsRegistrarURL "Use generic-lens or generic-optics with 'registrarURL' instead." #-}

-- | Email address to contact to report incorrect contact information for a domain, to report that the domain is being used to send spam, to report that someone is cybersquatting on a domain name, or report some other type of abuse.
--
-- /Note:/ Consider using 'abuseContactEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsAbuseContactEmail :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Text)
gddrsAbuseContactEmail = Lens.lens (abuseContactEmail :: GetDomainDetailResponse -> Lude.Maybe Lude.Text) (\s a -> s {abuseContactEmail = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsAbuseContactEmail "Use generic-lens or generic-optics with 'abuseContactEmail' instead." #-}

-- | The date when the registration for the domain is set to expire. The date and time is in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsExpirationDate :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Timestamp)
gddrsExpirationDate = Lens.lens (expirationDate :: GetDomainDetailResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationDate = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | The date when the domain was created as found in the response to a WHOIS query. The date and time is in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsCreationDate :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Timestamp)
gddrsCreationDate = Lens.lens (creationDate :: GetDomainDetailResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Name of the registrar of the domain as identified in the registry. Domains with a .com, .net, or .org TLD are registered by Amazon Registrar. All other domains are registered by our registrar associate, Gandi. The value for domains that are registered by Gandi is @"GANDI SAS"@ .
--
-- /Note:/ Consider using 'registrarName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsRegistrarName :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Text)
gddrsRegistrarName = Lens.lens (registrarName :: GetDomainDetailResponse -> Lude.Maybe Lude.Text) (\s a -> s {registrarName = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsRegistrarName "Use generic-lens or generic-optics with 'registrarName' instead." #-}

-- | Reseller of the domain. Domains registered or transferred using Route 53 domains will have @"Amazon"@ as the reseller.
--
-- /Note:/ Consider using 'reseller' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsReseller :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe Lude.Text)
gddrsReseller = Lens.lens (reseller :: GetDomainDetailResponse -> Lude.Maybe Lude.Text) (\s a -> s {reseller = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsReseller "Use generic-lens or generic-optics with 'reseller' instead." #-}

-- | An array of domain name status codes, also known as Extensible Provisioning Protocol (EPP) status codes.
--
-- ICANN, the organization that maintains a central database of domain names, has developed a set of domain name status codes that tell you the status of a variety of operations on a domain name, for example, registering a domain name, transferring a domain name to another registrar, renewing the registration for a domain name, and so on. All registrars use this same set of status codes.
-- For a current list of domain name status codes and an explanation of what each code means, go to the <https://www.icann.org/ ICANN website> and search for @epp status codes@ . (Search on the ICANN website; web searches sometimes return an old version of the document.)
--
-- /Note:/ Consider using 'statusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsStatusList :: Lens.Lens' GetDomainDetailResponse (Lude.Maybe [Lude.Text])
gddrsStatusList = Lens.lens (statusList :: GetDomainDetailResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {statusList = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsStatusList "Use generic-lens or generic-optics with 'statusList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsResponseStatus :: Lens.Lens' GetDomainDetailResponse Lude.Int
gddrsResponseStatus = Lens.lens (responseStatus :: GetDomainDetailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The name of a domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsDomainName :: Lens.Lens' GetDomainDetailResponse Lude.Text
gddrsDomainName = Lens.lens (domainName :: GetDomainDetailResponse -> Lude.Text) (\s a -> s {domainName = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the domain.
--
-- /Note:/ Consider using 'nameservers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsNameservers :: Lens.Lens' GetDomainDetailResponse [Nameserver]
gddrsNameservers = Lens.lens (nameservers :: GetDomainDetailResponse -> [Nameserver]) (\s a -> s {nameservers = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsNameservers "Use generic-lens or generic-optics with 'nameservers' instead." #-}

-- | Provides details about the domain administrative contact.
--
-- /Note:/ Consider using 'adminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsAdminContact :: Lens.Lens' GetDomainDetailResponse ContactDetail
gddrsAdminContact = Lens.lens (adminContact :: GetDomainDetailResponse -> ContactDetail) (\s a -> s {adminContact = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsAdminContact "Use generic-lens or generic-optics with 'adminContact' instead." #-}

-- | Provides details about the domain registrant.
--
-- /Note:/ Consider using 'registrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsRegistrantContact :: Lens.Lens' GetDomainDetailResponse ContactDetail
gddrsRegistrantContact = Lens.lens (registrantContact :: GetDomainDetailResponse -> ContactDetail) (\s a -> s {registrantContact = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsRegistrantContact "Use generic-lens or generic-optics with 'registrantContact' instead." #-}

-- | Provides details about the domain technical contact.
--
-- /Note:/ Consider using 'techContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrsTechContact :: Lens.Lens' GetDomainDetailResponse ContactDetail
gddrsTechContact = Lens.lens (techContact :: GetDomainDetailResponse -> ContactDetail) (\s a -> s {techContact = a} :: GetDomainDetailResponse)
{-# DEPRECATED gddrsTechContact "Use generic-lens or generic-optics with 'techContact' instead." #-}
