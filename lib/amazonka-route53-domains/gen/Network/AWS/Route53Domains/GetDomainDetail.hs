{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gddrrsDomainName,
    gddrrsNameservers,
    gddrrsAdminContact,
    gddrrsRegistrantContact,
    gddrrsTechContact,
    gddrrsAbuseContactEmail,
    gddrrsAbuseContactPhone,
    gddrrsAdminPrivacy,
    gddrrsAutoRenew,
    gddrrsCreationDate,
    gddrrsDnsSec,
    gddrrsExpirationDate,
    gddrrsRegistrantPrivacy,
    gddrrsRegistrarName,
    gddrrsRegistrarUrl,
    gddrrsRegistryDomainId,
    gddrrsReseller,
    gddrrsStatusList,
    gddrrsTechPrivacy,
    gddrrsUpdatedDate,
    gddrrsWhoIsServer,
    gddrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The GetDomainDetail request includes the following element.
--
-- /See:/ 'mkGetDomainDetail' smart constructor.
newtype GetDomainDetail = GetDomainDetail'
  { -- | The name of the domain that you want to get detailed information about.
    domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDomainDetail' value with any optional fields omitted.
mkGetDomainDetail ::
  -- | 'domainName'
  Types.DomainName ->
  GetDomainDetail
mkGetDomainDetail domainName = GetDomainDetail' {domainName}

-- | The name of the domain that you want to get detailed information about.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddDomainName :: Lens.Lens' GetDomainDetail Types.DomainName
gddDomainName = Lens.field @"domainName"
{-# DEPRECATED gddDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.FromJSON GetDomainDetail where
  toJSON GetDomainDetail {..} =
    Core.object
      (Core.catMaybes [Core.Just ("DomainName" Core..= domainName)])

instance Core.AWSRequest GetDomainDetail where
  type Rs GetDomainDetail = GetDomainDetailResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53Domains_v20140515.GetDomainDetail")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainDetailResponse'
            Core.<$> (x Core..: "DomainName")
            Core.<*> (x Core..:? "Nameservers" Core..!= Core.mempty)
            Core.<*> (x Core..: "AdminContact")
            Core.<*> (x Core..: "RegistrantContact")
            Core.<*> (x Core..: "TechContact")
            Core.<*> (x Core..:? "AbuseContactEmail")
            Core.<*> (x Core..:? "AbuseContactPhone")
            Core.<*> (x Core..:? "AdminPrivacy")
            Core.<*> (x Core..:? "AutoRenew")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "DnsSec")
            Core.<*> (x Core..:? "ExpirationDate")
            Core.<*> (x Core..:? "RegistrantPrivacy")
            Core.<*> (x Core..:? "RegistrarName")
            Core.<*> (x Core..:? "RegistrarUrl")
            Core.<*> (x Core..:? "RegistryDomainId")
            Core.<*> (x Core..:? "Reseller")
            Core.<*> (x Core..:? "StatusList")
            Core.<*> (x Core..:? "TechPrivacy")
            Core.<*> (x Core..:? "UpdatedDate")
            Core.<*> (x Core..:? "WhoIsServer")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The GetDomainDetail response includes the following elements.
--
-- /See:/ 'mkGetDomainDetailResponse' smart constructor.
data GetDomainDetailResponse = GetDomainDetailResponse'
  { -- | The name of a domain.
    domainName :: Types.DomainName,
    -- | The name of the domain.
    nameservers :: [Types.Nameserver],
    -- | Provides details about the domain administrative contact.
    adminContact :: Types.ContactDetail,
    -- | Provides details about the domain registrant.
    registrantContact :: Types.ContactDetail,
    -- | Provides details about the domain technical contact.
    techContact :: Types.ContactDetail,
    -- | Email address to contact to report incorrect contact information for a domain, to report that the domain is being used to send spam, to report that someone is cybersquatting on a domain name, or report some other type of abuse.
    abuseContactEmail :: Core.Maybe Types.Email,
    -- | Phone number for reporting abuse.
    abuseContactPhone :: Core.Maybe Types.AbuseContactPhone,
    -- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the admin contact.
    adminPrivacy :: Core.Maybe Core.Bool,
    -- | Specifies whether the domain registration is set to renew automatically.
    autoRenew :: Core.Maybe Core.Bool,
    -- | The date when the domain was created as found in the response to a WHOIS query. The date and time is in Unix time format and Coordinated Universal time (UTC).
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | Reserved for future use.
    dnsSec :: Core.Maybe Types.DNSSec,
    -- | The date when the registration for the domain is set to expire. The date and time is in Unix time format and Coordinated Universal time (UTC).
    expirationDate :: Core.Maybe Core.NominalDiffTime,
    -- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
    registrantPrivacy :: Core.Maybe Core.Bool,
    -- | Name of the registrar of the domain as identified in the registry. Domains with a .com, .net, or .org TLD are registered by Amazon Registrar. All other domains are registered by our registrar associate, Gandi. The value for domains that are registered by Gandi is @"GANDI SAS"@ .
    registrarName :: Core.Maybe Types.RegistrarName,
    -- | Web address of the registrar.
    registrarUrl :: Core.Maybe Types.RegistrarUrl,
    -- | Reserved for future use.
    registryDomainId :: Core.Maybe Types.RegistryDomainId,
    -- | Reseller of the domain. Domains registered or transferred using Route 53 domains will have @"Amazon"@ as the reseller.
    reseller :: Core.Maybe Types.Reseller,
    -- | An array of domain name status codes, also known as Extensible Provisioning Protocol (EPP) status codes.
    --
    -- ICANN, the organization that maintains a central database of domain names, has developed a set of domain name status codes that tell you the status of a variety of operations on a domain name, for example, registering a domain name, transferring a domain name to another registrar, renewing the registration for a domain name, and so on. All registrars use this same set of status codes.
    -- For a current list of domain name status codes and an explanation of what each code means, go to the <https://www.icann.org/ ICANN website> and search for @epp status codes@ . (Search on the ICANN website; web searches sometimes return an old version of the document.)
    statusList :: Core.Maybe [Types.DomainStatus],
    -- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the technical contact.
    techPrivacy :: Core.Maybe Core.Bool,
    -- | The last updated date of the domain as found in the response to a WHOIS query. The date and time is in Unix time format and Coordinated Universal time (UTC).
    updatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The fully qualified name of the WHOIS server that can answer the WHOIS query for the domain.
    whoIsServer :: Core.Maybe Types.WhoIsServer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDomainDetailResponse' value with any optional fields omitted.
mkGetDomainDetailResponse ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'adminContact'
  Types.ContactDetail ->
  -- | 'registrantContact'
  Types.ContactDetail ->
  -- | 'techContact'
  Types.ContactDetail ->
  -- | 'responseStatus'
  Core.Int ->
  GetDomainDetailResponse
mkGetDomainDetailResponse
  domainName
  adminContact
  registrantContact
  techContact
  responseStatus =
    GetDomainDetailResponse'
      { domainName,
        nameservers = Core.mempty,
        adminContact,
        registrantContact,
        techContact,
        abuseContactEmail = Core.Nothing,
        abuseContactPhone = Core.Nothing,
        adminPrivacy = Core.Nothing,
        autoRenew = Core.Nothing,
        creationDate = Core.Nothing,
        dnsSec = Core.Nothing,
        expirationDate = Core.Nothing,
        registrantPrivacy = Core.Nothing,
        registrarName = Core.Nothing,
        registrarUrl = Core.Nothing,
        registryDomainId = Core.Nothing,
        reseller = Core.Nothing,
        statusList = Core.Nothing,
        techPrivacy = Core.Nothing,
        updatedDate = Core.Nothing,
        whoIsServer = Core.Nothing,
        responseStatus
      }

-- | The name of a domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsDomainName :: Lens.Lens' GetDomainDetailResponse Types.DomainName
gddrrsDomainName = Lens.field @"domainName"
{-# DEPRECATED gddrrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the domain.
--
-- /Note:/ Consider using 'nameservers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsNameservers :: Lens.Lens' GetDomainDetailResponse [Types.Nameserver]
gddrrsNameservers = Lens.field @"nameservers"
{-# DEPRECATED gddrrsNameservers "Use generic-lens or generic-optics with 'nameservers' instead." #-}

-- | Provides details about the domain administrative contact.
--
-- /Note:/ Consider using 'adminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsAdminContact :: Lens.Lens' GetDomainDetailResponse Types.ContactDetail
gddrrsAdminContact = Lens.field @"adminContact"
{-# DEPRECATED gddrrsAdminContact "Use generic-lens or generic-optics with 'adminContact' instead." #-}

-- | Provides details about the domain registrant.
--
-- /Note:/ Consider using 'registrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsRegistrantContact :: Lens.Lens' GetDomainDetailResponse Types.ContactDetail
gddrrsRegistrantContact = Lens.field @"registrantContact"
{-# DEPRECATED gddrrsRegistrantContact "Use generic-lens or generic-optics with 'registrantContact' instead." #-}

-- | Provides details about the domain technical contact.
--
-- /Note:/ Consider using 'techContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsTechContact :: Lens.Lens' GetDomainDetailResponse Types.ContactDetail
gddrrsTechContact = Lens.field @"techContact"
{-# DEPRECATED gddrrsTechContact "Use generic-lens or generic-optics with 'techContact' instead." #-}

-- | Email address to contact to report incorrect contact information for a domain, to report that the domain is being used to send spam, to report that someone is cybersquatting on a domain name, or report some other type of abuse.
--
-- /Note:/ Consider using 'abuseContactEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsAbuseContactEmail :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.Email)
gddrrsAbuseContactEmail = Lens.field @"abuseContactEmail"
{-# DEPRECATED gddrrsAbuseContactEmail "Use generic-lens or generic-optics with 'abuseContactEmail' instead." #-}

-- | Phone number for reporting abuse.
--
-- /Note:/ Consider using 'abuseContactPhone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsAbuseContactPhone :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.AbuseContactPhone)
gddrrsAbuseContactPhone = Lens.field @"abuseContactPhone"
{-# DEPRECATED gddrrsAbuseContactPhone "Use generic-lens or generic-optics with 'abuseContactPhone' instead." #-}

-- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the admin contact.
--
-- /Note:/ Consider using 'adminPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsAdminPrivacy :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.Bool)
gddrrsAdminPrivacy = Lens.field @"adminPrivacy"
{-# DEPRECATED gddrrsAdminPrivacy "Use generic-lens or generic-optics with 'adminPrivacy' instead." #-}

-- | Specifies whether the domain registration is set to renew automatically.
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsAutoRenew :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.Bool)
gddrrsAutoRenew = Lens.field @"autoRenew"
{-# DEPRECATED gddrrsAutoRenew "Use generic-lens or generic-optics with 'autoRenew' instead." #-}

-- | The date when the domain was created as found in the response to a WHOIS query. The date and time is in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsCreationDate :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.NominalDiffTime)
gddrrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED gddrrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'dnsSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsDnsSec :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.DNSSec)
gddrrsDnsSec = Lens.field @"dnsSec"
{-# DEPRECATED gddrrsDnsSec "Use generic-lens or generic-optics with 'dnsSec' instead." #-}

-- | The date when the registration for the domain is set to expire. The date and time is in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsExpirationDate :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.NominalDiffTime)
gddrrsExpirationDate = Lens.field @"expirationDate"
{-# DEPRECATED gddrrsExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
--
-- /Note:/ Consider using 'registrantPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsRegistrantPrivacy :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.Bool)
gddrrsRegistrantPrivacy = Lens.field @"registrantPrivacy"
{-# DEPRECATED gddrrsRegistrantPrivacy "Use generic-lens or generic-optics with 'registrantPrivacy' instead." #-}

-- | Name of the registrar of the domain as identified in the registry. Domains with a .com, .net, or .org TLD are registered by Amazon Registrar. All other domains are registered by our registrar associate, Gandi. The value for domains that are registered by Gandi is @"GANDI SAS"@ .
--
-- /Note:/ Consider using 'registrarName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsRegistrarName :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.RegistrarName)
gddrrsRegistrarName = Lens.field @"registrarName"
{-# DEPRECATED gddrrsRegistrarName "Use generic-lens or generic-optics with 'registrarName' instead." #-}

-- | Web address of the registrar.
--
-- /Note:/ Consider using 'registrarUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsRegistrarUrl :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.RegistrarUrl)
gddrrsRegistrarUrl = Lens.field @"registrarUrl"
{-# DEPRECATED gddrrsRegistrarUrl "Use generic-lens or generic-optics with 'registrarUrl' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'registryDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsRegistryDomainId :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.RegistryDomainId)
gddrrsRegistryDomainId = Lens.field @"registryDomainId"
{-# DEPRECATED gddrrsRegistryDomainId "Use generic-lens or generic-optics with 'registryDomainId' instead." #-}

-- | Reseller of the domain. Domains registered or transferred using Route 53 domains will have @"Amazon"@ as the reseller.
--
-- /Note:/ Consider using 'reseller' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsReseller :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.Reseller)
gddrrsReseller = Lens.field @"reseller"
{-# DEPRECATED gddrrsReseller "Use generic-lens or generic-optics with 'reseller' instead." #-}

-- | An array of domain name status codes, also known as Extensible Provisioning Protocol (EPP) status codes.
--
-- ICANN, the organization that maintains a central database of domain names, has developed a set of domain name status codes that tell you the status of a variety of operations on a domain name, for example, registering a domain name, transferring a domain name to another registrar, renewing the registration for a domain name, and so on. All registrars use this same set of status codes.
-- For a current list of domain name status codes and an explanation of what each code means, go to the <https://www.icann.org/ ICANN website> and search for @epp status codes@ . (Search on the ICANN website; web searches sometimes return an old version of the document.)
--
-- /Note:/ Consider using 'statusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsStatusList :: Lens.Lens' GetDomainDetailResponse (Core.Maybe [Types.DomainStatus])
gddrrsStatusList = Lens.field @"statusList"
{-# DEPRECATED gddrrsStatusList "Use generic-lens or generic-optics with 'statusList' instead." #-}

-- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the technical contact.
--
-- /Note:/ Consider using 'techPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsTechPrivacy :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.Bool)
gddrrsTechPrivacy = Lens.field @"techPrivacy"
{-# DEPRECATED gddrrsTechPrivacy "Use generic-lens or generic-optics with 'techPrivacy' instead." #-}

-- | The last updated date of the domain as found in the response to a WHOIS query. The date and time is in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'updatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsUpdatedDate :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.NominalDiffTime)
gddrrsUpdatedDate = Lens.field @"updatedDate"
{-# DEPRECATED gddrrsUpdatedDate "Use generic-lens or generic-optics with 'updatedDate' instead." #-}

-- | The fully qualified name of the WHOIS server that can answer the WHOIS query for the domain.
--
-- /Note:/ Consider using 'whoIsServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsWhoIsServer :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.WhoIsServer)
gddrrsWhoIsServer = Lens.field @"whoIsServer"
{-# DEPRECATED gddrrsWhoIsServer "Use generic-lens or generic-optics with 'whoIsServer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsResponseStatus :: Lens.Lens' GetDomainDetailResponse Core.Int
gddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
