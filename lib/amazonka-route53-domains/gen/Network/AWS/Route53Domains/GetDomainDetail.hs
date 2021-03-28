{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetDomainDetail (..)
    , mkGetDomainDetail
    -- ** Request lenses
    , gddDomainName

    -- * Destructuring the response
    , GetDomainDetailResponse (..)
    , mkGetDomainDetailResponse
    -- ** Response lenses
    , gddrrsDomainName
    , gddrrsNameservers
    , gddrrsAdminContact
    , gddrrsRegistrantContact
    , gddrrsTechContact
    , gddrrsAbuseContactEmail
    , gddrrsAbuseContactPhone
    , gddrrsAdminPrivacy
    , gddrrsAutoRenew
    , gddrrsCreationDate
    , gddrrsDnsSec
    , gddrrsExpirationDate
    , gddrrsRegistrantPrivacy
    , gddrrsRegistrarName
    , gddrrsRegistrarUrl
    , gddrrsRegistryDomainId
    , gddrrsReseller
    , gddrrsStatusList
    , gddrrsTechPrivacy
    , gddrrsUpdatedDate
    , gddrrsWhoIsServer
    , gddrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The GetDomainDetail request includes the following element.
--
-- /See:/ 'mkGetDomainDetail' smart constructor.
newtype GetDomainDetail = GetDomainDetail'
  { domainName :: Types.DomainName
    -- ^ The name of the domain that you want to get detailed information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDomainDetail' value with any optional fields omitted.
mkGetDomainDetail
    :: Types.DomainName -- ^ 'domainName'
    -> GetDomainDetail
mkGetDomainDetail domainName = GetDomainDetail'{domainName}

-- | The name of the domain that you want to get detailed information about.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddDomainName :: Lens.Lens' GetDomainDetail Types.DomainName
gddDomainName = Lens.field @"domainName"
{-# INLINEABLE gddDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery GetDomainDetail where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDomainDetail where
        toHeaders GetDomainDetail{..}
          = Core.pure
              ("X-Amz-Target", "Route53Domains_v20140515.GetDomainDetail")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDomainDetail where
        toJSON GetDomainDetail{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DomainName" Core..= domainName)])

instance Core.AWSRequest GetDomainDetail where
        type Rs GetDomainDetail = GetDomainDetailResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDomainDetailResponse' Core.<$>
                   (x Core..: "DomainName") Core.<*>
                     x Core..:? "Nameservers" Core..!= Core.mempty
                     Core.<*> x Core..: "AdminContact"
                     Core.<*> x Core..: "RegistrantContact"
                     Core.<*> x Core..: "TechContact"
                     Core.<*> x Core..:? "AbuseContactEmail"
                     Core.<*> x Core..:? "AbuseContactPhone"
                     Core.<*> x Core..:? "AdminPrivacy"
                     Core.<*> x Core..:? "AutoRenew"
                     Core.<*> x Core..:? "CreationDate"
                     Core.<*> x Core..:? "DnsSec"
                     Core.<*> x Core..:? "ExpirationDate"
                     Core.<*> x Core..:? "RegistrantPrivacy"
                     Core.<*> x Core..:? "RegistrarName"
                     Core.<*> x Core..:? "RegistrarUrl"
                     Core.<*> x Core..:? "RegistryDomainId"
                     Core.<*> x Core..:? "Reseller"
                     Core.<*> x Core..:? "StatusList"
                     Core.<*> x Core..:? "TechPrivacy"
                     Core.<*> x Core..:? "UpdatedDate"
                     Core.<*> x Core..:? "WhoIsServer"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The GetDomainDetail response includes the following elements.
--
-- /See:/ 'mkGetDomainDetailResponse' smart constructor.
data GetDomainDetailResponse = GetDomainDetailResponse'
  { domainName :: Types.DomainName
    -- ^ The name of a domain.
  , nameservers :: [Types.Nameserver]
    -- ^ The name of the domain.
  , adminContact :: Types.ContactDetail
    -- ^ Provides details about the domain administrative contact.
  , registrantContact :: Types.ContactDetail
    -- ^ Provides details about the domain registrant.
  , techContact :: Types.ContactDetail
    -- ^ Provides details about the domain technical contact.
  , abuseContactEmail :: Core.Maybe Types.Email
    -- ^ Email address to contact to report incorrect contact information for a domain, to report that the domain is being used to send spam, to report that someone is cybersquatting on a domain name, or report some other type of abuse.
  , abuseContactPhone :: Core.Maybe Types.AbuseContactPhone
    -- ^ Phone number for reporting abuse.
  , adminPrivacy :: Core.Maybe Core.Bool
    -- ^ Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the admin contact.
  , autoRenew :: Core.Maybe Core.Bool
    -- ^ Specifies whether the domain registration is set to renew automatically.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the domain was created as found in the response to a WHOIS query. The date and time is in Unix time format and Coordinated Universal time (UTC).
  , dnsSec :: Core.Maybe Types.DNSSec
    -- ^ Reserved for future use.
  , expirationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the registration for the domain is set to expire. The date and time is in Unix time format and Coordinated Universal time (UTC).
  , registrantPrivacy :: Core.Maybe Core.Bool
    -- ^ Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
  , registrarName :: Core.Maybe Types.RegistrarName
    -- ^ Name of the registrar of the domain as identified in the registry. Domains with a .com, .net, or .org TLD are registered by Amazon Registrar. All other domains are registered by our registrar associate, Gandi. The value for domains that are registered by Gandi is @"GANDI SAS"@ . 
  , registrarUrl :: Core.Maybe Types.RegistrarUrl
    -- ^ Web address of the registrar.
  , registryDomainId :: Core.Maybe Types.RegistryDomainId
    -- ^ Reserved for future use.
  , reseller :: Core.Maybe Types.Reseller
    -- ^ Reseller of the domain. Domains registered or transferred using Route 53 domains will have @"Amazon"@ as the reseller. 
  , statusList :: Core.Maybe [Types.DomainStatus]
    -- ^ An array of domain name status codes, also known as Extensible Provisioning Protocol (EPP) status codes.
--
-- ICANN, the organization that maintains a central database of domain names, has developed a set of domain name status codes that tell you the status of a variety of operations on a domain name, for example, registering a domain name, transferring a domain name to another registrar, renewing the registration for a domain name, and so on. All registrars use this same set of status codes.
-- For a current list of domain name status codes and an explanation of what each code means, go to the <https://www.icann.org/ ICANN website> and search for @epp status codes@ . (Search on the ICANN website; web searches sometimes return an old version of the document.)
  , techPrivacy :: Core.Maybe Core.Bool
    -- ^ Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the technical contact.
  , updatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The last updated date of the domain as found in the response to a WHOIS query. The date and time is in Unix time format and Coordinated Universal time (UTC).
  , whoIsServer :: Core.Maybe Types.WhoIsServer
    -- ^ The fully qualified name of the WHOIS server that can answer the WHOIS query for the domain.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDomainDetailResponse' value with any optional fields omitted.
mkGetDomainDetailResponse
    :: Types.DomainName -- ^ 'domainName'
    -> Types.ContactDetail -- ^ 'adminContact'
    -> Types.ContactDetail -- ^ 'registrantContact'
    -> Types.ContactDetail -- ^ 'techContact'
    -> Core.Int -- ^ 'responseStatus'
    -> GetDomainDetailResponse
mkGetDomainDetailResponse domainName adminContact registrantContact
  techContact responseStatus
  = GetDomainDetailResponse'{domainName, nameservers = Core.mempty,
                             adminContact, registrantContact, techContact,
                             abuseContactEmail = Core.Nothing, abuseContactPhone = Core.Nothing,
                             adminPrivacy = Core.Nothing, autoRenew = Core.Nothing,
                             creationDate = Core.Nothing, dnsSec = Core.Nothing,
                             expirationDate = Core.Nothing, registrantPrivacy = Core.Nothing,
                             registrarName = Core.Nothing, registrarUrl = Core.Nothing,
                             registryDomainId = Core.Nothing, reseller = Core.Nothing,
                             statusList = Core.Nothing, techPrivacy = Core.Nothing,
                             updatedDate = Core.Nothing, whoIsServer = Core.Nothing,
                             responseStatus}

-- | The name of a domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsDomainName :: Lens.Lens' GetDomainDetailResponse Types.DomainName
gddrrsDomainName = Lens.field @"domainName"
{-# INLINEABLE gddrrsDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The name of the domain.
--
-- /Note:/ Consider using 'nameservers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsNameservers :: Lens.Lens' GetDomainDetailResponse [Types.Nameserver]
gddrrsNameservers = Lens.field @"nameservers"
{-# INLINEABLE gddrrsNameservers #-}
{-# DEPRECATED nameservers "Use generic-lens or generic-optics with 'nameservers' instead"  #-}

-- | Provides details about the domain administrative contact.
--
-- /Note:/ Consider using 'adminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsAdminContact :: Lens.Lens' GetDomainDetailResponse Types.ContactDetail
gddrrsAdminContact = Lens.field @"adminContact"
{-# INLINEABLE gddrrsAdminContact #-}
{-# DEPRECATED adminContact "Use generic-lens or generic-optics with 'adminContact' instead"  #-}

-- | Provides details about the domain registrant.
--
-- /Note:/ Consider using 'registrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsRegistrantContact :: Lens.Lens' GetDomainDetailResponse Types.ContactDetail
gddrrsRegistrantContact = Lens.field @"registrantContact"
{-# INLINEABLE gddrrsRegistrantContact #-}
{-# DEPRECATED registrantContact "Use generic-lens or generic-optics with 'registrantContact' instead"  #-}

-- | Provides details about the domain technical contact.
--
-- /Note:/ Consider using 'techContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsTechContact :: Lens.Lens' GetDomainDetailResponse Types.ContactDetail
gddrrsTechContact = Lens.field @"techContact"
{-# INLINEABLE gddrrsTechContact #-}
{-# DEPRECATED techContact "Use generic-lens or generic-optics with 'techContact' instead"  #-}

-- | Email address to contact to report incorrect contact information for a domain, to report that the domain is being used to send spam, to report that someone is cybersquatting on a domain name, or report some other type of abuse.
--
-- /Note:/ Consider using 'abuseContactEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsAbuseContactEmail :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.Email)
gddrrsAbuseContactEmail = Lens.field @"abuseContactEmail"
{-# INLINEABLE gddrrsAbuseContactEmail #-}
{-# DEPRECATED abuseContactEmail "Use generic-lens or generic-optics with 'abuseContactEmail' instead"  #-}

-- | Phone number for reporting abuse.
--
-- /Note:/ Consider using 'abuseContactPhone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsAbuseContactPhone :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.AbuseContactPhone)
gddrrsAbuseContactPhone = Lens.field @"abuseContactPhone"
{-# INLINEABLE gddrrsAbuseContactPhone #-}
{-# DEPRECATED abuseContactPhone "Use generic-lens or generic-optics with 'abuseContactPhone' instead"  #-}

-- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the admin contact.
--
-- /Note:/ Consider using 'adminPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsAdminPrivacy :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.Bool)
gddrrsAdminPrivacy = Lens.field @"adminPrivacy"
{-# INLINEABLE gddrrsAdminPrivacy #-}
{-# DEPRECATED adminPrivacy "Use generic-lens or generic-optics with 'adminPrivacy' instead"  #-}

-- | Specifies whether the domain registration is set to renew automatically.
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsAutoRenew :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.Bool)
gddrrsAutoRenew = Lens.field @"autoRenew"
{-# INLINEABLE gddrrsAutoRenew #-}
{-# DEPRECATED autoRenew "Use generic-lens or generic-optics with 'autoRenew' instead"  #-}

-- | The date when the domain was created as found in the response to a WHOIS query. The date and time is in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsCreationDate :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.NominalDiffTime)
gddrrsCreationDate = Lens.field @"creationDate"
{-# INLINEABLE gddrrsCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'dnsSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsDnsSec :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.DNSSec)
gddrrsDnsSec = Lens.field @"dnsSec"
{-# INLINEABLE gddrrsDnsSec #-}
{-# DEPRECATED dnsSec "Use generic-lens or generic-optics with 'dnsSec' instead"  #-}

-- | The date when the registration for the domain is set to expire. The date and time is in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsExpirationDate :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.NominalDiffTime)
gddrrsExpirationDate = Lens.field @"expirationDate"
{-# INLINEABLE gddrrsExpirationDate #-}
{-# DEPRECATED expirationDate "Use generic-lens or generic-optics with 'expirationDate' instead"  #-}

-- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
--
-- /Note:/ Consider using 'registrantPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsRegistrantPrivacy :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.Bool)
gddrrsRegistrantPrivacy = Lens.field @"registrantPrivacy"
{-# INLINEABLE gddrrsRegistrantPrivacy #-}
{-# DEPRECATED registrantPrivacy "Use generic-lens or generic-optics with 'registrantPrivacy' instead"  #-}

-- | Name of the registrar of the domain as identified in the registry. Domains with a .com, .net, or .org TLD are registered by Amazon Registrar. All other domains are registered by our registrar associate, Gandi. The value for domains that are registered by Gandi is @"GANDI SAS"@ . 
--
-- /Note:/ Consider using 'registrarName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsRegistrarName :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.RegistrarName)
gddrrsRegistrarName = Lens.field @"registrarName"
{-# INLINEABLE gddrrsRegistrarName #-}
{-# DEPRECATED registrarName "Use generic-lens or generic-optics with 'registrarName' instead"  #-}

-- | Web address of the registrar.
--
-- /Note:/ Consider using 'registrarUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsRegistrarUrl :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.RegistrarUrl)
gddrrsRegistrarUrl = Lens.field @"registrarUrl"
{-# INLINEABLE gddrrsRegistrarUrl #-}
{-# DEPRECATED registrarUrl "Use generic-lens or generic-optics with 'registrarUrl' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'registryDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsRegistryDomainId :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.RegistryDomainId)
gddrrsRegistryDomainId = Lens.field @"registryDomainId"
{-# INLINEABLE gddrrsRegistryDomainId #-}
{-# DEPRECATED registryDomainId "Use generic-lens or generic-optics with 'registryDomainId' instead"  #-}

-- | Reseller of the domain. Domains registered or transferred using Route 53 domains will have @"Amazon"@ as the reseller. 
--
-- /Note:/ Consider using 'reseller' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsReseller :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.Reseller)
gddrrsReseller = Lens.field @"reseller"
{-# INLINEABLE gddrrsReseller #-}
{-# DEPRECATED reseller "Use generic-lens or generic-optics with 'reseller' instead"  #-}

-- | An array of domain name status codes, also known as Extensible Provisioning Protocol (EPP) status codes.
--
-- ICANN, the organization that maintains a central database of domain names, has developed a set of domain name status codes that tell you the status of a variety of operations on a domain name, for example, registering a domain name, transferring a domain name to another registrar, renewing the registration for a domain name, and so on. All registrars use this same set of status codes.
-- For a current list of domain name status codes and an explanation of what each code means, go to the <https://www.icann.org/ ICANN website> and search for @epp status codes@ . (Search on the ICANN website; web searches sometimes return an old version of the document.)
--
-- /Note:/ Consider using 'statusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsStatusList :: Lens.Lens' GetDomainDetailResponse (Core.Maybe [Types.DomainStatus])
gddrrsStatusList = Lens.field @"statusList"
{-# INLINEABLE gddrrsStatusList #-}
{-# DEPRECATED statusList "Use generic-lens or generic-optics with 'statusList' instead"  #-}

-- | Specifies whether contact information is concealed from WHOIS queries. If the value is @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If the value is @false@ , WHOIS queries return the information that you entered for the technical contact.
--
-- /Note:/ Consider using 'techPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsTechPrivacy :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.Bool)
gddrrsTechPrivacy = Lens.field @"techPrivacy"
{-# INLINEABLE gddrrsTechPrivacy #-}
{-# DEPRECATED techPrivacy "Use generic-lens or generic-optics with 'techPrivacy' instead"  #-}

-- | The last updated date of the domain as found in the response to a WHOIS query. The date and time is in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'updatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsUpdatedDate :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Core.NominalDiffTime)
gddrrsUpdatedDate = Lens.field @"updatedDate"
{-# INLINEABLE gddrrsUpdatedDate #-}
{-# DEPRECATED updatedDate "Use generic-lens or generic-optics with 'updatedDate' instead"  #-}

-- | The fully qualified name of the WHOIS server that can answer the WHOIS query for the domain.
--
-- /Note:/ Consider using 'whoIsServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsWhoIsServer :: Lens.Lens' GetDomainDetailResponse (Core.Maybe Types.WhoIsServer)
gddrrsWhoIsServer = Lens.field @"whoIsServer"
{-# INLINEABLE gddrrsWhoIsServer #-}
{-# DEPRECATED whoIsServer "Use generic-lens or generic-optics with 'whoIsServer' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gddrrsResponseStatus :: Lens.Lens' GetDomainDetailResponse Core.Int
gddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
