{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.UpdateDomainContactPrivacy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the specified domain contact's privacy setting. When privacy protection is enabled, contact information such as email address is replaced either with contact information for Amazon Registrar (for .com, .net, and .org domains) or with contact information for our registrar associate, Gandi.
--
-- This operation affects only the contact information for the specified contact type (registrant, administrator, or tech). If the request succeeds, Amazon Route 53 returns an operation ID that you can use with <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to track the progress and completion of the action. If the request doesn't complete successfully, the domain registrant will be notified by email.
-- /Important:/ By disabling the privacy service via API, you consent to the publication of the contact information provided for this domain via the public WHOIS database. You certify that you are the registrant of this domain name and have the authority to make this decision. You may withdraw your consent at any time by enabling privacy protection using either @UpdateDomainContactPrivacy@ or the Route 53 console. Enabling privacy protection removes the contact information provided for this domain from the WHOIS database. For more information on our privacy practices, see <https://aws.amazon.com/privacy/ https://aws.amazon.com/privacy/> .
module Network.AWS.Route53Domains.UpdateDomainContactPrivacy
    (
    -- * Creating a request
      UpdateDomainContactPrivacy (..)
    , mkUpdateDomainContactPrivacy
    -- ** Request lenses
    , udcpDomainName
    , udcpAdminPrivacy
    , udcpRegistrantPrivacy
    , udcpTechPrivacy

    -- * Destructuring the response
    , UpdateDomainContactPrivacyResponse (..)
    , mkUpdateDomainContactPrivacyResponse
    -- ** Response lenses
    , udcprrsOperationId
    , udcprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The UpdateDomainContactPrivacy request includes the following elements.
--
-- /See:/ 'mkUpdateDomainContactPrivacy' smart constructor.
data UpdateDomainContactPrivacy = UpdateDomainContactPrivacy'
  { domainName :: Types.DomainName
    -- ^ The name of the domain that you want to update the privacy setting for.
  , adminPrivacy :: Core.Maybe Core.Bool
    -- ^ Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
  , registrantPrivacy :: Core.Maybe Core.Bool
    -- ^ Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
  , techPrivacy :: Core.Maybe Core.Bool
    -- ^ Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainContactPrivacy' value with any optional fields omitted.
mkUpdateDomainContactPrivacy
    :: Types.DomainName -- ^ 'domainName'
    -> UpdateDomainContactPrivacy
mkUpdateDomainContactPrivacy domainName
  = UpdateDomainContactPrivacy'{domainName,
                                adminPrivacy = Core.Nothing, registrantPrivacy = Core.Nothing,
                                techPrivacy = Core.Nothing}

-- | The name of the domain that you want to update the privacy setting for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcpDomainName :: Lens.Lens' UpdateDomainContactPrivacy Types.DomainName
udcpDomainName = Lens.field @"domainName"
{-# INLINEABLE udcpDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
--
-- /Note:/ Consider using 'adminPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcpAdminPrivacy :: Lens.Lens' UpdateDomainContactPrivacy (Core.Maybe Core.Bool)
udcpAdminPrivacy = Lens.field @"adminPrivacy"
{-# INLINEABLE udcpAdminPrivacy #-}
{-# DEPRECATED adminPrivacy "Use generic-lens or generic-optics with 'adminPrivacy' instead"  #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
--
-- /Note:/ Consider using 'registrantPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcpRegistrantPrivacy :: Lens.Lens' UpdateDomainContactPrivacy (Core.Maybe Core.Bool)
udcpRegistrantPrivacy = Lens.field @"registrantPrivacy"
{-# INLINEABLE udcpRegistrantPrivacy #-}
{-# DEPRECATED registrantPrivacy "Use generic-lens or generic-optics with 'registrantPrivacy' instead"  #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact.
--
-- /Note:/ Consider using 'techPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcpTechPrivacy :: Lens.Lens' UpdateDomainContactPrivacy (Core.Maybe Core.Bool)
udcpTechPrivacy = Lens.field @"techPrivacy"
{-# INLINEABLE udcpTechPrivacy #-}
{-# DEPRECATED techPrivacy "Use generic-lens or generic-optics with 'techPrivacy' instead"  #-}

instance Core.ToQuery UpdateDomainContactPrivacy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDomainContactPrivacy where
        toHeaders UpdateDomainContactPrivacy{..}
          = Core.pure
              ("X-Amz-Target",
               "Route53Domains_v20140515.UpdateDomainContactPrivacy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDomainContactPrivacy where
        toJSON UpdateDomainContactPrivacy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainName" Core..= domainName),
                  ("AdminPrivacy" Core..=) Core.<$> adminPrivacy,
                  ("RegistrantPrivacy" Core..=) Core.<$> registrantPrivacy,
                  ("TechPrivacy" Core..=) Core.<$> techPrivacy])

instance Core.AWSRequest UpdateDomainContactPrivacy where
        type Rs UpdateDomainContactPrivacy =
             UpdateDomainContactPrivacyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateDomainContactPrivacyResponse' Core.<$>
                   (x Core..: "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The UpdateDomainContactPrivacy response includes the following element.
--
-- /See:/ 'mkUpdateDomainContactPrivacyResponse' smart constructor.
data UpdateDomainContactPrivacyResponse = UpdateDomainContactPrivacyResponse'
  { operationId :: Types.OperationId
    -- ^ Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainContactPrivacyResponse' value with any optional fields omitted.
mkUpdateDomainContactPrivacyResponse
    :: Types.OperationId -- ^ 'operationId'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateDomainContactPrivacyResponse
mkUpdateDomainContactPrivacyResponse operationId responseStatus
  = UpdateDomainContactPrivacyResponse'{operationId, responseStatus}

-- | Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcprrsOperationId :: Lens.Lens' UpdateDomainContactPrivacyResponse Types.OperationId
udcprrsOperationId = Lens.field @"operationId"
{-# INLINEABLE udcprrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcprrsResponseStatus :: Lens.Lens' UpdateDomainContactPrivacyResponse Core.Int
udcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
