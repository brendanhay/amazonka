{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CheckDomainTransferability (..)
    , mkCheckDomainTransferability
    -- ** Request lenses
    , cdtDomainName
    , cdtAuthCode

    -- * Destructuring the response
    , CheckDomainTransferabilityResponse (..)
    , mkCheckDomainTransferabilityResponse
    -- ** Response lenses
    , cdtrrsTransferability
    , cdtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The CheckDomainTransferability request contains the following elements.
--
-- /See:/ 'mkCheckDomainTransferability' smart constructor.
data CheckDomainTransferability = CheckDomainTransferability'
  { domainName :: Types.DomainName
    -- ^ The name of the domain that you want to transfer to Route 53. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
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
  , authCode :: Core.Maybe Types.DomainAuthCode
    -- ^ If the registrar for the top-level domain (TLD) requires an authorization code to transfer the domain, the code that you got from the current registrar for the domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CheckDomainTransferability' value with any optional fields omitted.
mkCheckDomainTransferability
    :: Types.DomainName -- ^ 'domainName'
    -> CheckDomainTransferability
mkCheckDomainTransferability domainName
  = CheckDomainTransferability'{domainName, authCode = Core.Nothing}

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
{-# INLINEABLE cdtDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | If the registrar for the top-level domain (TLD) requires an authorization code to transfer the domain, the code that you got from the current registrar for the domain.
--
-- /Note:/ Consider using 'authCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtAuthCode :: Lens.Lens' CheckDomainTransferability (Core.Maybe Types.DomainAuthCode)
cdtAuthCode = Lens.field @"authCode"
{-# INLINEABLE cdtAuthCode #-}
{-# DEPRECATED authCode "Use generic-lens or generic-optics with 'authCode' instead"  #-}

instance Core.ToQuery CheckDomainTransferability where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CheckDomainTransferability where
        toHeaders CheckDomainTransferability{..}
          = Core.pure
              ("X-Amz-Target",
               "Route53Domains_v20140515.CheckDomainTransferability")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CheckDomainTransferability where
        toJSON CheckDomainTransferability{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainName" Core..= domainName),
                  ("AuthCode" Core..=) Core.<$> authCode])

instance Core.AWSRequest CheckDomainTransferability where
        type Rs CheckDomainTransferability =
             CheckDomainTransferabilityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CheckDomainTransferabilityResponse' Core.<$>
                   (x Core..: "Transferability") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The CheckDomainTransferability response includes the following elements.
--
-- /See:/ 'mkCheckDomainTransferabilityResponse' smart constructor.
data CheckDomainTransferabilityResponse = CheckDomainTransferabilityResponse'
  { transferability :: Types.DomainTransferability
    -- ^ A complex type that contains information about whether the specified domain can be transferred to Route 53.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CheckDomainTransferabilityResponse' value with any optional fields omitted.
mkCheckDomainTransferabilityResponse
    :: Types.DomainTransferability -- ^ 'transferability'
    -> Core.Int -- ^ 'responseStatus'
    -> CheckDomainTransferabilityResponse
mkCheckDomainTransferabilityResponse transferability responseStatus
  = CheckDomainTransferabilityResponse'{transferability,
                                        responseStatus}

-- | A complex type that contains information about whether the specified domain can be transferred to Route 53.
--
-- /Note:/ Consider using 'transferability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtrrsTransferability :: Lens.Lens' CheckDomainTransferabilityResponse Types.DomainTransferability
cdtrrsTransferability = Lens.field @"transferability"
{-# INLINEABLE cdtrrsTransferability #-}
{-# DEPRECATED transferability "Use generic-lens or generic-optics with 'transferability' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtrrsResponseStatus :: Lens.Lens' CheckDomainTransferabilityResponse Core.Int
cdtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
