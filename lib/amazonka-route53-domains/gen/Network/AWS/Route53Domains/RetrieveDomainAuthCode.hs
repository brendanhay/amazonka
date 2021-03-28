{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.RetrieveDomainAuthCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the AuthCode for the domain. To transfer a domain to another registrar, you provide this value to the new registrar.
module Network.AWS.Route53Domains.RetrieveDomainAuthCode
    (
    -- * Creating a request
      RetrieveDomainAuthCode (..)
    , mkRetrieveDomainAuthCode
    -- ** Request lenses
    , rdacDomainName

    -- * Destructuring the response
    , RetrieveDomainAuthCodeResponse (..)
    , mkRetrieveDomainAuthCodeResponse
    -- ** Response lenses
    , rdacrrsAuthCode
    , rdacrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | A request for the authorization code for the specified domain. To transfer a domain to another registrar, you provide this value to the new registrar.
--
-- /See:/ 'mkRetrieveDomainAuthCode' smart constructor.
newtype RetrieveDomainAuthCode = RetrieveDomainAuthCode'
  { domainName :: Types.DomainName
    -- ^ The name of the domain that you want to get an authorization code for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RetrieveDomainAuthCode' value with any optional fields omitted.
mkRetrieveDomainAuthCode
    :: Types.DomainName -- ^ 'domainName'
    -> RetrieveDomainAuthCode
mkRetrieveDomainAuthCode domainName
  = RetrieveDomainAuthCode'{domainName}

-- | The name of the domain that you want to get an authorization code for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdacDomainName :: Lens.Lens' RetrieveDomainAuthCode Types.DomainName
rdacDomainName = Lens.field @"domainName"
{-# INLINEABLE rdacDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery RetrieveDomainAuthCode where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RetrieveDomainAuthCode where
        toHeaders RetrieveDomainAuthCode{..}
          = Core.pure
              ("X-Amz-Target", "Route53Domains_v20140515.RetrieveDomainAuthCode")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RetrieveDomainAuthCode where
        toJSON RetrieveDomainAuthCode{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DomainName" Core..= domainName)])

instance Core.AWSRequest RetrieveDomainAuthCode where
        type Rs RetrieveDomainAuthCode = RetrieveDomainAuthCodeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RetrieveDomainAuthCodeResponse' Core.<$>
                   (x Core..: "AuthCode") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The RetrieveDomainAuthCode response includes the following element.
--
-- /See:/ 'mkRetrieveDomainAuthCodeResponse' smart constructor.
data RetrieveDomainAuthCodeResponse = RetrieveDomainAuthCodeResponse'
  { authCode :: Types.DomainAuthCode
    -- ^ The authorization code for the domain.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetrieveDomainAuthCodeResponse' value with any optional fields omitted.
mkRetrieveDomainAuthCodeResponse
    :: Types.DomainAuthCode -- ^ 'authCode'
    -> Core.Int -- ^ 'responseStatus'
    -> RetrieveDomainAuthCodeResponse
mkRetrieveDomainAuthCodeResponse authCode responseStatus
  = RetrieveDomainAuthCodeResponse'{authCode, responseStatus}

-- | The authorization code for the domain.
--
-- /Note:/ Consider using 'authCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdacrrsAuthCode :: Lens.Lens' RetrieveDomainAuthCodeResponse Types.DomainAuthCode
rdacrrsAuthCode = Lens.field @"authCode"
{-# INLINEABLE rdacrrsAuthCode #-}
{-# DEPRECATED authCode "Use generic-lens or generic-optics with 'authCode' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdacrrsResponseStatus :: Lens.Lens' RetrieveDomainAuthCodeResponse Core.Int
rdacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rdacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
