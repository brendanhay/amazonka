{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific domain recordset.
module Network.AWS.Lightsail.GetDomain
    (
    -- * Creating a request
      GetDomain (..)
    , mkGetDomain
    -- ** Request lenses
    , gdDomainName

    -- * Destructuring the response
    , GetDomainResponse (..)
    , mkGetDomainResponse
    -- ** Response lenses
    , gdrgrsDomain
    , gdrgrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDomain' smart constructor.
newtype GetDomain = GetDomain'
  { domainName :: Types.DomainName
    -- ^ The domain name for which your want to return information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDomain' value with any optional fields omitted.
mkGetDomain
    :: Types.DomainName -- ^ 'domainName'
    -> GetDomain
mkGetDomain domainName = GetDomain'{domainName}

-- | The domain name for which your want to return information about.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDomainName :: Lens.Lens' GetDomain Types.DomainName
gdDomainName = Lens.field @"domainName"
{-# INLINEABLE gdDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery GetDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDomain where
        toHeaders GetDomain{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetDomain")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDomain where
        toJSON GetDomain{..}
          = Core.object
              (Core.catMaybes [Core.Just ("domainName" Core..= domainName)])

instance Core.AWSRequest GetDomain where
        type Rs GetDomain = GetDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDomainResponse' Core.<$>
                   (x Core..:? "domain") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDomainResponse' smart constructor.
data GetDomainResponse = GetDomainResponse'
  { domain :: Core.Maybe Types.Domain
    -- ^ An array of key-value pairs containing information about your get domain request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDomainResponse' value with any optional fields omitted.
mkGetDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDomainResponse
mkGetDomainResponse responseStatus
  = GetDomainResponse'{domain = Core.Nothing, responseStatus}

-- | An array of key-value pairs containing information about your get domain request.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrgrsDomain :: Lens.Lens' GetDomainResponse (Core.Maybe Types.Domain)
gdrgrsDomain = Lens.field @"domain"
{-# INLINEABLE gdrgrsDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrgrsResponseStatus :: Lens.Lens' GetDomainResponse Core.Int
gdrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
