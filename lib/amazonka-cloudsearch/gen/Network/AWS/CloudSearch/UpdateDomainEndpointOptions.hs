{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.UpdateDomainEndpointOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the domain's endpoint options, specifically whether all requests to the domain must arrive over HTTPS. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-domain-endpoint-options.html Configuring Domain Endpoint Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.UpdateDomainEndpointOptions
    (
    -- * Creating a request
      UpdateDomainEndpointOptions (..)
    , mkUpdateDomainEndpointOptions
    -- ** Request lenses
    , udeoDomainName
    , udeoDomainEndpointOptions

    -- * Destructuring the response
    , UpdateDomainEndpointOptionsResponse (..)
    , mkUpdateDomainEndpointOptionsResponse
    -- ** Response lenses
    , udeorrsDomainEndpointOptions
    , udeorrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'UpdateDomainEndpointOptions' @ operation. Specifies the name of the domain you want to update and the domain endpoint options.
--
-- /See:/ 'mkUpdateDomainEndpointOptions' smart constructor.
data UpdateDomainEndpointOptions = UpdateDomainEndpointOptions'
  { domainName :: Types.DomainName
    -- ^ A string that represents the name of a domain.
  , domainEndpointOptions :: Types.DomainEndpointOptions
    -- ^ Whether to require that all requests to the domain arrive over HTTPS. We recommend Policy-Min-TLS-1-2-2019-07 for TLSSecurityPolicy. For compatibility with older clients, the default is Policy-Min-TLS-1-0-2019-07. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainEndpointOptions' value with any optional fields omitted.
mkUpdateDomainEndpointOptions
    :: Types.DomainName -- ^ 'domainName'
    -> Types.DomainEndpointOptions -- ^ 'domainEndpointOptions'
    -> UpdateDomainEndpointOptions
mkUpdateDomainEndpointOptions domainName domainEndpointOptions
  = UpdateDomainEndpointOptions'{domainName, domainEndpointOptions}

-- | A string that represents the name of a domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeoDomainName :: Lens.Lens' UpdateDomainEndpointOptions Types.DomainName
udeoDomainName = Lens.field @"domainName"
{-# INLINEABLE udeoDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Whether to require that all requests to the domain arrive over HTTPS. We recommend Policy-Min-TLS-1-2-2019-07 for TLSSecurityPolicy. For compatibility with older clients, the default is Policy-Min-TLS-1-0-2019-07. 
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeoDomainEndpointOptions :: Lens.Lens' UpdateDomainEndpointOptions Types.DomainEndpointOptions
udeoDomainEndpointOptions = Lens.field @"domainEndpointOptions"
{-# INLINEABLE udeoDomainEndpointOptions #-}
{-# DEPRECATED domainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead"  #-}

instance Core.ToQuery UpdateDomainEndpointOptions where
        toQuery UpdateDomainEndpointOptions{..}
          = Core.toQueryPair "Action"
              ("UpdateDomainEndpointOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<>
              Core.toQueryPair "DomainEndpointOptions" domainEndpointOptions

instance Core.ToHeaders UpdateDomainEndpointOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateDomainEndpointOptions where
        type Rs UpdateDomainEndpointOptions =
             UpdateDomainEndpointOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "UpdateDomainEndpointOptionsResult"
              (\ s h x ->
                 UpdateDomainEndpointOptionsResponse' Core.<$>
                   (x Core..@? "DomainEndpointOptions") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @UpdateDomainEndpointOptions@ request. Contains the configuration and status of the domain's endpoint options. 
--
-- /See:/ 'mkUpdateDomainEndpointOptionsResponse' smart constructor.
data UpdateDomainEndpointOptionsResponse = UpdateDomainEndpointOptionsResponse'
  { domainEndpointOptions :: Core.Maybe Types.DomainEndpointOptionsStatus
    -- ^ The newly-configured domain endpoint options.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateDomainEndpointOptionsResponse' value with any optional fields omitted.
mkUpdateDomainEndpointOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDomainEndpointOptionsResponse
mkUpdateDomainEndpointOptionsResponse responseStatus
  = UpdateDomainEndpointOptionsResponse'{domainEndpointOptions =
                                           Core.Nothing,
                                         responseStatus}

-- | The newly-configured domain endpoint options.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeorrsDomainEndpointOptions :: Lens.Lens' UpdateDomainEndpointOptionsResponse (Core.Maybe Types.DomainEndpointOptionsStatus)
udeorrsDomainEndpointOptions = Lens.field @"domainEndpointOptions"
{-# INLINEABLE udeorrsDomainEndpointOptions #-}
{-# DEPRECATED domainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeorrsResponseStatus :: Lens.Lens' UpdateDomainEndpointOptionsResponse Core.Int
udeorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udeorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
