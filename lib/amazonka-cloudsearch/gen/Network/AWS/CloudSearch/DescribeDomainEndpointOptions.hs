{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeDomainEndpointOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the domain's endpoint options, specifically whether all requests to the domain must arrive over HTTPS. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-domain-endpoint-options.html Configuring Domain Endpoint Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeDomainEndpointOptions
    (
    -- * Creating a request
      DescribeDomainEndpointOptions (..)
    , mkDescribeDomainEndpointOptions
    -- ** Request lenses
    , ddeoDomainName
    , ddeoDeployed

    -- * Destructuring the response
    , DescribeDomainEndpointOptionsResponse (..)
    , mkDescribeDomainEndpointOptionsResponse
    -- ** Response lenses
    , ddeorrsDomainEndpointOptions
    , ddeorrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeDomainEndpointOptions' @ operation. Specify the name of the domain you want to describe. To show the active configuration and exclude any pending changes, set the Deployed option to @true@ .
--
-- /See:/ 'mkDescribeDomainEndpointOptions' smart constructor.
data DescribeDomainEndpointOptions = DescribeDomainEndpointOptions'
  { domainName :: Types.DomainName
    -- ^ A string that represents the name of a domain.
  , deployed :: Core.Maybe Core.Bool
    -- ^ Whether to retrieve the latest configuration (which might be in a Processing state) or the current, active configuration. Defaults to @false@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDomainEndpointOptions' value with any optional fields omitted.
mkDescribeDomainEndpointOptions
    :: Types.DomainName -- ^ 'domainName'
    -> DescribeDomainEndpointOptions
mkDescribeDomainEndpointOptions domainName
  = DescribeDomainEndpointOptions'{domainName,
                                   deployed = Core.Nothing}

-- | A string that represents the name of a domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeoDomainName :: Lens.Lens' DescribeDomainEndpointOptions Types.DomainName
ddeoDomainName = Lens.field @"domainName"
{-# INLINEABLE ddeoDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Whether to retrieve the latest configuration (which might be in a Processing state) or the current, active configuration. Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeoDeployed :: Lens.Lens' DescribeDomainEndpointOptions (Core.Maybe Core.Bool)
ddeoDeployed = Lens.field @"deployed"
{-# INLINEABLE ddeoDeployed #-}
{-# DEPRECATED deployed "Use generic-lens or generic-optics with 'deployed' instead"  #-}

instance Core.ToQuery DescribeDomainEndpointOptions where
        toQuery DescribeDomainEndpointOptions{..}
          = Core.toQueryPair "Action"
              ("DescribeDomainEndpointOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Deployed") deployed

instance Core.ToHeaders DescribeDomainEndpointOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDomainEndpointOptions where
        type Rs DescribeDomainEndpointOptions =
             DescribeDomainEndpointOptionsResponse
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
          = Response.receiveXMLWrapper "DescribeDomainEndpointOptionsResult"
              (\ s h x ->
                 DescribeDomainEndpointOptionsResponse' Core.<$>
                   (x Core..@? "DomainEndpointOptions") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DescribeDomainEndpointOptions@ request. Contains the status and configuration of a search domain's endpoint options. 
--
-- /See:/ 'mkDescribeDomainEndpointOptionsResponse' smart constructor.
data DescribeDomainEndpointOptionsResponse = DescribeDomainEndpointOptionsResponse'
  { domainEndpointOptions :: Core.Maybe Types.DomainEndpointOptionsStatus
    -- ^ The status and configuration of a search domain's endpoint options.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDomainEndpointOptionsResponse' value with any optional fields omitted.
mkDescribeDomainEndpointOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDomainEndpointOptionsResponse
mkDescribeDomainEndpointOptionsResponse responseStatus
  = DescribeDomainEndpointOptionsResponse'{domainEndpointOptions =
                                             Core.Nothing,
                                           responseStatus}

-- | The status and configuration of a search domain's endpoint options.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeorrsDomainEndpointOptions :: Lens.Lens' DescribeDomainEndpointOptionsResponse (Core.Maybe Types.DomainEndpointOptionsStatus)
ddeorrsDomainEndpointOptions = Lens.field @"domainEndpointOptions"
{-# INLINEABLE ddeorrsDomainEndpointOptions #-}
{-# DEPRECATED domainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeorrsResponseStatus :: Lens.Lens' DescribeDomainEndpointOptionsResponse Core.Int
ddeorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddeorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
