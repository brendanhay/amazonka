{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeAvailabilityOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the availability options configured for a domain. By default, shows the configuration with any pending changes. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html Configuring Availability Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeAvailabilityOptions
    (
    -- * Creating a request
      DescribeAvailabilityOptions (..)
    , mkDescribeAvailabilityOptions
    -- ** Request lenses
    , daoDomainName
    , daoDeployed

    -- * Destructuring the response
    , DescribeAvailabilityOptionsResponse (..)
    , mkDescribeAvailabilityOptionsResponse
    -- ** Response lenses
    , daorrsAvailabilityOptions
    , daorrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeAvailabilityOptions' @ operation. Specifies the name of the domain you want to describe. To show the active configuration and exclude any pending changes, set the Deployed option to @true@ .
--
-- /See:/ 'mkDescribeAvailabilityOptions' smart constructor.
data DescribeAvailabilityOptions = DescribeAvailabilityOptions'
  { domainName :: Types.DomainName
    -- ^ The name of the domain you want to describe.
  , deployed :: Core.Maybe Core.Bool
    -- ^ Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAvailabilityOptions' value with any optional fields omitted.
mkDescribeAvailabilityOptions
    :: Types.DomainName -- ^ 'domainName'
    -> DescribeAvailabilityOptions
mkDescribeAvailabilityOptions domainName
  = DescribeAvailabilityOptions'{domainName, deployed = Core.Nothing}

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoDomainName :: Lens.Lens' DescribeAvailabilityOptions Types.DomainName
daoDomainName = Lens.field @"domainName"
{-# INLINEABLE daoDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoDeployed :: Lens.Lens' DescribeAvailabilityOptions (Core.Maybe Core.Bool)
daoDeployed = Lens.field @"deployed"
{-# INLINEABLE daoDeployed #-}
{-# DEPRECATED deployed "Use generic-lens or generic-optics with 'deployed' instead"  #-}

instance Core.ToQuery DescribeAvailabilityOptions where
        toQuery DescribeAvailabilityOptions{..}
          = Core.toQueryPair "Action"
              ("DescribeAvailabilityOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Deployed") deployed

instance Core.ToHeaders DescribeAvailabilityOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAvailabilityOptions where
        type Rs DescribeAvailabilityOptions =
             DescribeAvailabilityOptionsResponse
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
          = Response.receiveXMLWrapper "DescribeAvailabilityOptionsResult"
              (\ s h x ->
                 DescribeAvailabilityOptionsResponse' Core.<$>
                   (x Core..@? "AvailabilityOptions") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DescribeAvailabilityOptions@ request. Indicates whether or not the Multi-AZ option is enabled for the domain specified in the request. 
--
-- /See:/ 'mkDescribeAvailabilityOptionsResponse' smart constructor.
data DescribeAvailabilityOptionsResponse = DescribeAvailabilityOptionsResponse'
  { availabilityOptions :: Core.Maybe Types.AvailabilityOptionsStatus
    -- ^ The availability options configured for the domain. Indicates whether Multi-AZ is enabled for the domain. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAvailabilityOptionsResponse' value with any optional fields omitted.
mkDescribeAvailabilityOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAvailabilityOptionsResponse
mkDescribeAvailabilityOptionsResponse responseStatus
  = DescribeAvailabilityOptionsResponse'{availabilityOptions =
                                           Core.Nothing,
                                         responseStatus}

-- | The availability options configured for the domain. Indicates whether Multi-AZ is enabled for the domain. 
--
-- /Note:/ Consider using 'availabilityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daorrsAvailabilityOptions :: Lens.Lens' DescribeAvailabilityOptionsResponse (Core.Maybe Types.AvailabilityOptionsStatus)
daorrsAvailabilityOptions = Lens.field @"availabilityOptions"
{-# INLINEABLE daorrsAvailabilityOptions #-}
{-# DEPRECATED availabilityOptions "Use generic-lens or generic-optics with 'availabilityOptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daorrsResponseStatus :: Lens.Lens' DescribeAvailabilityOptionsResponse Core.Int
daorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
