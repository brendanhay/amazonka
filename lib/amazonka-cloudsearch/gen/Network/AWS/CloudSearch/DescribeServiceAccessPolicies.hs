{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeServiceAccessPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the access policies that control access to the domain's document and search endpoints. By default, shows the configuration with any pending changes. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-access.html Configuring Access for a Search Domain> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeServiceAccessPolicies
    (
    -- * Creating a request
      DescribeServiceAccessPolicies (..)
    , mkDescribeServiceAccessPolicies
    -- ** Request lenses
    , dsapDomainName
    , dsapDeployed

    -- * Destructuring the response
    , DescribeServiceAccessPoliciesResponse (..)
    , mkDescribeServiceAccessPoliciesResponse
    -- ** Response lenses
    , dsaprrsAccessPolicies
    , dsaprrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeServiceAccessPolicies' @ operation. Specifies the name of the domain you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
-- /See:/ 'mkDescribeServiceAccessPolicies' smart constructor.
data DescribeServiceAccessPolicies = DescribeServiceAccessPolicies'
  { domainName :: Types.DomainName
    -- ^ The name of the domain you want to describe.
  , deployed :: Core.Maybe Core.Bool
    -- ^ Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServiceAccessPolicies' value with any optional fields omitted.
mkDescribeServiceAccessPolicies
    :: Types.DomainName -- ^ 'domainName'
    -> DescribeServiceAccessPolicies
mkDescribeServiceAccessPolicies domainName
  = DescribeServiceAccessPolicies'{domainName,
                                   deployed = Core.Nothing}

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsapDomainName :: Lens.Lens' DescribeServiceAccessPolicies Types.DomainName
dsapDomainName = Lens.field @"domainName"
{-# INLINEABLE dsapDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsapDeployed :: Lens.Lens' DescribeServiceAccessPolicies (Core.Maybe Core.Bool)
dsapDeployed = Lens.field @"deployed"
{-# INLINEABLE dsapDeployed #-}
{-# DEPRECATED deployed "Use generic-lens or generic-optics with 'deployed' instead"  #-}

instance Core.ToQuery DescribeServiceAccessPolicies where
        toQuery DescribeServiceAccessPolicies{..}
          = Core.toQueryPair "Action"
              ("DescribeServiceAccessPolicies" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Deployed") deployed

instance Core.ToHeaders DescribeServiceAccessPolicies where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeServiceAccessPolicies where
        type Rs DescribeServiceAccessPolicies =
             DescribeServiceAccessPoliciesResponse
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
          = Response.receiveXMLWrapper "DescribeServiceAccessPoliciesResult"
              (\ s h x ->
                 DescribeServiceAccessPoliciesResponse' Core.<$>
                   (x Core..@ "AccessPolicies") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DescribeServiceAccessPolicies@ request.
--
-- /See:/ 'mkDescribeServiceAccessPoliciesResponse' smart constructor.
data DescribeServiceAccessPoliciesResponse = DescribeServiceAccessPoliciesResponse'
  { accessPolicies :: Types.AccessPoliciesStatus
    -- ^ The access rules configured for the domain specified in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeServiceAccessPoliciesResponse' value with any optional fields omitted.
mkDescribeServiceAccessPoliciesResponse
    :: Types.AccessPoliciesStatus -- ^ 'accessPolicies'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeServiceAccessPoliciesResponse
mkDescribeServiceAccessPoliciesResponse accessPolicies
  responseStatus
  = DescribeServiceAccessPoliciesResponse'{accessPolicies,
                                           responseStatus}

-- | The access rules configured for the domain specified in the request.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaprrsAccessPolicies :: Lens.Lens' DescribeServiceAccessPoliciesResponse Types.AccessPoliciesStatus
dsaprrsAccessPolicies = Lens.field @"accessPolicies"
{-# INLINEABLE dsaprrsAccessPolicies #-}
{-# DEPRECATED accessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaprrsResponseStatus :: Lens.Lens' DescribeServiceAccessPoliciesResponse Core.Int
dsaprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsaprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
