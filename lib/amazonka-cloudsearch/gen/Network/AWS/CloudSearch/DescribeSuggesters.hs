{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeSuggesters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the suggesters configured for a domain. A suggester enables you to display possible matches before users finish typing their queries. Can be limited to specific suggesters by name. By default, shows all suggesters and includes any pending changes to the configuration. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeSuggesters
    (
    -- * Creating a request
      DescribeSuggesters (..)
    , mkDescribeSuggesters
    -- ** Request lenses
    , dssDomainName
    , dssDeployed
    , dssSuggesterNames

    -- * Destructuring the response
    , DescribeSuggestersResponse (..)
    , mkDescribeSuggestersResponse
    -- ** Response lenses
    , dsrfrsSuggesters
    , dsrfrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeSuggester' @ operation. Specifies the name of the domain you want to describe. To restrict the response to particular suggesters, specify the names of the suggesters you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
-- /See:/ 'mkDescribeSuggesters' smart constructor.
data DescribeSuggesters = DescribeSuggesters'
  { domainName :: Types.DomainName
    -- ^ The name of the domain you want to describe.
  , deployed :: Core.Maybe Core.Bool
    -- ^ Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
  , suggesterNames :: Core.Maybe [Types.StandardName]
    -- ^ The suggesters you want to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSuggesters' value with any optional fields omitted.
mkDescribeSuggesters
    :: Types.DomainName -- ^ 'domainName'
    -> DescribeSuggesters
mkDescribeSuggesters domainName
  = DescribeSuggesters'{domainName, deployed = Core.Nothing,
                        suggesterNames = Core.Nothing}

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssDomainName :: Lens.Lens' DescribeSuggesters Types.DomainName
dssDomainName = Lens.field @"domainName"
{-# INLINEABLE dssDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssDeployed :: Lens.Lens' DescribeSuggesters (Core.Maybe Core.Bool)
dssDeployed = Lens.field @"deployed"
{-# INLINEABLE dssDeployed #-}
{-# DEPRECATED deployed "Use generic-lens or generic-optics with 'deployed' instead"  #-}

-- | The suggesters you want to describe.
--
-- /Note:/ Consider using 'suggesterNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssSuggesterNames :: Lens.Lens' DescribeSuggesters (Core.Maybe [Types.StandardName])
dssSuggesterNames = Lens.field @"suggesterNames"
{-# INLINEABLE dssSuggesterNames #-}
{-# DEPRECATED suggesterNames "Use generic-lens or generic-optics with 'suggesterNames' instead"  #-}

instance Core.ToQuery DescribeSuggesters where
        toQuery DescribeSuggesters{..}
          = Core.toQueryPair "Action" ("DescribeSuggesters" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Deployed") deployed
              Core.<>
              Core.toQueryPair "SuggesterNames"
                (Core.maybe Core.mempty (Core.toQueryList "member") suggesterNames)

instance Core.ToHeaders DescribeSuggesters where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeSuggesters where
        type Rs DescribeSuggesters = DescribeSuggestersResponse
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
          = Response.receiveXMLWrapper "DescribeSuggestersResult"
              (\ s h x ->
                 DescribeSuggestersResponse' Core.<$>
                   (x Core..@ "Suggesters" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DescribeSuggesters@ request.
--
-- /See:/ 'mkDescribeSuggestersResponse' smart constructor.
data DescribeSuggestersResponse = DescribeSuggestersResponse'
  { suggesters :: [Types.SuggesterStatus]
    -- ^ The suggesters configured for the domain specified in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSuggestersResponse' value with any optional fields omitted.
mkDescribeSuggestersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSuggestersResponse
mkDescribeSuggestersResponse responseStatus
  = DescribeSuggestersResponse'{suggesters = Core.mempty,
                                responseStatus}

-- | The suggesters configured for the domain specified in the request.
--
-- /Note:/ Consider using 'suggesters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsSuggesters :: Lens.Lens' DescribeSuggestersResponse [Types.SuggesterStatus]
dsrfrsSuggesters = Lens.field @"suggesters"
{-# INLINEABLE dsrfrsSuggesters #-}
{-# DEPRECATED suggesters "Use generic-lens or generic-optics with 'suggesters' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DescribeSuggestersResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
