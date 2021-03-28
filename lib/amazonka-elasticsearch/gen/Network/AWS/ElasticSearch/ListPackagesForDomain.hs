{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.ListPackagesForDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all packages associated with the Amazon ES domain.
module Network.AWS.ElasticSearch.ListPackagesForDomain
    (
    -- * Creating a request
      ListPackagesForDomain (..)
    , mkListPackagesForDomain
    -- ** Request lenses
    , lpfdDomainName
    , lpfdMaxResults
    , lpfdNextToken

    -- * Destructuring the response
    , ListPackagesForDomainResponse (..)
    , mkListPackagesForDomainResponse
    -- ** Response lenses
    , lpfdrrsDomainPackageDetailsList
    , lpfdrrsNextToken
    , lpfdrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'ListPackagesForDomain' @ operation. 
--
-- /See:/ 'mkListPackagesForDomain' smart constructor.
data ListPackagesForDomain = ListPackagesForDomain'
  { domainName :: Types.DomainName
    -- ^ The name of the domain for which you want to list associated packages.
  , maxResults :: Core.Maybe Core.Int
    -- ^ Limits results to a maximum number of packages.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPackagesForDomain' value with any optional fields omitted.
mkListPackagesForDomain
    :: Types.DomainName -- ^ 'domainName'
    -> ListPackagesForDomain
mkListPackagesForDomain domainName
  = ListPackagesForDomain'{domainName, maxResults = Core.Nothing,
                           nextToken = Core.Nothing}

-- | The name of the domain for which you want to list associated packages.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdDomainName :: Lens.Lens' ListPackagesForDomain Types.DomainName
lpfdDomainName = Lens.field @"domainName"
{-# INLINEABLE lpfdDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Limits results to a maximum number of packages.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdMaxResults :: Lens.Lens' ListPackagesForDomain (Core.Maybe Core.Int)
lpfdMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpfdMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdNextToken :: Lens.Lens' ListPackagesForDomain (Core.Maybe Types.NextToken)
lpfdNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpfdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPackagesForDomain where
        toQuery ListPackagesForDomain{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListPackagesForDomain where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListPackagesForDomain where
        type Rs ListPackagesForDomain = ListPackagesForDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2015-01-01/domain/" Core.<> Core.toText domainName Core.<>
                             "/packages",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPackagesForDomainResponse' Core.<$>
                   (x Core..:? "DomainPackageDetailsList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Container for response parameters to @'ListPackagesForDomain' @ operation. 
--
-- /See:/ 'mkListPackagesForDomainResponse' smart constructor.
data ListPackagesForDomainResponse = ListPackagesForDomainResponse'
  { domainPackageDetailsList :: Core.Maybe [Types.DomainPackageDetails]
    -- ^ List of @DomainPackageDetails@ objects.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Pagination token that needs to be supplied to the next call to get the next page of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPackagesForDomainResponse' value with any optional fields omitted.
mkListPackagesForDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPackagesForDomainResponse
mkListPackagesForDomainResponse responseStatus
  = ListPackagesForDomainResponse'{domainPackageDetailsList =
                                     Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | List of @DomainPackageDetails@ objects.
--
-- /Note:/ Consider using 'domainPackageDetailsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdrrsDomainPackageDetailsList :: Lens.Lens' ListPackagesForDomainResponse (Core.Maybe [Types.DomainPackageDetails])
lpfdrrsDomainPackageDetailsList = Lens.field @"domainPackageDetailsList"
{-# INLINEABLE lpfdrrsDomainPackageDetailsList #-}
{-# DEPRECATED domainPackageDetailsList "Use generic-lens or generic-optics with 'domainPackageDetailsList' instead"  #-}

-- | Pagination token that needs to be supplied to the next call to get the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdrrsNextToken :: Lens.Lens' ListPackagesForDomainResponse (Core.Maybe Core.Text)
lpfdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpfdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdrrsResponseStatus :: Lens.Lens' ListPackagesForDomainResponse Core.Int
lpfdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpfdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
