{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.ListDomainsForPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Amazon ES domains associated with the package.
module Network.AWS.ElasticSearch.ListDomainsForPackage
    (
    -- * Creating a request
      ListDomainsForPackage (..)
    , mkListDomainsForPackage
    -- ** Request lenses
    , ldfpPackageID
    , ldfpMaxResults
    , ldfpNextToken

    -- * Destructuring the response
    , ListDomainsForPackageResponse (..)
    , mkListDomainsForPackageResponse
    -- ** Response lenses
    , ldfprrsDomainPackageDetailsList
    , ldfprrsNextToken
    , ldfprrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'ListDomainsForPackage' @ operation. 
--
-- /See:/ 'mkListDomainsForPackage' smart constructor.
data ListDomainsForPackage = ListDomainsForPackage'
  { packageID :: Types.PackageID
    -- ^ The package for which to list domains.
  , maxResults :: Core.Maybe Core.Int
    -- ^ Limits results to a maximum number of domains.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomainsForPackage' value with any optional fields omitted.
mkListDomainsForPackage
    :: Types.PackageID -- ^ 'packageID'
    -> ListDomainsForPackage
mkListDomainsForPackage packageID
  = ListDomainsForPackage'{packageID, maxResults = Core.Nothing,
                           nextToken = Core.Nothing}

-- | The package for which to list domains.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfpPackageID :: Lens.Lens' ListDomainsForPackage Types.PackageID
ldfpPackageID = Lens.field @"packageID"
{-# INLINEABLE ldfpPackageID #-}
{-# DEPRECATED packageID "Use generic-lens or generic-optics with 'packageID' instead"  #-}

-- | Limits results to a maximum number of domains.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfpMaxResults :: Lens.Lens' ListDomainsForPackage (Core.Maybe Core.Int)
ldfpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ldfpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfpNextToken :: Lens.Lens' ListDomainsForPackage (Core.Maybe Types.NextToken)
ldfpNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldfpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListDomainsForPackage where
        toQuery ListDomainsForPackage{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListDomainsForPackage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListDomainsForPackage where
        type Rs ListDomainsForPackage = ListDomainsForPackageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2015-01-01/packages/" Core.<> Core.toText packageID Core.<>
                             "/domains",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDomainsForPackageResponse' Core.<$>
                   (x Core..:? "DomainPackageDetailsList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Container for response parameters to @'ListDomainsForPackage' @ operation. 
--
-- /See:/ 'mkListDomainsForPackageResponse' smart constructor.
data ListDomainsForPackageResponse = ListDomainsForPackageResponse'
  { domainPackageDetailsList :: Core.Maybe [Types.DomainPackageDetails]
    -- ^ List of @DomainPackageDetails@ objects.
  , nextToken :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDomainsForPackageResponse' value with any optional fields omitted.
mkListDomainsForPackageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDomainsForPackageResponse
mkListDomainsForPackageResponse responseStatus
  = ListDomainsForPackageResponse'{domainPackageDetailsList =
                                     Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | List of @DomainPackageDetails@ objects.
--
-- /Note:/ Consider using 'domainPackageDetailsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfprrsDomainPackageDetailsList :: Lens.Lens' ListDomainsForPackageResponse (Core.Maybe [Types.DomainPackageDetails])
ldfprrsDomainPackageDetailsList = Lens.field @"domainPackageDetailsList"
{-# INLINEABLE ldfprrsDomainPackageDetailsList #-}
{-# DEPRECATED domainPackageDetailsList "Use generic-lens or generic-optics with 'domainPackageDetailsList' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfprrsNextToken :: Lens.Lens' ListDomainsForPackageResponse (Core.Maybe Core.Text)
ldfprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldfprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfprrsResponseStatus :: Lens.Lens' ListDomainsForPackageResponse Core.Int
ldfprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldfprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
