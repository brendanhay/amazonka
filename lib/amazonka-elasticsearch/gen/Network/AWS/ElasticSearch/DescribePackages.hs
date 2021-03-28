{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribePackages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all packages available to Amazon ES. Includes options for filtering, limiting the number of results, and pagination.
module Network.AWS.ElasticSearch.DescribePackages
    (
    -- * Creating a request
      DescribePackages (..)
    , mkDescribePackages
    -- ** Request lenses
    , dpFilters
    , dpMaxResults
    , dpNextToken

    -- * Destructuring the response
    , DescribePackagesResponse (..)
    , mkDescribePackagesResponse
    -- ** Response lenses
    , dprrsNextToken
    , dprrsPackageDetailsList
    , dprrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'DescribePackage' @ operation. 
--
-- /See:/ 'mkDescribePackages' smart constructor.
data DescribePackages = DescribePackages'
  { filters :: Core.Maybe [Types.DescribePackagesFilter]
    -- ^ Only returns packages that match the @DescribePackagesFilterList@ values.
  , maxResults :: Core.Maybe Core.Int
    -- ^ Limits results to a maximum number of packages.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePackages' value with any optional fields omitted.
mkDescribePackages
    :: DescribePackages
mkDescribePackages
  = DescribePackages'{filters = Core.Nothing,
                      maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Only returns packages that match the @DescribePackagesFilterList@ values.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpFilters :: Lens.Lens' DescribePackages (Core.Maybe [Types.DescribePackagesFilter])
dpFilters = Lens.field @"filters"
{-# INLINEABLE dpFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | Limits results to a maximum number of packages.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMaxResults :: Lens.Lens' DescribePackages (Core.Maybe Core.Int)
dpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpNextToken :: Lens.Lens' DescribePackages (Core.Maybe Types.NextToken)
dpNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribePackages where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribePackages where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON DescribePackages where
        toJSON DescribePackages{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribePackages where
        type Rs DescribePackages = DescribePackagesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2015-01-01/packages/describe",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribePackagesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "PackageDetailsList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Container for response returned by @'DescribePackages' @ operation. 
--
-- /See:/ 'mkDescribePackagesResponse' smart constructor.
data DescribePackagesResponse = DescribePackagesResponse'
  { nextToken :: Core.Maybe Core.Text
  , packageDetailsList :: Core.Maybe [Types.PackageDetails]
    -- ^ List of @PackageDetails@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribePackagesResponse' value with any optional fields omitted.
mkDescribePackagesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePackagesResponse
mkDescribePackagesResponse responseStatus
  = DescribePackagesResponse'{nextToken = Core.Nothing,
                              packageDetailsList = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsNextToken :: Lens.Lens' DescribePackagesResponse (Core.Maybe Core.Text)
dprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | List of @PackageDetails@ objects.
--
-- /Note:/ Consider using 'packageDetailsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsPackageDetailsList :: Lens.Lens' DescribePackagesResponse (Core.Maybe [Types.PackageDetails])
dprrsPackageDetailsList = Lens.field @"packageDetailsList"
{-# INLINEABLE dprrsPackageDetailsList #-}
{-# DEPRECATED packageDetailsList "Use generic-lens or generic-optics with 'packageDetailsList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DescribePackagesResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
