{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.GetPackageVersionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of versions of the package, along with their creation time and commit message.
module Network.AWS.ElasticSearch.GetPackageVersionHistory
    (
    -- * Creating a request
      GetPackageVersionHistory (..)
    , mkGetPackageVersionHistory
    -- ** Request lenses
    , gpvhPackageID
    , gpvhMaxResults
    , gpvhNextToken

    -- * Destructuring the response
    , GetPackageVersionHistoryResponse (..)
    , mkGetPackageVersionHistoryResponse
    -- ** Response lenses
    , gpvhrrsNextToken
    , gpvhrrsPackageID
    , gpvhrrsPackageVersionHistoryList
    , gpvhrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'GetPackageVersionHistory' @ operation. 
--
-- /See:/ 'mkGetPackageVersionHistory' smart constructor.
data GetPackageVersionHistory = GetPackageVersionHistory'
  { packageID :: Types.PackageID
    -- ^ Returns an audit history of versions of the package.
  , maxResults :: Core.Maybe Core.Int
    -- ^ Limits results to a maximum number of versions.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPackageVersionHistory' value with any optional fields omitted.
mkGetPackageVersionHistory
    :: Types.PackageID -- ^ 'packageID'
    -> GetPackageVersionHistory
mkGetPackageVersionHistory packageID
  = GetPackageVersionHistory'{packageID, maxResults = Core.Nothing,
                              nextToken = Core.Nothing}

-- | Returns an audit history of versions of the package.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhPackageID :: Lens.Lens' GetPackageVersionHistory Types.PackageID
gpvhPackageID = Lens.field @"packageID"
{-# INLINEABLE gpvhPackageID #-}
{-# DEPRECATED packageID "Use generic-lens or generic-optics with 'packageID' instead"  #-}

-- | Limits results to a maximum number of versions.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhMaxResults :: Lens.Lens' GetPackageVersionHistory (Core.Maybe Core.Int)
gpvhMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gpvhMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhNextToken :: Lens.Lens' GetPackageVersionHistory (Core.Maybe Types.NextToken)
gpvhNextToken = Lens.field @"nextToken"
{-# INLINEABLE gpvhNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetPackageVersionHistory where
        toQuery GetPackageVersionHistory{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders GetPackageVersionHistory where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetPackageVersionHistory where
        type Rs GetPackageVersionHistory = GetPackageVersionHistoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2015-01-01/packages/" Core.<> Core.toText packageID Core.<>
                             "/history",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPackageVersionHistoryResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "PackageID" Core.<*>
                     x Core..:? "PackageVersionHistoryList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Container for response returned by @'GetPackageVersionHistory' @ operation. 
--
-- /See:/ 'mkGetPackageVersionHistoryResponse' smart constructor.
data GetPackageVersionHistoryResponse = GetPackageVersionHistoryResponse'
  { nextToken :: Core.Maybe Core.Text
  , packageID :: Core.Maybe Types.PackageID
  , packageVersionHistoryList :: Core.Maybe [Types.PackageVersionHistory]
    -- ^ List of @PackageVersionHistory@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetPackageVersionHistoryResponse' value with any optional fields omitted.
mkGetPackageVersionHistoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPackageVersionHistoryResponse
mkGetPackageVersionHistoryResponse responseStatus
  = GetPackageVersionHistoryResponse'{nextToken = Core.Nothing,
                                      packageID = Core.Nothing,
                                      packageVersionHistoryList = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhrrsNextToken :: Lens.Lens' GetPackageVersionHistoryResponse (Core.Maybe Core.Text)
gpvhrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gpvhrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhrrsPackageID :: Lens.Lens' GetPackageVersionHistoryResponse (Core.Maybe Types.PackageID)
gpvhrrsPackageID = Lens.field @"packageID"
{-# INLINEABLE gpvhrrsPackageID #-}
{-# DEPRECATED packageID "Use generic-lens or generic-optics with 'packageID' instead"  #-}

-- | List of @PackageVersionHistory@ objects.
--
-- /Note:/ Consider using 'packageVersionHistoryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhrrsPackageVersionHistoryList :: Lens.Lens' GetPackageVersionHistoryResponse (Core.Maybe [Types.PackageVersionHistory])
gpvhrrsPackageVersionHistoryList = Lens.field @"packageVersionHistoryList"
{-# INLINEABLE gpvhrrsPackageVersionHistoryList #-}
{-# DEPRECATED packageVersionHistoryList "Use generic-lens or generic-optics with 'packageVersionHistoryList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhrrsResponseStatus :: Lens.Lens' GetPackageVersionHistoryResponse Core.Int
gpvhrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpvhrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
