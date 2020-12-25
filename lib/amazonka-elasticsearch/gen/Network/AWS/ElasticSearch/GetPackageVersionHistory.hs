{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetPackageVersionHistory (..),
    mkGetPackageVersionHistory,

    -- ** Request lenses
    gpvhPackageID,
    gpvhMaxResults,
    gpvhNextToken,

    -- * Destructuring the response
    GetPackageVersionHistoryResponse (..),
    mkGetPackageVersionHistoryResponse,

    -- ** Response lenses
    gpvhrrsNextToken,
    gpvhrrsPackageID,
    gpvhrrsPackageVersionHistoryList,
    gpvhrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'GetPackageVersionHistory' @ operation.
--
-- /See:/ 'mkGetPackageVersionHistory' smart constructor.
data GetPackageVersionHistory = GetPackageVersionHistory'
  { -- | Returns an audit history of versions of the package.
    packageID :: Types.PackageID,
    -- | Limits results to a maximum number of versions.
    maxResults :: Core.Maybe Core.Int,
    -- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPackageVersionHistory' value with any optional fields omitted.
mkGetPackageVersionHistory ::
  -- | 'packageID'
  Types.PackageID ->
  GetPackageVersionHistory
mkGetPackageVersionHistory packageID =
  GetPackageVersionHistory'
    { packageID,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Returns an audit history of versions of the package.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhPackageID :: Lens.Lens' GetPackageVersionHistory Types.PackageID
gpvhPackageID = Lens.field @"packageID"
{-# DEPRECATED gpvhPackageID "Use generic-lens or generic-optics with 'packageID' instead." #-}

-- | Limits results to a maximum number of versions.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhMaxResults :: Lens.Lens' GetPackageVersionHistory (Core.Maybe Core.Int)
gpvhMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gpvhMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhNextToken :: Lens.Lens' GetPackageVersionHistory (Core.Maybe Types.NextToken)
gpvhNextToken = Lens.field @"nextToken"
{-# DEPRECATED gpvhNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetPackageVersionHistory where
  type Rs GetPackageVersionHistory = GetPackageVersionHistoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2015-01-01/packages/" Core.<> (Core.toText packageID)
                Core.<> ("/history")
            ),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPackageVersionHistoryResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "PackageID")
            Core.<*> (x Core..:? "PackageVersionHistoryList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Container for response returned by @'GetPackageVersionHistory' @ operation.
--
-- /See:/ 'mkGetPackageVersionHistoryResponse' smart constructor.
data GetPackageVersionHistoryResponse = GetPackageVersionHistoryResponse'
  { nextToken :: Core.Maybe Types.String,
    packageID :: Core.Maybe Types.PackageID,
    -- | List of @PackageVersionHistory@ objects.
    packageVersionHistoryList :: Core.Maybe [Types.PackageVersionHistory],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetPackageVersionHistoryResponse' value with any optional fields omitted.
mkGetPackageVersionHistoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPackageVersionHistoryResponse
mkGetPackageVersionHistoryResponse responseStatus =
  GetPackageVersionHistoryResponse'
    { nextToken = Core.Nothing,
      packageID = Core.Nothing,
      packageVersionHistoryList = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhrrsNextToken :: Lens.Lens' GetPackageVersionHistoryResponse (Core.Maybe Types.String)
gpvhrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gpvhrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhrrsPackageID :: Lens.Lens' GetPackageVersionHistoryResponse (Core.Maybe Types.PackageID)
gpvhrrsPackageID = Lens.field @"packageID"
{-# DEPRECATED gpvhrrsPackageID "Use generic-lens or generic-optics with 'packageID' instead." #-}

-- | List of @PackageVersionHistory@ objects.
--
-- /Note:/ Consider using 'packageVersionHistoryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhrrsPackageVersionHistoryList :: Lens.Lens' GetPackageVersionHistoryResponse (Core.Maybe [Types.PackageVersionHistory])
gpvhrrsPackageVersionHistoryList = Lens.field @"packageVersionHistoryList"
{-# DEPRECATED gpvhrrsPackageVersionHistoryList "Use generic-lens or generic-optics with 'packageVersionHistoryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvhrrsResponseStatus :: Lens.Lens' GetPackageVersionHistoryResponse Core.Int
gpvhrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gpvhrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
