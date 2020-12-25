{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListDomainsForPackage (..),
    mkListDomainsForPackage,

    -- ** Request lenses
    ldfpPackageID,
    ldfpMaxResults,
    ldfpNextToken,

    -- * Destructuring the response
    ListDomainsForPackageResponse (..),
    mkListDomainsForPackageResponse,

    -- ** Response lenses
    ldfprrsDomainPackageDetailsList,
    ldfprrsNextToken,
    ldfprrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'ListDomainsForPackage' @ operation.
--
-- /See:/ 'mkListDomainsForPackage' smart constructor.
data ListDomainsForPackage = ListDomainsForPackage'
  { -- | The package for which to list domains.
    packageID :: Types.PackageID,
    -- | Limits results to a maximum number of domains.
    maxResults :: Core.Maybe Core.Int,
    -- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomainsForPackage' value with any optional fields omitted.
mkListDomainsForPackage ::
  -- | 'packageID'
  Types.PackageID ->
  ListDomainsForPackage
mkListDomainsForPackage packageID =
  ListDomainsForPackage'
    { packageID,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The package for which to list domains.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfpPackageID :: Lens.Lens' ListDomainsForPackage Types.PackageID
ldfpPackageID = Lens.field @"packageID"
{-# DEPRECATED ldfpPackageID "Use generic-lens or generic-optics with 'packageID' instead." #-}

-- | Limits results to a maximum number of domains.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfpMaxResults :: Lens.Lens' ListDomainsForPackage (Core.Maybe Core.Int)
ldfpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldfpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfpNextToken :: Lens.Lens' ListDomainsForPackage (Core.Maybe Types.NextToken)
ldfpNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldfpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListDomainsForPackage where
  type Rs ListDomainsForPackage = ListDomainsForPackageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2015-01-01/packages/" Core.<> (Core.toText packageID)
                Core.<> ("/domains")
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
          ListDomainsForPackageResponse'
            Core.<$> (x Core..:? "DomainPackageDetailsList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Container for response parameters to @'ListDomainsForPackage' @ operation.
--
-- /See:/ 'mkListDomainsForPackageResponse' smart constructor.
data ListDomainsForPackageResponse = ListDomainsForPackageResponse'
  { -- | List of @DomainPackageDetails@ objects.
    domainPackageDetailsList :: Core.Maybe [Types.DomainPackageDetails],
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDomainsForPackageResponse' value with any optional fields omitted.
mkListDomainsForPackageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDomainsForPackageResponse
mkListDomainsForPackageResponse responseStatus =
  ListDomainsForPackageResponse'
    { domainPackageDetailsList =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | List of @DomainPackageDetails@ objects.
--
-- /Note:/ Consider using 'domainPackageDetailsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfprrsDomainPackageDetailsList :: Lens.Lens' ListDomainsForPackageResponse (Core.Maybe [Types.DomainPackageDetails])
ldfprrsDomainPackageDetailsList = Lens.field @"domainPackageDetailsList"
{-# DEPRECATED ldfprrsDomainPackageDetailsList "Use generic-lens or generic-optics with 'domainPackageDetailsList' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfprrsNextToken :: Lens.Lens' ListDomainsForPackageResponse (Core.Maybe Types.String)
ldfprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldfprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldfprrsResponseStatus :: Lens.Lens' ListDomainsForPackageResponse Core.Int
ldfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
