{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListPackagesForDomain (..),
    mkListPackagesForDomain,

    -- ** Request lenses
    lpfdDomainName,
    lpfdMaxResults,
    lpfdNextToken,

    -- * Destructuring the response
    ListPackagesForDomainResponse (..),
    mkListPackagesForDomainResponse,

    -- ** Response lenses
    lpfdrrsDomainPackageDetailsList,
    lpfdrrsNextToken,
    lpfdrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'ListPackagesForDomain' @ operation.
--
-- /See:/ 'mkListPackagesForDomain' smart constructor.
data ListPackagesForDomain = ListPackagesForDomain'
  { -- | The name of the domain for which you want to list associated packages.
    domainName :: Types.DomainName,
    -- | Limits results to a maximum number of packages.
    maxResults :: Core.Maybe Core.Int,
    -- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPackagesForDomain' value with any optional fields omitted.
mkListPackagesForDomain ::
  -- | 'domainName'
  Types.DomainName ->
  ListPackagesForDomain
mkListPackagesForDomain domainName =
  ListPackagesForDomain'
    { domainName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the domain for which you want to list associated packages.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdDomainName :: Lens.Lens' ListPackagesForDomain Types.DomainName
lpfdDomainName = Lens.field @"domainName"
{-# DEPRECATED lpfdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Limits results to a maximum number of packages.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdMaxResults :: Lens.Lens' ListPackagesForDomain (Core.Maybe Core.Int)
lpfdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lpfdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdNextToken :: Lens.Lens' ListPackagesForDomain (Core.Maybe Types.NextToken)
lpfdNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpfdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListPackagesForDomain where
  type Rs ListPackagesForDomain = ListPackagesForDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2015-01-01/domain/" Core.<> (Core.toText domainName)
                Core.<> ("/packages")
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
          ListPackagesForDomainResponse'
            Core.<$> (x Core..:? "DomainPackageDetailsList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Container for response parameters to @'ListPackagesForDomain' @ operation.
--
-- /See:/ 'mkListPackagesForDomainResponse' smart constructor.
data ListPackagesForDomainResponse = ListPackagesForDomainResponse'
  { -- | List of @DomainPackageDetails@ objects.
    domainPackageDetailsList :: Core.Maybe [Types.DomainPackageDetails],
    -- | Pagination token that needs to be supplied to the next call to get the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListPackagesForDomainResponse' value with any optional fields omitted.
mkListPackagesForDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPackagesForDomainResponse
mkListPackagesForDomainResponse responseStatus =
  ListPackagesForDomainResponse'
    { domainPackageDetailsList =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | List of @DomainPackageDetails@ objects.
--
-- /Note:/ Consider using 'domainPackageDetailsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdrrsDomainPackageDetailsList :: Lens.Lens' ListPackagesForDomainResponse (Core.Maybe [Types.DomainPackageDetails])
lpfdrrsDomainPackageDetailsList = Lens.field @"domainPackageDetailsList"
{-# DEPRECATED lpfdrrsDomainPackageDetailsList "Use generic-lens or generic-optics with 'domainPackageDetailsList' instead." #-}

-- | Pagination token that needs to be supplied to the next call to get the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdrrsNextToken :: Lens.Lens' ListPackagesForDomainResponse (Core.Maybe Types.String)
lpfdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpfdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfdrrsResponseStatus :: Lens.Lens' ListPackagesForDomainResponse Core.Int
lpfdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpfdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
