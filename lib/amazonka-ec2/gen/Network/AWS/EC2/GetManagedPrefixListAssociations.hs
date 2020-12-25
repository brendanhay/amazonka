{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetManagedPrefixListAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the resources that are associated with the specified managed prefix list.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetManagedPrefixListAssociations
  ( -- * Creating a request
    GetManagedPrefixListAssociations (..),
    mkGetManagedPrefixListAssociations,

    -- ** Request lenses
    gmplaPrefixListId,
    gmplaDryRun,
    gmplaMaxResults,
    gmplaNextToken,

    -- * Destructuring the response
    GetManagedPrefixListAssociationsResponse (..),
    mkGetManagedPrefixListAssociationsResponse,

    -- ** Response lenses
    gmplarrsNextToken,
    gmplarrsPrefixListAssociations,
    gmplarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetManagedPrefixListAssociations' smart constructor.
data GetManagedPrefixListAssociations = GetManagedPrefixListAssociations'
  { -- | The ID of the prefix list.
    prefixListId :: Types.PrefixListResourceId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetManagedPrefixListAssociations' value with any optional fields omitted.
mkGetManagedPrefixListAssociations ::
  -- | 'prefixListId'
  Types.PrefixListResourceId ->
  GetManagedPrefixListAssociations
mkGetManagedPrefixListAssociations prefixListId =
  GetManagedPrefixListAssociations'
    { prefixListId,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplaPrefixListId :: Lens.Lens' GetManagedPrefixListAssociations Types.PrefixListResourceId
gmplaPrefixListId = Lens.field @"prefixListId"
{-# DEPRECATED gmplaPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplaDryRun :: Lens.Lens' GetManagedPrefixListAssociations (Core.Maybe Core.Bool)
gmplaDryRun = Lens.field @"dryRun"
{-# DEPRECATED gmplaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplaMaxResults :: Lens.Lens' GetManagedPrefixListAssociations (Core.Maybe Core.Natural)
gmplaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gmplaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplaNextToken :: Lens.Lens' GetManagedPrefixListAssociations (Core.Maybe Types.NextToken)
gmplaNextToken = Lens.field @"nextToken"
{-# DEPRECATED gmplaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetManagedPrefixListAssociations where
  type
    Rs GetManagedPrefixListAssociations =
      GetManagedPrefixListAssociationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetManagedPrefixListAssociations")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "PrefixListId" prefixListId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetManagedPrefixListAssociationsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "prefixListAssociationSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetManagedPrefixListAssociations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"prefixListAssociations" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetManagedPrefixListAssociationsResponse' smart constructor.
data GetManagedPrefixListAssociationsResponse = GetManagedPrefixListAssociationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the associations.
    prefixListAssociations :: Core.Maybe [Types.PrefixListAssociation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetManagedPrefixListAssociationsResponse' value with any optional fields omitted.
mkGetManagedPrefixListAssociationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetManagedPrefixListAssociationsResponse
mkGetManagedPrefixListAssociationsResponse responseStatus =
  GetManagedPrefixListAssociationsResponse'
    { nextToken =
        Core.Nothing,
      prefixListAssociations = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplarrsNextToken :: Lens.Lens' GetManagedPrefixListAssociationsResponse (Core.Maybe Types.NextToken)
gmplarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gmplarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the associations.
--
-- /Note:/ Consider using 'prefixListAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplarrsPrefixListAssociations :: Lens.Lens' GetManagedPrefixListAssociationsResponse (Core.Maybe [Types.PrefixListAssociation])
gmplarrsPrefixListAssociations = Lens.field @"prefixListAssociations"
{-# DEPRECATED gmplarrsPrefixListAssociations "Use generic-lens or generic-optics with 'prefixListAssociations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplarrsResponseStatus :: Lens.Lens' GetManagedPrefixListAssociationsResponse Core.Int
gmplarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmplarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
