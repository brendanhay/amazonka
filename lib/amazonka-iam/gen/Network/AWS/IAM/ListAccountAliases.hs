{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAccountAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account alias associated with the AWS account (Note: you can have only one). For information about using an AWS account alias, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID> in the /IAM User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAccountAliases
  ( -- * Creating a request
    ListAccountAliases (..),
    mkListAccountAliases,

    -- ** Request lenses
    laaMarker,
    laaMaxItems,

    -- * Destructuring the response
    ListAccountAliasesResponse (..),
    mkListAccountAliasesResponse,

    -- ** Response lenses
    laarrsAccountAliases,
    laarrsIsTruncated,
    laarrsMarker,
    laarrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAccountAliases' smart constructor.
data ListAccountAliases = ListAccountAliases'
  { -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Core.Maybe Types.MarkerType,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAccountAliases' value with any optional fields omitted.
mkListAccountAliases ::
  ListAccountAliases
mkListAccountAliases =
  ListAccountAliases'
    { marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laaMarker :: Lens.Lens' ListAccountAliases (Core.Maybe Types.MarkerType)
laaMarker = Lens.field @"marker"
{-# DEPRECATED laaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laaMaxItems :: Lens.Lens' ListAccountAliases (Core.Maybe Core.Natural)
laaMaxItems = Lens.field @"maxItems"
{-# DEPRECATED laaMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListAccountAliases where
  type Rs ListAccountAliases = ListAccountAliasesResponse
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
            ( Core.pure ("Action", "ListAccountAliases")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListAccountAliasesResult"
      ( \s h x ->
          ListAccountAliasesResponse'
            Core.<$> ( x Core..@? "AccountAliases" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAccountAliases where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the response to a successful 'ListAccountAliases' request.
--
-- /See:/ 'mkListAccountAliasesResponse' smart constructor.
data ListAccountAliasesResponse = ListAccountAliasesResponse'
  { -- | A list of aliases associated with the account. AWS supports only one alias per account.
    accountAliases :: [Types.AccountAliasType],
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Core.Maybe Types.ResponseMarkerType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAccountAliasesResponse' value with any optional fields omitted.
mkListAccountAliasesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAccountAliasesResponse
mkListAccountAliasesResponse responseStatus =
  ListAccountAliasesResponse'
    { accountAliases = Core.mempty,
      isTruncated = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of aliases associated with the account. AWS supports only one alias per account.
--
-- /Note:/ Consider using 'accountAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laarrsAccountAliases :: Lens.Lens' ListAccountAliasesResponse [Types.AccountAliasType]
laarrsAccountAliases = Lens.field @"accountAliases"
{-# DEPRECATED laarrsAccountAliases "Use generic-lens or generic-optics with 'accountAliases' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laarrsIsTruncated :: Lens.Lens' ListAccountAliasesResponse (Core.Maybe Core.Bool)
laarrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED laarrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laarrsMarker :: Lens.Lens' ListAccountAliasesResponse (Core.Maybe Types.ResponseMarkerType)
laarrsMarker = Lens.field @"marker"
{-# DEPRECATED laarrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laarrsResponseStatus :: Lens.Lens' ListAccountAliasesResponse Core.Int
laarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED laarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
