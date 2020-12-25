{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAccessKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the access key IDs associated with the specified IAM user. If there is none, the operation returns an empty list.
--
-- Although each user is limited to a small number of keys, you can still paginate the results using the @MaxItems@ and @Marker@ parameters.
-- If the @UserName@ field is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAccessKeys
  ( -- * Creating a request
    ListAccessKeys (..),
    mkListAccessKeys,

    -- ** Request lenses
    lakMarker,
    lakMaxItems,
    lakUserName,

    -- * Destructuring the response
    ListAccessKeysResponse (..),
    mkListAccessKeysResponse,

    -- ** Response lenses
    lakrrsAccessKeyMetadata,
    lakrrsIsTruncated,
    lakrrsMarker,
    lakrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAccessKeys' smart constructor.
data ListAccessKeys = ListAccessKeys'
  { -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Core.Maybe Types.MarkerType,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Core.Maybe Core.Natural,
    -- | The name of the user.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Core.Maybe Types.UserName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAccessKeys' value with any optional fields omitted.
mkListAccessKeys ::
  ListAccessKeys
mkListAccessKeys =
  ListAccessKeys'
    { marker = Core.Nothing,
      maxItems = Core.Nothing,
      userName = Core.Nothing
    }

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakMarker :: Lens.Lens' ListAccessKeys (Core.Maybe Types.MarkerType)
lakMarker = Lens.field @"marker"
{-# DEPRECATED lakMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakMaxItems :: Lens.Lens' ListAccessKeys (Core.Maybe Core.Natural)
lakMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lakMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The name of the user.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakUserName :: Lens.Lens' ListAccessKeys (Core.Maybe Types.UserName)
lakUserName = Lens.field @"userName"
{-# DEPRECATED lakUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest ListAccessKeys where
  type Rs ListAccessKeys = ListAccessKeysResponse
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
            ( Core.pure ("Action", "ListAccessKeys")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
                Core.<> (Core.toQueryValue "UserName" Core.<$> userName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListAccessKeysResult"
      ( \s h x ->
          ListAccessKeysResponse'
            Core.<$> ( x Core..@? "AccessKeyMetadata" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAccessKeys where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the response to a successful 'ListAccessKeys' request.
--
-- /See:/ 'mkListAccessKeysResponse' smart constructor.
data ListAccessKeysResponse = ListAccessKeysResponse'
  { -- | A list of objects containing metadata about the access keys.
    accessKeyMetadata :: [Types.AccessKeyMetadata],
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Core.Maybe Types.ResponseMarkerType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAccessKeysResponse' value with any optional fields omitted.
mkListAccessKeysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAccessKeysResponse
mkListAccessKeysResponse responseStatus =
  ListAccessKeysResponse'
    { accessKeyMetadata = Core.mempty,
      isTruncated = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of objects containing metadata about the access keys.
--
-- /Note:/ Consider using 'accessKeyMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrrsAccessKeyMetadata :: Lens.Lens' ListAccessKeysResponse [Types.AccessKeyMetadata]
lakrrsAccessKeyMetadata = Lens.field @"accessKeyMetadata"
{-# DEPRECATED lakrrsAccessKeyMetadata "Use generic-lens or generic-optics with 'accessKeyMetadata' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrrsIsTruncated :: Lens.Lens' ListAccessKeysResponse (Core.Maybe Core.Bool)
lakrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lakrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrrsMarker :: Lens.Lens' ListAccessKeysResponse (Core.Maybe Types.ResponseMarkerType)
lakrrsMarker = Lens.field @"marker"
{-# DEPRECATED lakrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrrsResponseStatus :: Lens.Lens' ListAccessKeysResponse Core.Int
lakrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lakrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
