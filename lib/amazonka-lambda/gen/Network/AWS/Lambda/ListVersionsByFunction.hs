{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListVersionsByFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html versions> , with the version-specific configuration of each. Lambda returns up to 50 versions per call.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListVersionsByFunction
  ( -- * Creating a request
    ListVersionsByFunction (..),
    mkListVersionsByFunction,

    -- ** Request lenses
    lvbfFunctionName,
    lvbfMarker,
    lvbfMaxItems,

    -- * Destructuring the response
    ListVersionsByFunctionResponse (..),
    mkListVersionsByFunctionResponse,

    -- ** Response lenses
    lvbfrrsNextMarker,
    lvbfrrsVersions,
    lvbfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListVersionsByFunction' smart constructor.
data ListVersionsByFunction = ListVersionsByFunction'
  { -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @MyFunction@ .
    --
    --
    --     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
    --
    --
    --     * __Partial ARN__ - @123456789012:function:MyFunction@ .
    --
    --
    -- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
    functionName :: Types.NamespacedFunctionName,
    -- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of versions to return.
    maxItems :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVersionsByFunction' value with any optional fields omitted.
mkListVersionsByFunction ::
  -- | 'functionName'
  Types.NamespacedFunctionName ->
  ListVersionsByFunction
mkListVersionsByFunction functionName =
  ListVersionsByFunction'
    { functionName,
      marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfFunctionName :: Lens.Lens' ListVersionsByFunction Types.NamespacedFunctionName
lvbfFunctionName = Lens.field @"functionName"
{-# DEPRECATED lvbfFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfMarker :: Lens.Lens' ListVersionsByFunction (Core.Maybe Types.String)
lvbfMarker = Lens.field @"marker"
{-# DEPRECATED lvbfMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of versions to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfMaxItems :: Lens.Lens' ListVersionsByFunction (Core.Maybe Core.Natural)
lvbfMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lvbfMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListVersionsByFunction where
  type Rs ListVersionsByFunction = ListVersionsByFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2015-03-31/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/versions")
            ),
        Core._rqQuery =
          Core.toQueryValue "Marker" Core.<$> marker
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVersionsByFunctionResponse'
            Core.<$> (x Core..:? "NextMarker")
            Core.<*> (x Core..:? "Versions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListVersionsByFunction where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListVersionsByFunctionResponse' smart constructor.
data ListVersionsByFunctionResponse = ListVersionsByFunctionResponse'
  { -- | The pagination token that's included if more results are available.
    nextMarker :: Core.Maybe Types.String,
    -- | A list of Lambda function versions.
    versions :: Core.Maybe [Types.FunctionConfiguration],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVersionsByFunctionResponse' value with any optional fields omitted.
mkListVersionsByFunctionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListVersionsByFunctionResponse
mkListVersionsByFunctionResponse responseStatus =
  ListVersionsByFunctionResponse'
    { nextMarker = Core.Nothing,
      versions = Core.Nothing,
      responseStatus
    }

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfrrsNextMarker :: Lens.Lens' ListVersionsByFunctionResponse (Core.Maybe Types.String)
lvbfrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lvbfrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A list of Lambda function versions.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfrrsVersions :: Lens.Lens' ListVersionsByFunctionResponse (Core.Maybe [Types.FunctionConfiguration])
lvbfrrsVersions = Lens.field @"versions"
{-# DEPRECATED lvbfrrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfrrsResponseStatus :: Lens.Lens' ListVersionsByFunctionResponse Core.Int
lvbfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lvbfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
