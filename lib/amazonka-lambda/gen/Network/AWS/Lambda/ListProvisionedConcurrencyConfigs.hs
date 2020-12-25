{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListProvisionedConcurrencyConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of provisioned concurrency configurations for a function.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListProvisionedConcurrencyConfigs
  ( -- * Creating a request
    ListProvisionedConcurrencyConfigs (..),
    mkListProvisionedConcurrencyConfigs,

    -- ** Request lenses
    lpccFunctionName,
    lpccMarker,
    lpccMaxItems,

    -- * Destructuring the response
    ListProvisionedConcurrencyConfigsResponse (..),
    mkListProvisionedConcurrencyConfigsResponse,

    -- ** Response lenses
    lpccrrsNextMarker,
    lpccrrsProvisionedConcurrencyConfigs,
    lpccrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListProvisionedConcurrencyConfigs' smart constructor.
data ListProvisionedConcurrencyConfigs = ListProvisionedConcurrencyConfigs'
  { -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @my-function@ .
    --
    --
    --     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
    --
    --
    --     * __Partial ARN__ - @123456789012:function:my-function@ .
    --
    --
    -- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
    functionName :: Types.FunctionName,
    -- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
    marker :: Core.Maybe Types.Marker,
    -- | Specify a number to limit the number of configurations returned.
    maxItems :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProvisionedConcurrencyConfigs' value with any optional fields omitted.
mkListProvisionedConcurrencyConfigs ::
  -- | 'functionName'
  Types.FunctionName ->
  ListProvisionedConcurrencyConfigs
mkListProvisionedConcurrencyConfigs functionName =
  ListProvisionedConcurrencyConfigs'
    { functionName,
      marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpccFunctionName :: Lens.Lens' ListProvisionedConcurrencyConfigs Types.FunctionName
lpccFunctionName = Lens.field @"functionName"
{-# DEPRECATED lpccFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpccMarker :: Lens.Lens' ListProvisionedConcurrencyConfigs (Core.Maybe Types.Marker)
lpccMarker = Lens.field @"marker"
{-# DEPRECATED lpccMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specify a number to limit the number of configurations returned.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpccMaxItems :: Lens.Lens' ListProvisionedConcurrencyConfigs (Core.Maybe Core.Natural)
lpccMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lpccMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListProvisionedConcurrencyConfigs where
  type
    Rs ListProvisionedConcurrencyConfigs =
      ListProvisionedConcurrencyConfigsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2019-09-30/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/provisioned-concurrency")
            ),
        Core._rqQuery =
          Core.toQueryValue "Marker" Core.<$> marker
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
            Core.<> (Core.pure ("List=ALL", "")),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisionedConcurrencyConfigsResponse'
            Core.<$> (x Core..:? "NextMarker")
            Core.<*> (x Core..:? "ProvisionedConcurrencyConfigs")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListProvisionedConcurrencyConfigs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"provisionedConcurrencyConfigs" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListProvisionedConcurrencyConfigsResponse' smart constructor.
data ListProvisionedConcurrencyConfigsResponse = ListProvisionedConcurrencyConfigsResponse'
  { -- | The pagination token that's included if more results are available.
    nextMarker :: Core.Maybe Types.String,
    -- | A list of provisioned concurrency configurations.
    provisionedConcurrencyConfigs :: Core.Maybe [Types.ProvisionedConcurrencyConfigListItem],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProvisionedConcurrencyConfigsResponse' value with any optional fields omitted.
mkListProvisionedConcurrencyConfigsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListProvisionedConcurrencyConfigsResponse
mkListProvisionedConcurrencyConfigsResponse responseStatus =
  ListProvisionedConcurrencyConfigsResponse'
    { nextMarker =
        Core.Nothing,
      provisionedConcurrencyConfigs = Core.Nothing,
      responseStatus
    }

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpccrrsNextMarker :: Lens.Lens' ListProvisionedConcurrencyConfigsResponse (Core.Maybe Types.String)
lpccrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lpccrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A list of provisioned concurrency configurations.
--
-- /Note:/ Consider using 'provisionedConcurrencyConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpccrrsProvisionedConcurrencyConfigs :: Lens.Lens' ListProvisionedConcurrencyConfigsResponse (Core.Maybe [Types.ProvisionedConcurrencyConfigListItem])
lpccrrsProvisionedConcurrencyConfigs = Lens.field @"provisionedConcurrencyConfigs"
{-# DEPRECATED lpccrrsProvisionedConcurrencyConfigs "Use generic-lens or generic-optics with 'provisionedConcurrencyConfigs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpccrrsResponseStatus :: Lens.Lens' ListProvisionedConcurrencyConfigsResponse Core.Int
lpccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
