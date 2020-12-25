{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.ListOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of the operations that return an operation ID and that have ever been performed on domains that were registered by the current account.
--
-- This operation returns paginated results.
module Network.AWS.Route53Domains.ListOperations
  ( -- * Creating a request
    ListOperations (..),
    mkListOperations,

    -- ** Request lenses
    loMarker,
    loMaxItems,
    loSubmittedSince,

    -- * Destructuring the response
    ListOperationsResponse (..),
    mkListOperationsResponse,

    -- ** Response lenses
    lorrsOperations,
    lorrsNextPageMarker,
    lorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The ListOperations request includes the following elements.
--
-- /See:/ 'mkListOperations' smart constructor.
data ListOperations = ListOperations'
  { -- | For an initial request for a list of operations, omit this element. If the number of operations that are not yet complete is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional operations. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
    marker :: Core.Maybe Types.Marker,
    -- | Number of domains to be returned.
    --
    -- Default: 20
    maxItems :: Core.Maybe Core.Int,
    -- | An optional parameter that lets you get information about all the operations that you submitted after a specified date and time. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
    submittedSince :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListOperations' value with any optional fields omitted.
mkListOperations ::
  ListOperations
mkListOperations =
  ListOperations'
    { marker = Core.Nothing,
      maxItems = Core.Nothing,
      submittedSince = Core.Nothing
    }

-- | For an initial request for a list of operations, omit this element. If the number of operations that are not yet complete is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional operations. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMarker :: Lens.Lens' ListOperations (Core.Maybe Types.Marker)
loMarker = Lens.field @"marker"
{-# DEPRECATED loMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Number of domains to be returned.
--
-- Default: 20
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaxItems :: Lens.Lens' ListOperations (Core.Maybe Core.Int)
loMaxItems = Lens.field @"maxItems"
{-# DEPRECATED loMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | An optional parameter that lets you get information about all the operations that you submitted after a specified date and time. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'submittedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loSubmittedSince :: Lens.Lens' ListOperations (Core.Maybe Core.NominalDiffTime)
loSubmittedSince = Lens.field @"submittedSince"
{-# DEPRECATED loSubmittedSince "Use generic-lens or generic-optics with 'submittedSince' instead." #-}

instance Core.FromJSON ListOperations where
  toJSON ListOperations {..} =
    Core.object
      ( Core.catMaybes
          [ ("Marker" Core..=) Core.<$> marker,
            ("MaxItems" Core..=) Core.<$> maxItems,
            ("SubmittedSince" Core..=) Core.<$> submittedSince
          ]
      )

instance Core.AWSRequest ListOperations where
  type Rs ListOperations = ListOperationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53Domains_v20140515.ListOperations")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOperationsResponse'
            Core.<$> (x Core..:? "Operations" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextPageMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListOperations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageMarker") =
      Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"operations") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker"
            Lens..~ rs Lens.^. Lens.field @"nextPageMarker"
        )

-- | The ListOperations response includes the following elements.
--
-- /See:/ 'mkListOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
  { -- | Lists summaries of the operations.
    operations :: [Types.OperationSummary],
    -- | If there are more operations than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
    nextPageMarker :: Core.Maybe Types.NextPageMarker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListOperationsResponse' value with any optional fields omitted.
mkListOperationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListOperationsResponse
mkListOperationsResponse responseStatus =
  ListOperationsResponse'
    { operations = Core.mempty,
      nextPageMarker = Core.Nothing,
      responseStatus
    }

-- | Lists summaries of the operations.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsOperations :: Lens.Lens' ListOperationsResponse [Types.OperationSummary]
lorrsOperations = Lens.field @"operations"
{-# DEPRECATED lorrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | If there are more operations than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
--
-- /Note:/ Consider using 'nextPageMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsNextPageMarker :: Lens.Lens' ListOperationsResponse (Core.Maybe Types.NextPageMarker)
lorrsNextPageMarker = Lens.field @"nextPageMarker"
{-# DEPRECATED lorrsNextPageMarker "Use generic-lens or generic-optics with 'nextPageMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsResponseStatus :: Lens.Lens' ListOperationsResponse Core.Int
lorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
