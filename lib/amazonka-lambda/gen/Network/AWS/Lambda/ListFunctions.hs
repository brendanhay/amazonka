{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListFunctions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Lambda functions, with the version-specific configuration of each. Lambda returns up to 50 functions per call.
--
-- Set @FunctionVersion@ to @ALL@ to include all published versions of each function in addition to the unpublished version. To get more information about a function or version, use 'GetFunction' .
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListFunctions
  ( -- * Creating a request
    ListFunctions (..),
    mkListFunctions,

    -- ** Request lenses
    lfFunctionVersion,
    lfMarker,
    lfMasterRegion,
    lfMaxItems,

    -- * Destructuring the response
    ListFunctionsResponse (..),
    mkListFunctionsResponse,

    -- ** Response lenses
    lfrrsFunctions,
    lfrrsNextMarker,
    lfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFunctions' smart constructor.
data ListFunctions = ListFunctions'
  { -- | Set to @ALL@ to include entries for all published versions of each function.
    functionVersion :: Core.Maybe Types.FunctionVersion,
    -- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
    marker :: Core.Maybe Types.String,
    -- | For Lambda@Edge functions, the AWS Region of the master function. For example, @us-east-1@ filters the list of functions to only include Lambda@Edge functions replicated from a master function in US East (N. Virginia). If specified, you must set @FunctionVersion@ to @ALL@ .
    masterRegion :: Core.Maybe Types.MasterRegion,
    -- | The maximum number of functions to return.
    maxItems :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctions' value with any optional fields omitted.
mkListFunctions ::
  ListFunctions
mkListFunctions =
  ListFunctions'
    { functionVersion = Core.Nothing,
      marker = Core.Nothing,
      masterRegion = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | Set to @ALL@ to include entries for all published versions of each function.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfFunctionVersion :: Lens.Lens' ListFunctions (Core.Maybe Types.FunctionVersion)
lfFunctionVersion = Lens.field @"functionVersion"
{-# DEPRECATED lfFunctionVersion "Use generic-lens or generic-optics with 'functionVersion' instead." #-}

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMarker :: Lens.Lens' ListFunctions (Core.Maybe Types.String)
lfMarker = Lens.field @"marker"
{-# DEPRECATED lfMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | For Lambda@Edge functions, the AWS Region of the master function. For example, @us-east-1@ filters the list of functions to only include Lambda@Edge functions replicated from a master function in US East (N. Virginia). If specified, you must set @FunctionVersion@ to @ALL@ .
--
-- /Note:/ Consider using 'masterRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMasterRegion :: Lens.Lens' ListFunctions (Core.Maybe Types.MasterRegion)
lfMasterRegion = Lens.field @"masterRegion"
{-# DEPRECATED lfMasterRegion "Use generic-lens or generic-optics with 'masterRegion' instead." #-}

-- | The maximum number of functions to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMaxItems :: Lens.Lens' ListFunctions (Core.Maybe Core.Natural)
lfMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lfMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListFunctions where
  type Rs ListFunctions = ListFunctionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2015-03-31/functions/",
        Core._rqQuery =
          Core.toQueryValue "FunctionVersion" Core.<$> functionVersion
            Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
            Core.<> (Core.toQueryValue "MasterRegion" Core.<$> masterRegion)
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionsResponse'
            Core.<$> (x Core..:? "Functions")
            Core.<*> (x Core..:? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListFunctions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"functions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | A list of Lambda functions.
--
-- /See:/ 'mkListFunctionsResponse' smart constructor.
data ListFunctionsResponse = ListFunctionsResponse'
  { -- | A list of Lambda functions.
    functions :: Core.Maybe [Types.FunctionConfiguration],
    -- | The pagination token that's included if more results are available.
    nextMarker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctionsResponse' value with any optional fields omitted.
mkListFunctionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListFunctionsResponse
mkListFunctionsResponse responseStatus =
  ListFunctionsResponse'
    { functions = Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | A list of Lambda functions.
--
-- /Note:/ Consider using 'functions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsFunctions :: Lens.Lens' ListFunctionsResponse (Core.Maybe [Types.FunctionConfiguration])
lfrrsFunctions = Lens.field @"functions"
{-# DEPRECATED lfrrsFunctions "Use generic-lens or generic-optics with 'functions' instead." #-}

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsNextMarker :: Lens.Lens' ListFunctionsResponse (Core.Maybe Types.String)
lfrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lfrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsResponseStatus :: Lens.Lens' ListFunctionsResponse Core.Int
lfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
