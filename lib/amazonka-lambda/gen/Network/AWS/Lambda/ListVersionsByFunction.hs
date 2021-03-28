{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListVersionsByFunction (..)
    , mkListVersionsByFunction
    -- ** Request lenses
    , lvbfFunctionName
    , lvbfMarker
    , lvbfMaxItems

    -- * Destructuring the response
    , ListVersionsByFunctionResponse (..)
    , mkListVersionsByFunctionResponse
    -- ** Response lenses
    , lvbfrrsNextMarker
    , lvbfrrsVersions
    , lvbfrrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListVersionsByFunction' smart constructor.
data ListVersionsByFunction = ListVersionsByFunction'
  { functionName :: Types.NamespacedFunctionName
    -- ^ The name of the Lambda function.
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
  , marker :: Core.Maybe Core.Text
    -- ^ Specify the pagination token that's returned by a previous request to retrieve the next page of results.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ The maximum number of versions to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVersionsByFunction' value with any optional fields omitted.
mkListVersionsByFunction
    :: Types.NamespacedFunctionName -- ^ 'functionName'
    -> ListVersionsByFunction
mkListVersionsByFunction functionName
  = ListVersionsByFunction'{functionName, marker = Core.Nothing,
                            maxItems = Core.Nothing}

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
{-# INLINEABLE lvbfFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfMarker :: Lens.Lens' ListVersionsByFunction (Core.Maybe Core.Text)
lvbfMarker = Lens.field @"marker"
{-# INLINEABLE lvbfMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of versions to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfMaxItems :: Lens.Lens' ListVersionsByFunction (Core.Maybe Core.Natural)
lvbfMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lvbfMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListVersionsByFunction where
        toQuery ListVersionsByFunction{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListVersionsByFunction where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListVersionsByFunction where
        type Rs ListVersionsByFunction = ListVersionsByFunctionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2015-03-31/functions/" Core.<> Core.toText functionName Core.<>
                             "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListVersionsByFunctionResponse' Core.<$>
                   (x Core..:? "NextMarker") Core.<*> x Core..:? "Versions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListVersionsByFunction where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListVersionsByFunctionResponse' smart constructor.
data ListVersionsByFunctionResponse = ListVersionsByFunctionResponse'
  { nextMarker :: Core.Maybe Core.Text
    -- ^ The pagination token that's included if more results are available.
  , versions :: Core.Maybe [Types.FunctionConfiguration]
    -- ^ A list of Lambda function versions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVersionsByFunctionResponse' value with any optional fields omitted.
mkListVersionsByFunctionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListVersionsByFunctionResponse
mkListVersionsByFunctionResponse responseStatus
  = ListVersionsByFunctionResponse'{nextMarker = Core.Nothing,
                                    versions = Core.Nothing, responseStatus}

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfrrsNextMarker :: Lens.Lens' ListVersionsByFunctionResponse (Core.Maybe Core.Text)
lvbfrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lvbfrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | A list of Lambda function versions.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfrrsVersions :: Lens.Lens' ListVersionsByFunctionResponse (Core.Maybe [Types.FunctionConfiguration])
lvbfrrsVersions = Lens.field @"versions"
{-# INLINEABLE lvbfrrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfrrsResponseStatus :: Lens.Lens' ListVersionsByFunctionResponse Core.Int
lvbfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lvbfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
