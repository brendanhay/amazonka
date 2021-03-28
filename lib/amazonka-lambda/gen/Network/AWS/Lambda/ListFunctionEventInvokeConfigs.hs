{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListFunctionEventInvokeConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configurations for asynchronous invocation for a function.
--
-- To configure options for asynchronous invocation, use 'PutFunctionEventInvokeConfig' .
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListFunctionEventInvokeConfigs
    (
    -- * Creating a request
      ListFunctionEventInvokeConfigs (..)
    , mkListFunctionEventInvokeConfigs
    -- ** Request lenses
    , lfeicFunctionName
    , lfeicMarker
    , lfeicMaxItems

    -- * Destructuring the response
    , ListFunctionEventInvokeConfigsResponse (..)
    , mkListFunctionEventInvokeConfigsResponse
    -- ** Response lenses
    , lfeicrrsFunctionEventInvokeConfigs
    , lfeicrrsNextMarker
    , lfeicrrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFunctionEventInvokeConfigs' smart constructor.
data ListFunctionEventInvokeConfigs = ListFunctionEventInvokeConfigs'
  { functionName :: Types.FunctionName
    -- ^ The name of the Lambda function.
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
  , marker :: Core.Maybe Core.Text
    -- ^ Specify the pagination token that's returned by a previous request to retrieve the next page of results.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ The maximum number of configurations to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctionEventInvokeConfigs' value with any optional fields omitted.
mkListFunctionEventInvokeConfigs
    :: Types.FunctionName -- ^ 'functionName'
    -> ListFunctionEventInvokeConfigs
mkListFunctionEventInvokeConfigs functionName
  = ListFunctionEventInvokeConfigs'{functionName,
                                    marker = Core.Nothing, maxItems = Core.Nothing}

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
lfeicFunctionName :: Lens.Lens' ListFunctionEventInvokeConfigs Types.FunctionName
lfeicFunctionName = Lens.field @"functionName"
{-# INLINEABLE lfeicFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfeicMarker :: Lens.Lens' ListFunctionEventInvokeConfigs (Core.Maybe Core.Text)
lfeicMarker = Lens.field @"marker"
{-# INLINEABLE lfeicMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of configurations to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfeicMaxItems :: Lens.Lens' ListFunctionEventInvokeConfigs (Core.Maybe Core.Natural)
lfeicMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lfeicMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListFunctionEventInvokeConfigs where
        toQuery ListFunctionEventInvokeConfigs{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListFunctionEventInvokeConfigs where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListFunctionEventInvokeConfigs where
        type Rs ListFunctionEventInvokeConfigs =
             ListFunctionEventInvokeConfigsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2019-09-25/functions/" Core.<> Core.toText functionName Core.<>
                             "/event-invoke-config/list",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListFunctionEventInvokeConfigsResponse' Core.<$>
                   (x Core..:? "FunctionEventInvokeConfigs") Core.<*>
                     x Core..:? "NextMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListFunctionEventInvokeConfigs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"functionEventInvokeConfigs" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListFunctionEventInvokeConfigsResponse' smart constructor.
data ListFunctionEventInvokeConfigsResponse = ListFunctionEventInvokeConfigsResponse'
  { functionEventInvokeConfigs :: Core.Maybe [Types.FunctionEventInvokeConfig]
    -- ^ A list of configurations.
  , nextMarker :: Core.Maybe Core.Text
    -- ^ The pagination token that's included if more results are available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListFunctionEventInvokeConfigsResponse' value with any optional fields omitted.
mkListFunctionEventInvokeConfigsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListFunctionEventInvokeConfigsResponse
mkListFunctionEventInvokeConfigsResponse responseStatus
  = ListFunctionEventInvokeConfigsResponse'{functionEventInvokeConfigs
                                              = Core.Nothing,
                                            nextMarker = Core.Nothing, responseStatus}

-- | A list of configurations.
--
-- /Note:/ Consider using 'functionEventInvokeConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfeicrrsFunctionEventInvokeConfigs :: Lens.Lens' ListFunctionEventInvokeConfigsResponse (Core.Maybe [Types.FunctionEventInvokeConfig])
lfeicrrsFunctionEventInvokeConfigs = Lens.field @"functionEventInvokeConfigs"
{-# INLINEABLE lfeicrrsFunctionEventInvokeConfigs #-}
{-# DEPRECATED functionEventInvokeConfigs "Use generic-lens or generic-optics with 'functionEventInvokeConfigs' instead"  #-}

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfeicrrsNextMarker :: Lens.Lens' ListFunctionEventInvokeConfigsResponse (Core.Maybe Core.Text)
lfeicrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lfeicrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfeicrrsResponseStatus :: Lens.Lens' ListFunctionEventInvokeConfigsResponse Core.Int
lfeicrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lfeicrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
