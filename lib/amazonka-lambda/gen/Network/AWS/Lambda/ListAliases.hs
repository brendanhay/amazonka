{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html aliases> for a Lambda function.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListAliases
    (
    -- * Creating a request
      ListAliases (..)
    , mkListAliases
    -- ** Request lenses
    , laFunctionName
    , laFunctionVersion
    , laMarker
    , laMaxItems

    -- * Destructuring the response
    , ListAliasesResponse (..)
    , mkListAliasesResponse
    -- ** Response lenses
    , larrsAliases
    , larrsNextMarker
    , larrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAliases' smart constructor.
data ListAliases = ListAliases'
  { functionName :: Types.FunctionName
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
  , functionVersion :: Core.Maybe Types.Version
    -- ^ Specify a function version to only list aliases that invoke that version.
  , marker :: Core.Maybe Core.Text
    -- ^ Specify the pagination token that's returned by a previous request to retrieve the next page of results.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ Limit the number of aliases returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAliases' value with any optional fields omitted.
mkListAliases
    :: Types.FunctionName -- ^ 'functionName'
    -> ListAliases
mkListAliases functionName
  = ListAliases'{functionName, functionVersion = Core.Nothing,
                 marker = Core.Nothing, maxItems = Core.Nothing}

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
laFunctionName :: Lens.Lens' ListAliases Types.FunctionName
laFunctionName = Lens.field @"functionName"
{-# INLINEABLE laFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | Specify a function version to only list aliases that invoke that version.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laFunctionVersion :: Lens.Lens' ListAliases (Core.Maybe Types.Version)
laFunctionVersion = Lens.field @"functionVersion"
{-# INLINEABLE laFunctionVersion #-}
{-# DEPRECATED functionVersion "Use generic-lens or generic-optics with 'functionVersion' instead"  #-}

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMarker :: Lens.Lens' ListAliases (Core.Maybe Core.Text)
laMarker = Lens.field @"marker"
{-# INLINEABLE laMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Limit the number of aliases returned.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxItems :: Lens.Lens' ListAliases (Core.Maybe Core.Natural)
laMaxItems = Lens.field @"maxItems"
{-# INLINEABLE laMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListAliases where
        toQuery ListAliases{..}
          = Core.maybe Core.mempty (Core.toQueryPair "FunctionVersion")
              functionVersion
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListAliases where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListAliases where
        type Rs ListAliases = ListAliasesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2015-03-31/functions/" Core.<> Core.toText functionName Core.<>
                             "/aliases",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAliasesResponse' Core.<$>
                   (x Core..:? "Aliases") Core.<*> x Core..:? "NextMarker" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAliases where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"aliases" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { aliases :: Core.Maybe [Types.AliasConfiguration]
    -- ^ A list of aliases.
  , nextMarker :: Core.Maybe Core.Text
    -- ^ The pagination token that's included if more results are available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAliasesResponse' value with any optional fields omitted.
mkListAliasesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAliasesResponse
mkListAliasesResponse responseStatus
  = ListAliasesResponse'{aliases = Core.Nothing,
                         nextMarker = Core.Nothing, responseStatus}

-- | A list of aliases.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsAliases :: Lens.Lens' ListAliasesResponse (Core.Maybe [Types.AliasConfiguration])
larrsAliases = Lens.field @"aliases"
{-# INLINEABLE larrsAliases #-}
{-# DEPRECATED aliases "Use generic-lens or generic-optics with 'aliases' instead"  #-}

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextMarker :: Lens.Lens' ListAliasesResponse (Core.Maybe Core.Text)
larrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE larrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListAliasesResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE larrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
