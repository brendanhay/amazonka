{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns bot information as follows: 
--
--
--     * If you provide the @nameContains@ field, the response includes information for the @> LATEST@ version of all bots whose name contains the specified string.
--
--
--     * If you don't specify the @nameContains@ field, the operation returns information about the @> LATEST@ version of all of your bots.
--
--
-- This operation requires permission for the @lex:GetBots@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBots
    (
    -- * Creating a request
      GetBots (..)
    , mkGetBots
    -- ** Request lenses
    , gbMaxResults
    , gbNameContains
    , gbNextToken

    -- * Destructuring the response
    , GetBotsResponse (..)
    , mkGetBotsResponse
    -- ** Response lenses
    , gbrfrsBots
    , gbrfrsNextToken
    , gbrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBots' smart constructor.
data GetBots = GetBots'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of bots to return in the response that the request will return. The default is 10.
  , nameContains :: Core.Maybe Types.NameContains
    -- ^ Substring to match in bot names. A bot will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token that fetches the next page of bots. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of bots, specify the pagination token in the next request. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBots' value with any optional fields omitted.
mkGetBots
    :: GetBots
mkGetBots
  = GetBots'{maxResults = Core.Nothing, nameContains = Core.Nothing,
             nextToken = Core.Nothing}

-- | The maximum number of bots to return in the response that the request will return. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbMaxResults :: Lens.Lens' GetBots (Core.Maybe Core.Natural)
gbMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gbMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Substring to match in bot names. A bot will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbNameContains :: Lens.Lens' GetBots (Core.Maybe Types.NameContains)
gbNameContains = Lens.field @"nameContains"
{-# INLINEABLE gbNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | A pagination token that fetches the next page of bots. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of bots, specify the pagination token in the next request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbNextToken :: Lens.Lens' GetBots (Core.Maybe Types.NextToken)
gbNextToken = Lens.field @"nextToken"
{-# INLINEABLE gbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetBots where
        toQuery GetBots{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nameContains")
                nameContains
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders GetBots where
        toHeaders GetBots{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetBots where
        type Rs GetBots = GetBotsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/bots/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetBotsResponse' Core.<$>
                   (x Core..:? "bots") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetBots where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"bots" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetBotsResponse' smart constructor.
data GetBotsResponse = GetBotsResponse'
  { bots :: Core.Maybe [Types.BotMetadata]
    -- ^ An array of @botMetadata@ objects, with one entry for each bot. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of bots. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetBotsResponse' value with any optional fields omitted.
mkGetBotsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBotsResponse
mkGetBotsResponse responseStatus
  = GetBotsResponse'{bots = Core.Nothing, nextToken = Core.Nothing,
                     responseStatus}

-- | An array of @botMetadata@ objects, with one entry for each bot. 
--
-- /Note:/ Consider using 'bots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrfrsBots :: Lens.Lens' GetBotsResponse (Core.Maybe [Types.BotMetadata])
gbrfrsBots = Lens.field @"bots"
{-# INLINEABLE gbrfrsBots #-}
{-# DEPRECATED bots "Use generic-lens or generic-optics with 'bots' instead"  #-}

-- | If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of bots. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrfrsNextToken :: Lens.Lens' GetBotsResponse (Core.Maybe Types.NextToken)
gbrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gbrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrfrsResponseStatus :: Lens.Lens' GetBotsResponse Core.Int
gbrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
