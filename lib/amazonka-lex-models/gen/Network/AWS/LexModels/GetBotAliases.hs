{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBotAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of aliases for a specified Amazon Lex bot.
--
-- This operation requires permissions for the @lex:GetBotAliases@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBotAliases
    (
    -- * Creating a request
      GetBotAliases (..)
    , mkGetBotAliases
    -- ** Request lenses
    , gbaBotName
    , gbaMaxResults
    , gbaNameContains
    , gbaNextToken

    -- * Destructuring the response
    , GetBotAliasesResponse (..)
    , mkGetBotAliasesResponse
    -- ** Response lenses
    , gbarrsBotAliases
    , gbarrsNextToken
    , gbarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBotAliases' smart constructor.
data GetBotAliases = GetBotAliases'
  { botName :: Types.BotName
    -- ^ The name of the bot.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of aliases to return in the response. The default is 50. . 
  , nameContains :: Core.Maybe Types.AliasName
    -- ^ Substring to match in bot alias names. An alias will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token for fetching the next page of aliases. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of aliases, specify the pagination token in the next request. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBotAliases' value with any optional fields omitted.
mkGetBotAliases
    :: Types.BotName -- ^ 'botName'
    -> GetBotAliases
mkGetBotAliases botName
  = GetBotAliases'{botName, maxResults = Core.Nothing,
                   nameContains = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbaBotName :: Lens.Lens' GetBotAliases Types.BotName
gbaBotName = Lens.field @"botName"
{-# INLINEABLE gbaBotName #-}
{-# DEPRECATED botName "Use generic-lens or generic-optics with 'botName' instead"  #-}

-- | The maximum number of aliases to return in the response. The default is 50. . 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbaMaxResults :: Lens.Lens' GetBotAliases (Core.Maybe Core.Natural)
gbaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gbaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Substring to match in bot alias names. An alias will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbaNameContains :: Lens.Lens' GetBotAliases (Core.Maybe Types.AliasName)
gbaNameContains = Lens.field @"nameContains"
{-# INLINEABLE gbaNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | A pagination token for fetching the next page of aliases. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of aliases, specify the pagination token in the next request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbaNextToken :: Lens.Lens' GetBotAliases (Core.Maybe Types.NextToken)
gbaNextToken = Lens.field @"nextToken"
{-# INLINEABLE gbaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetBotAliases where
        toQuery GetBotAliases{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nameContains")
                nameContains
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders GetBotAliases where
        toHeaders GetBotAliases{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetBotAliases where
        type Rs GetBotAliases = GetBotAliasesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/bots/" Core.<> Core.toText botName Core.<> "/aliases/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetBotAliasesResponse' Core.<$>
                   (x Core..:? "BotAliases") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetBotAliases where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"botAliases" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetBotAliasesResponse' smart constructor.
data GetBotAliasesResponse = GetBotAliasesResponse'
  { botAliases :: Core.Maybe [Types.BotAliasMetadata]
    -- ^ An array of @BotAliasMetadata@ objects, each describing a bot alias.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token for fetching next page of aliases. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of aliases, specify the pagination token in the next request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetBotAliasesResponse' value with any optional fields omitted.
mkGetBotAliasesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBotAliasesResponse
mkGetBotAliasesResponse responseStatus
  = GetBotAliasesResponse'{botAliases = Core.Nothing,
                           nextToken = Core.Nothing, responseStatus}

-- | An array of @BotAliasMetadata@ objects, each describing a bot alias.
--
-- /Note:/ Consider using 'botAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarrsBotAliases :: Lens.Lens' GetBotAliasesResponse (Core.Maybe [Types.BotAliasMetadata])
gbarrsBotAliases = Lens.field @"botAliases"
{-# INLINEABLE gbarrsBotAliases #-}
{-# DEPRECATED botAliases "Use generic-lens or generic-optics with 'botAliases' instead"  #-}

-- | A pagination token for fetching next page of aliases. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of aliases, specify the pagination token in the next request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarrsNextToken :: Lens.Lens' GetBotAliasesResponse (Core.Maybe Types.NextToken)
gbarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gbarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarrsResponseStatus :: Lens.Lens' GetBotAliasesResponse Core.Int
gbarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
