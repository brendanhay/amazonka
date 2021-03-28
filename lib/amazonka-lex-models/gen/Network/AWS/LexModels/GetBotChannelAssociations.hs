{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBotChannelAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all of the channels associated with the specified bot. 
--
-- The @GetBotChannelAssociations@ operation requires permissions for the @lex:GetBotChannelAssociations@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBotChannelAssociations
    (
    -- * Creating a request
      GetBotChannelAssociations (..)
    , mkGetBotChannelAssociations
    -- ** Request lenses
    , gbcaBotName
    , gbcaBotAlias
    , gbcaMaxResults
    , gbcaNameContains
    , gbcaNextToken

    -- * Destructuring the response
    , GetBotChannelAssociationsResponse (..)
    , mkGetBotChannelAssociationsResponse
    -- ** Response lenses
    , gbcarfrsBotChannelAssociations
    , gbcarfrsNextToken
    , gbcarfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBotChannelAssociations' smart constructor.
data GetBotChannelAssociations = GetBotChannelAssociations'
  { botName :: Types.BotName
    -- ^ The name of the Amazon Lex bot in the association.
  , botAlias :: Types.BotAlias
    -- ^ An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of associations to return in the response. The default is 50. 
  , nameContains :: Core.Maybe Types.NameContains
    -- ^ Substring to match in channel association names. An association will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz." To return all bot channel associations, use a hyphen ("-") as the @nameContains@ parameter.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token for fetching the next page of associations. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of associations, specify the pagination token in the next request. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBotChannelAssociations' value with any optional fields omitted.
mkGetBotChannelAssociations
    :: Types.BotName -- ^ 'botName'
    -> Types.BotAlias -- ^ 'botAlias'
    -> GetBotChannelAssociations
mkGetBotChannelAssociations botName botAlias
  = GetBotChannelAssociations'{botName, botAlias,
                               maxResults = Core.Nothing, nameContains = Core.Nothing,
                               nextToken = Core.Nothing}

-- | The name of the Amazon Lex bot in the association.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaBotName :: Lens.Lens' GetBotChannelAssociations Types.BotName
gbcaBotName = Lens.field @"botName"
{-# INLINEABLE gbcaBotName #-}
{-# DEPRECATED botName "Use generic-lens or generic-optics with 'botName' instead"  #-}

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaBotAlias :: Lens.Lens' GetBotChannelAssociations Types.BotAlias
gbcaBotAlias = Lens.field @"botAlias"
{-# INLINEABLE gbcaBotAlias #-}
{-# DEPRECATED botAlias "Use generic-lens or generic-optics with 'botAlias' instead"  #-}

-- | The maximum number of associations to return in the response. The default is 50. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaMaxResults :: Lens.Lens' GetBotChannelAssociations (Core.Maybe Core.Natural)
gbcaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gbcaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Substring to match in channel association names. An association will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz." To return all bot channel associations, use a hyphen ("-") as the @nameContains@ parameter.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaNameContains :: Lens.Lens' GetBotChannelAssociations (Core.Maybe Types.NameContains)
gbcaNameContains = Lens.field @"nameContains"
{-# INLINEABLE gbcaNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | A pagination token for fetching the next page of associations. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of associations, specify the pagination token in the next request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaNextToken :: Lens.Lens' GetBotChannelAssociations (Core.Maybe Types.NextToken)
gbcaNextToken = Lens.field @"nextToken"
{-# INLINEABLE gbcaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetBotChannelAssociations where
        toQuery GetBotChannelAssociations{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nameContains")
                nameContains
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders GetBotChannelAssociations where
        toHeaders GetBotChannelAssociations{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetBotChannelAssociations where
        type Rs GetBotChannelAssociations =
             GetBotChannelAssociationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/bots/" Core.<> Core.toText botName Core.<> "/aliases/" Core.<>
                             Core.toText botAlias
                             Core.<> "/channels/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetBotChannelAssociationsResponse' Core.<$>
                   (x Core..:? "botChannelAssociations") Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetBotChannelAssociations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"botChannelAssociations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetBotChannelAssociationsResponse' smart constructor.
data GetBotChannelAssociationsResponse = GetBotChannelAssociationsResponse'
  { botChannelAssociations :: Core.Maybe [Types.BotChannelAssociation]
    -- ^ An array of objects, one for each association, that provides information about the Amazon Lex bot and its association with the channel. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token that fetches the next page of associations. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of associations, specify the pagination token in the next request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetBotChannelAssociationsResponse' value with any optional fields omitted.
mkGetBotChannelAssociationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBotChannelAssociationsResponse
mkGetBotChannelAssociationsResponse responseStatus
  = GetBotChannelAssociationsResponse'{botChannelAssociations =
                                         Core.Nothing,
                                       nextToken = Core.Nothing, responseStatus}

-- | An array of objects, one for each association, that provides information about the Amazon Lex bot and its association with the channel. 
--
-- /Note:/ Consider using 'botChannelAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarfrsBotChannelAssociations :: Lens.Lens' GetBotChannelAssociationsResponse (Core.Maybe [Types.BotChannelAssociation])
gbcarfrsBotChannelAssociations = Lens.field @"botChannelAssociations"
{-# INLINEABLE gbcarfrsBotChannelAssociations #-}
{-# DEPRECATED botChannelAssociations "Use generic-lens or generic-optics with 'botChannelAssociations' instead"  #-}

-- | A pagination token that fetches the next page of associations. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of associations, specify the pagination token in the next request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarfrsNextToken :: Lens.Lens' GetBotChannelAssociationsResponse (Core.Maybe Types.NextToken)
gbcarfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gbcarfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarfrsResponseStatus :: Lens.Lens' GetBotChannelAssociationsResponse Core.Int
gbcarfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbcarfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
