{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetBotChannelAssociations (..),
    mkGetBotChannelAssociations,

    -- ** Request lenses
    gbcaBotName,
    gbcaBotAlias,
    gbcaMaxResults,
    gbcaNameContains,
    gbcaNextToken,

    -- * Destructuring the response
    GetBotChannelAssociationsResponse (..),
    mkGetBotChannelAssociationsResponse,

    -- ** Response lenses
    gbcarfrsBotChannelAssociations,
    gbcarfrsNextToken,
    gbcarfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBotChannelAssociations' smart constructor.
data GetBotChannelAssociations = GetBotChannelAssociations'
  { -- | The name of the Amazon Lex bot in the association.
    botName :: Types.BotName,
    -- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
    botAlias :: Types.BotAlias,
    -- | The maximum number of associations to return in the response. The default is 50.
    maxResults :: Core.Maybe Core.Natural,
    -- | Substring to match in channel association names. An association will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz." To return all bot channel associations, use a hyphen ("-") as the @nameContains@ parameter.
    nameContains :: Core.Maybe Types.NameContains,
    -- | A pagination token for fetching the next page of associations. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of associations, specify the pagination token in the next request.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBotChannelAssociations' value with any optional fields omitted.
mkGetBotChannelAssociations ::
  -- | 'botName'
  Types.BotName ->
  -- | 'botAlias'
  Types.BotAlias ->
  GetBotChannelAssociations
mkGetBotChannelAssociations botName botAlias =
  GetBotChannelAssociations'
    { botName,
      botAlias,
      maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the Amazon Lex bot in the association.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaBotName :: Lens.Lens' GetBotChannelAssociations Types.BotName
gbcaBotName = Lens.field @"botName"
{-# DEPRECATED gbcaBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaBotAlias :: Lens.Lens' GetBotChannelAssociations Types.BotAlias
gbcaBotAlias = Lens.field @"botAlias"
{-# DEPRECATED gbcaBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

-- | The maximum number of associations to return in the response. The default is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaMaxResults :: Lens.Lens' GetBotChannelAssociations (Core.Maybe Core.Natural)
gbcaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gbcaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Substring to match in channel association names. An association will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz." To return all bot channel associations, use a hyphen ("-") as the @nameContains@ parameter.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaNameContains :: Lens.Lens' GetBotChannelAssociations (Core.Maybe Types.NameContains)
gbcaNameContains = Lens.field @"nameContains"
{-# DEPRECATED gbcaNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A pagination token for fetching the next page of associations. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of associations, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaNextToken :: Lens.Lens' GetBotChannelAssociations (Core.Maybe Types.NextToken)
gbcaNextToken = Lens.field @"nextToken"
{-# DEPRECATED gbcaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetBotChannelAssociations where
  type
    Rs GetBotChannelAssociations =
      GetBotChannelAssociationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/bots/" Core.<> (Core.toText botName) Core.<> ("/aliases/")
                Core.<> (Core.toText botAlias)
                Core.<> ("/channels/")
            ),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nameContains" Core.<$> nameContains)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotChannelAssociationsResponse'
            Core.<$> (x Core..:? "botChannelAssociations")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetBotChannelAssociations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"botChannelAssociations" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetBotChannelAssociationsResponse' smart constructor.
data GetBotChannelAssociationsResponse = GetBotChannelAssociationsResponse'
  { -- | An array of objects, one for each association, that provides information about the Amazon Lex bot and its association with the channel.
    botChannelAssociations :: Core.Maybe [Types.BotChannelAssociation],
    -- | A pagination token that fetches the next page of associations. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of associations, specify the pagination token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetBotChannelAssociationsResponse' value with any optional fields omitted.
mkGetBotChannelAssociationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBotChannelAssociationsResponse
mkGetBotChannelAssociationsResponse responseStatus =
  GetBotChannelAssociationsResponse'
    { botChannelAssociations =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of objects, one for each association, that provides information about the Amazon Lex bot and its association with the channel.
--
-- /Note:/ Consider using 'botChannelAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarfrsBotChannelAssociations :: Lens.Lens' GetBotChannelAssociationsResponse (Core.Maybe [Types.BotChannelAssociation])
gbcarfrsBotChannelAssociations = Lens.field @"botChannelAssociations"
{-# DEPRECATED gbcarfrsBotChannelAssociations "Use generic-lens or generic-optics with 'botChannelAssociations' instead." #-}

-- | A pagination token that fetches the next page of associations. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of associations, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarfrsNextToken :: Lens.Lens' GetBotChannelAssociationsResponse (Core.Maybe Types.NextToken)
gbcarfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gbcarfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarfrsResponseStatus :: Lens.Lens' GetBotChannelAssociationsResponse Core.Int
gbcarfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbcarfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
