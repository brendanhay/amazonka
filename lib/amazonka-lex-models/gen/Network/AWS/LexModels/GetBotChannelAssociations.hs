{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gbcaNameContains,
    gbcaNextToken,
    gbcaMaxResults,
    gbcaBotName,
    gbcaBotAlias,

    -- * Destructuring the response
    GetBotChannelAssociationsResponse (..),
    mkGetBotChannelAssociationsResponse,

    -- ** Response lenses
    gbcasrsBotChannelAssociations,
    gbcasrsNextToken,
    gbcasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBotChannelAssociations' smart constructor.
data GetBotChannelAssociations = GetBotChannelAssociations'
  { nameContains ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    botName :: Lude.Text,
    botAlias :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBotChannelAssociations' with the minimum fields required to make a request.
--
-- * 'botAlias' - An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
-- * 'botName' - The name of the Amazon Lex bot in the association.
-- * 'maxResults' - The maximum number of associations to return in the response. The default is 50.
-- * 'nameContains' - Substring to match in channel association names. An association will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz." To return all bot channel associations, use a hyphen ("-") as the @nameContains@ parameter.
-- * 'nextToken' - A pagination token for fetching the next page of associations. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of associations, specify the pagination token in the next request.
mkGetBotChannelAssociations ::
  -- | 'botName'
  Lude.Text ->
  -- | 'botAlias'
  Lude.Text ->
  GetBotChannelAssociations
mkGetBotChannelAssociations pBotName_ pBotAlias_ =
  GetBotChannelAssociations'
    { nameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      botName = pBotName_,
      botAlias = pBotAlias_
    }

-- | Substring to match in channel association names. An association will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz." To return all bot channel associations, use a hyphen ("-") as the @nameContains@ parameter.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaNameContains :: Lens.Lens' GetBotChannelAssociations (Lude.Maybe Lude.Text)
gbcaNameContains = Lens.lens (nameContains :: GetBotChannelAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: GetBotChannelAssociations)
{-# DEPRECATED gbcaNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A pagination token for fetching the next page of associations. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of associations, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaNextToken :: Lens.Lens' GetBotChannelAssociations (Lude.Maybe Lude.Text)
gbcaNextToken = Lens.lens (nextToken :: GetBotChannelAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetBotChannelAssociations)
{-# DEPRECATED gbcaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of associations to return in the response. The default is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaMaxResults :: Lens.Lens' GetBotChannelAssociations (Lude.Maybe Lude.Natural)
gbcaMaxResults = Lens.lens (maxResults :: GetBotChannelAssociations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetBotChannelAssociations)
{-# DEPRECATED gbcaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the Amazon Lex bot in the association.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaBotName :: Lens.Lens' GetBotChannelAssociations Lude.Text
gbcaBotName = Lens.lens (botName :: GetBotChannelAssociations -> Lude.Text) (\s a -> s {botName = a} :: GetBotChannelAssociations)
{-# DEPRECATED gbcaBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcaBotAlias :: Lens.Lens' GetBotChannelAssociations Lude.Text
gbcaBotAlias = Lens.lens (botAlias :: GetBotChannelAssociations -> Lude.Text) (\s a -> s {botAlias = a} :: GetBotChannelAssociations)
{-# DEPRECATED gbcaBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

instance Page.AWSPager GetBotChannelAssociations where
  page rq rs
    | Page.stop (rs Lens.^. gbcasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gbcasrsBotChannelAssociations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gbcaNextToken Lens..~ rs Lens.^. gbcasrsNextToken

instance Lude.AWSRequest GetBotChannelAssociations where
  type
    Rs GetBotChannelAssociations =
      GetBotChannelAssociationsResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBotChannelAssociationsResponse'
            Lude.<$> (x Lude..?> "botChannelAssociations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBotChannelAssociations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetBotChannelAssociations where
  toPath GetBotChannelAssociations' {..} =
    Lude.mconcat
      [ "/bots/",
        Lude.toBS botName,
        "/aliases/",
        Lude.toBS botAlias,
        "/channels/"
      ]

instance Lude.ToQuery GetBotChannelAssociations where
  toQuery GetBotChannelAssociations' {..} =
    Lude.mconcat
      [ "nameContains" Lude.=: nameContains,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkGetBotChannelAssociationsResponse' smart constructor.
data GetBotChannelAssociationsResponse = GetBotChannelAssociationsResponse'
  { botChannelAssociations ::
      Lude.Maybe
        [BotChannelAssociation],
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBotChannelAssociationsResponse' with the minimum fields required to make a request.
--
-- * 'botChannelAssociations' - An array of objects, one for each association, that provides information about the Amazon Lex bot and its association with the channel.
-- * 'nextToken' - A pagination token that fetches the next page of associations. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of associations, specify the pagination token in the next request.
-- * 'responseStatus' - The response status code.
mkGetBotChannelAssociationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBotChannelAssociationsResponse
mkGetBotChannelAssociationsResponse pResponseStatus_ =
  GetBotChannelAssociationsResponse'
    { botChannelAssociations =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects, one for each association, that provides information about the Amazon Lex bot and its association with the channel.
--
-- /Note:/ Consider using 'botChannelAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcasrsBotChannelAssociations :: Lens.Lens' GetBotChannelAssociationsResponse (Lude.Maybe [BotChannelAssociation])
gbcasrsBotChannelAssociations = Lens.lens (botChannelAssociations :: GetBotChannelAssociationsResponse -> Lude.Maybe [BotChannelAssociation]) (\s a -> s {botChannelAssociations = a} :: GetBotChannelAssociationsResponse)
{-# DEPRECATED gbcasrsBotChannelAssociations "Use generic-lens or generic-optics with 'botChannelAssociations' instead." #-}

-- | A pagination token that fetches the next page of associations. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of associations, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcasrsNextToken :: Lens.Lens' GetBotChannelAssociationsResponse (Lude.Maybe Lude.Text)
gbcasrsNextToken = Lens.lens (nextToken :: GetBotChannelAssociationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetBotChannelAssociationsResponse)
{-# DEPRECATED gbcasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcasrsResponseStatus :: Lens.Lens' GetBotChannelAssociationsResponse Lude.Int
gbcasrsResponseStatus = Lens.lens (responseStatus :: GetBotChannelAssociationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBotChannelAssociationsResponse)
{-# DEPRECATED gbcasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
