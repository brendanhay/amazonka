{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetBots (..),
    mkGetBots,

    -- ** Request lenses
    gbNameContains,
    gbNextToken,
    gbMaxResults,

    -- * Destructuring the response
    GetBotsResponse (..),
    mkGetBotsResponse,

    -- ** Response lenses
    gbsrsBots,
    gbsrsNextToken,
    gbsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBots' smart constructor.
data GetBots = GetBots'
  { nameContains :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBots' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of bots to return in the response that the request will return. The default is 10.
-- * 'nameContains' - Substring to match in bot names. A bot will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
-- * 'nextToken' - A pagination token that fetches the next page of bots. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of bots, specify the pagination token in the next request.
mkGetBots ::
  GetBots
mkGetBots =
  GetBots'
    { nameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Substring to match in bot names. A bot will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbNameContains :: Lens.Lens' GetBots (Lude.Maybe Lude.Text)
gbNameContains = Lens.lens (nameContains :: GetBots -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: GetBots)
{-# DEPRECATED gbNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A pagination token that fetches the next page of bots. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of bots, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbNextToken :: Lens.Lens' GetBots (Lude.Maybe Lude.Text)
gbNextToken = Lens.lens (nextToken :: GetBots -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetBots)
{-# DEPRECATED gbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of bots to return in the response that the request will return. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbMaxResults :: Lens.Lens' GetBots (Lude.Maybe Lude.Natural)
gbMaxResults = Lens.lens (maxResults :: GetBots -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetBots)
{-# DEPRECATED gbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetBots where
  page rq rs
    | Page.stop (rs Lens.^. gbsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gbsrsBots) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gbNextToken Lens..~ rs Lens.^. gbsrsNextToken

instance Lude.AWSRequest GetBots where
  type Rs GetBots = GetBotsResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBotsResponse'
            Lude.<$> (x Lude..?> "bots" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBots where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetBots where
  toPath = Lude.const "/bots/"

instance Lude.ToQuery GetBots where
  toQuery GetBots' {..} =
    Lude.mconcat
      [ "nameContains" Lude.=: nameContains,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkGetBotsResponse' smart constructor.
data GetBotsResponse = GetBotsResponse'
  { bots ::
      Lude.Maybe [BotMetadata],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBotsResponse' with the minimum fields required to make a request.
--
-- * 'bots' - An array of @botMetadata@ objects, with one entry for each bot.
-- * 'nextToken' - If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of bots.
-- * 'responseStatus' - The response status code.
mkGetBotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBotsResponse
mkGetBotsResponse pResponseStatus_ =
  GetBotsResponse'
    { bots = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @botMetadata@ objects, with one entry for each bot.
--
-- /Note:/ Consider using 'bots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbsrsBots :: Lens.Lens' GetBotsResponse (Lude.Maybe [BotMetadata])
gbsrsBots = Lens.lens (bots :: GetBotsResponse -> Lude.Maybe [BotMetadata]) (\s a -> s {bots = a} :: GetBotsResponse)
{-# DEPRECATED gbsrsBots "Use generic-lens or generic-optics with 'bots' instead." #-}

-- | If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of bots.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbsrsNextToken :: Lens.Lens' GetBotsResponse (Lude.Maybe Lude.Text)
gbsrsNextToken = Lens.lens (nextToken :: GetBotsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetBotsResponse)
{-# DEPRECATED gbsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbsrsResponseStatus :: Lens.Lens' GetBotsResponse Lude.Int
gbsrsResponseStatus = Lens.lens (responseStatus :: GetBotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBotsResponse)
{-# DEPRECATED gbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
