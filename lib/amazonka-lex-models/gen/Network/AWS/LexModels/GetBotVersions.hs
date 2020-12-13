{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBotVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions of a bot.
--
-- The @GetBotVersions@ operation returns a @BotMetadata@ object for each version of a bot. For example, if a bot has three numbered versions, the @GetBotVersions@ operation returns four @BotMetadata@ objects in the response, one for each numbered version and one for the @> LATEST@ version.
-- The @GetBotVersions@ operation always returns at least one version, the @> LATEST@ version.
-- This operation requires permissions for the @lex:GetBotVersions@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBotVersions
  ( -- * Creating a request
    GetBotVersions (..),
    mkGetBotVersions,

    -- ** Request lenses
    gbvNextToken,
    gbvName,
    gbvMaxResults,

    -- * Destructuring the response
    GetBotVersionsResponse (..),
    mkGetBotVersionsResponse,

    -- ** Response lenses
    gbvrsBots,
    gbvrsNextToken,
    gbvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBotVersions' smart constructor.
data GetBotVersions = GetBotVersions'
  { -- | A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the bot for which versions should be returned.
    name :: Lude.Text,
    -- | The maximum number of bot versions to return in the response. The default is 10.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBotVersions' with the minimum fields required to make a request.
--
-- * 'nextToken' - A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
-- * 'name' - The name of the bot for which versions should be returned.
-- * 'maxResults' - The maximum number of bot versions to return in the response. The default is 10.
mkGetBotVersions ::
  -- | 'name'
  Lude.Text ->
  GetBotVersions
mkGetBotVersions pName_ =
  GetBotVersions'
    { nextToken = Lude.Nothing,
      name = pName_,
      maxResults = Lude.Nothing
    }

-- | A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvNextToken :: Lens.Lens' GetBotVersions (Lude.Maybe Lude.Text)
gbvNextToken = Lens.lens (nextToken :: GetBotVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetBotVersions)
{-# DEPRECATED gbvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the bot for which versions should be returned.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvName :: Lens.Lens' GetBotVersions Lude.Text
gbvName = Lens.lens (name :: GetBotVersions -> Lude.Text) (\s a -> s {name = a} :: GetBotVersions)
{-# DEPRECATED gbvName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum number of bot versions to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvMaxResults :: Lens.Lens' GetBotVersions (Lude.Maybe Lude.Natural)
gbvMaxResults = Lens.lens (maxResults :: GetBotVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetBotVersions)
{-# DEPRECATED gbvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetBotVersions where
  page rq rs
    | Page.stop (rs Lens.^. gbvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gbvrsBots) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gbvNextToken Lens..~ rs Lens.^. gbvrsNextToken

instance Lude.AWSRequest GetBotVersions where
  type Rs GetBotVersions = GetBotVersionsResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBotVersionsResponse'
            Lude.<$> (x Lude..?> "bots" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBotVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetBotVersions where
  toPath GetBotVersions' {..} =
    Lude.mconcat ["/bots/", Lude.toBS name, "/versions/"]

instance Lude.ToQuery GetBotVersions where
  toQuery GetBotVersions' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkGetBotVersionsResponse' smart constructor.
data GetBotVersionsResponse = GetBotVersionsResponse'
  { -- | An array of @BotMetadata@ objects, one for each numbered version of the bot plus one for the @> LATEST@ version.
    bots :: Lude.Maybe [BotMetadata],
    -- | A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBotVersionsResponse' with the minimum fields required to make a request.
--
-- * 'bots' - An array of @BotMetadata@ objects, one for each numbered version of the bot plus one for the @> LATEST@ version.
-- * 'nextToken' - A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
-- * 'responseStatus' - The response status code.
mkGetBotVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBotVersionsResponse
mkGetBotVersionsResponse pResponseStatus_ =
  GetBotVersionsResponse'
    { bots = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @BotMetadata@ objects, one for each numbered version of the bot plus one for the @> LATEST@ version.
--
-- /Note:/ Consider using 'bots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvrsBots :: Lens.Lens' GetBotVersionsResponse (Lude.Maybe [BotMetadata])
gbvrsBots = Lens.lens (bots :: GetBotVersionsResponse -> Lude.Maybe [BotMetadata]) (\s a -> s {bots = a} :: GetBotVersionsResponse)
{-# DEPRECATED gbvrsBots "Use generic-lens or generic-optics with 'bots' instead." #-}

-- | A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvrsNextToken :: Lens.Lens' GetBotVersionsResponse (Lude.Maybe Lude.Text)
gbvrsNextToken = Lens.lens (nextToken :: GetBotVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetBotVersionsResponse)
{-# DEPRECATED gbvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvrsResponseStatus :: Lens.Lens' GetBotVersionsResponse Lude.Int
gbvrsResponseStatus = Lens.lens (responseStatus :: GetBotVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBotVersionsResponse)
{-# DEPRECATED gbvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
