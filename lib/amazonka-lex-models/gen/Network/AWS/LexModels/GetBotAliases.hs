{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetBotAliases (..),
    mkGetBotAliases,

    -- ** Request lenses
    gbaNameContains,
    gbaNextToken,
    gbaMaxResults,
    gbaBotName,

    -- * Destructuring the response
    GetBotAliasesResponse (..),
    mkGetBotAliasesResponse,

    -- ** Response lenses
    gbarsNextToken,
    gbarsBotAliases,
    gbarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBotAliases' smart constructor.
data GetBotAliases = GetBotAliases'
  { nameContains ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    botName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBotAliases' with the minimum fields required to make a request.
--
-- * 'botName' - The name of the bot.
-- * 'maxResults' - The maximum number of aliases to return in the response. The default is 50. .
-- * 'nameContains' - Substring to match in bot alias names. An alias will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
-- * 'nextToken' - A pagination token for fetching the next page of aliases. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of aliases, specify the pagination token in the next request.
mkGetBotAliases ::
  -- | 'botName'
  Lude.Text ->
  GetBotAliases
mkGetBotAliases pBotName_ =
  GetBotAliases'
    { nameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      botName = pBotName_
    }

-- | Substring to match in bot alias names. An alias will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbaNameContains :: Lens.Lens' GetBotAliases (Lude.Maybe Lude.Text)
gbaNameContains = Lens.lens (nameContains :: GetBotAliases -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: GetBotAliases)
{-# DEPRECATED gbaNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A pagination token for fetching the next page of aliases. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of aliases, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbaNextToken :: Lens.Lens' GetBotAliases (Lude.Maybe Lude.Text)
gbaNextToken = Lens.lens (nextToken :: GetBotAliases -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetBotAliases)
{-# DEPRECATED gbaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of aliases to return in the response. The default is 50. .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbaMaxResults :: Lens.Lens' GetBotAliases (Lude.Maybe Lude.Natural)
gbaMaxResults = Lens.lens (maxResults :: GetBotAliases -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetBotAliases)
{-# DEPRECATED gbaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbaBotName :: Lens.Lens' GetBotAliases Lude.Text
gbaBotName = Lens.lens (botName :: GetBotAliases -> Lude.Text) (\s a -> s {botName = a} :: GetBotAliases)
{-# DEPRECATED gbaBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

instance Page.AWSPager GetBotAliases where
  page rq rs
    | Page.stop (rs Lens.^. gbarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gbarsBotAliases) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gbaNextToken Lens..~ rs Lens.^. gbarsNextToken

instance Lude.AWSRequest GetBotAliases where
  type Rs GetBotAliases = GetBotAliasesResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBotAliasesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "BotAliases" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBotAliases where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetBotAliases where
  toPath GetBotAliases' {..} =
    Lude.mconcat ["/bots/", Lude.toBS botName, "/aliases/"]

instance Lude.ToQuery GetBotAliases where
  toQuery GetBotAliases' {..} =
    Lude.mconcat
      [ "nameContains" Lude.=: nameContains,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkGetBotAliasesResponse' smart constructor.
data GetBotAliasesResponse = GetBotAliasesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    botAliases :: Lude.Maybe [BotAliasMetadata],
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

-- | Creates a value of 'GetBotAliasesResponse' with the minimum fields required to make a request.
--
-- * 'botAliases' - An array of @BotAliasMetadata@ objects, each describing a bot alias.
-- * 'nextToken' - A pagination token for fetching next page of aliases. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of aliases, specify the pagination token in the next request.
-- * 'responseStatus' - The response status code.
mkGetBotAliasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBotAliasesResponse
mkGetBotAliasesResponse pResponseStatus_ =
  GetBotAliasesResponse'
    { nextToken = Lude.Nothing,
      botAliases = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A pagination token for fetching next page of aliases. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of aliases, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarsNextToken :: Lens.Lens' GetBotAliasesResponse (Lude.Maybe Lude.Text)
gbarsNextToken = Lens.lens (nextToken :: GetBotAliasesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetBotAliasesResponse)
{-# DEPRECATED gbarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @BotAliasMetadata@ objects, each describing a bot alias.
--
-- /Note:/ Consider using 'botAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarsBotAliases :: Lens.Lens' GetBotAliasesResponse (Lude.Maybe [BotAliasMetadata])
gbarsBotAliases = Lens.lens (botAliases :: GetBotAliasesResponse -> Lude.Maybe [BotAliasMetadata]) (\s a -> s {botAliases = a} :: GetBotAliasesResponse)
{-# DEPRECATED gbarsBotAliases "Use generic-lens or generic-optics with 'botAliases' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarsResponseStatus :: Lens.Lens' GetBotAliasesResponse Lude.Int
gbarsResponseStatus = Lens.lens (responseStatus :: GetBotAliasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBotAliasesResponse)
{-# DEPRECATED gbarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
