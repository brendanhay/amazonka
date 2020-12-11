{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetIntents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns intent information as follows:
--
--
--     * If you specify the @nameContains@ field, returns the @> LATEST@ version of all intents that contain the specified string.
--
--
--     * If you don't specify the @nameContains@ field, returns information about the @> LATEST@ version of all intents.
--
--
-- The operation requires permission for the @lex:GetIntents@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetIntents
  ( -- * Creating a request
    GetIntents (..),
    mkGetIntents,

    -- ** Request lenses
    giNameContains,
    giNextToken,
    giMaxResults,

    -- * Destructuring the response
    GetIntentsResponse (..),
    mkGetIntentsResponse,

    -- ** Response lenses
    gisrsIntents,
    gisrsNextToken,
    gisrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetIntents' smart constructor.
data GetIntents = GetIntents'
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

-- | Creates a value of 'GetIntents' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of intents to return in the response. The default is 10.
-- * 'nameContains' - Substring to match in intent names. An intent will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
-- * 'nextToken' - A pagination token that fetches the next page of intents. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, specify the pagination token in the next request.
mkGetIntents ::
  GetIntents
mkGetIntents =
  GetIntents'
    { nameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Substring to match in intent names. An intent will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giNameContains :: Lens.Lens' GetIntents (Lude.Maybe Lude.Text)
giNameContains = Lens.lens (nameContains :: GetIntents -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: GetIntents)
{-# DEPRECATED giNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A pagination token that fetches the next page of intents. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giNextToken :: Lens.Lens' GetIntents (Lude.Maybe Lude.Text)
giNextToken = Lens.lens (nextToken :: GetIntents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetIntents)
{-# DEPRECATED giNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of intents to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giMaxResults :: Lens.Lens' GetIntents (Lude.Maybe Lude.Natural)
giMaxResults = Lens.lens (maxResults :: GetIntents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetIntents)
{-# DEPRECATED giMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetIntents where
  page rq rs
    | Page.stop (rs Lens.^. gisrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gisrsIntents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& giNextToken Lens..~ rs Lens.^. gisrsNextToken

instance Lude.AWSRequest GetIntents where
  type Rs GetIntents = GetIntentsResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetIntentsResponse'
            Lude.<$> (x Lude..?> "intents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetIntents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetIntents where
  toPath = Lude.const "/intents/"

instance Lude.ToQuery GetIntents where
  toQuery GetIntents' {..} =
    Lude.mconcat
      [ "nameContains" Lude.=: nameContains,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkGetIntentsResponse' smart constructor.
data GetIntentsResponse = GetIntentsResponse'
  { intents ::
      Lude.Maybe [IntentMetadata],
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

-- | Creates a value of 'GetIntentsResponse' with the minimum fields required to make a request.
--
-- * 'intents' - An array of @Intent@ objects. For more information, see 'PutBot' .
-- * 'nextToken' - If the response is truncated, the response includes a pagination token that you can specify in your next request to fetch the next page of intents.
-- * 'responseStatus' - The response status code.
mkGetIntentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIntentsResponse
mkGetIntentsResponse pResponseStatus_ =
  GetIntentsResponse'
    { intents = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @Intent@ objects. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsIntents :: Lens.Lens' GetIntentsResponse (Lude.Maybe [IntentMetadata])
gisrsIntents = Lens.lens (intents :: GetIntentsResponse -> Lude.Maybe [IntentMetadata]) (\s a -> s {intents = a} :: GetIntentsResponse)
{-# DEPRECATED gisrsIntents "Use generic-lens or generic-optics with 'intents' instead." #-}

-- | If the response is truncated, the response includes a pagination token that you can specify in your next request to fetch the next page of intents.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsNextToken :: Lens.Lens' GetIntentsResponse (Lude.Maybe Lude.Text)
gisrsNextToken = Lens.lens (nextToken :: GetIntentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetIntentsResponse)
{-# DEPRECATED gisrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsResponseStatus :: Lens.Lens' GetIntentsResponse Lude.Int
gisrsResponseStatus = Lens.lens (responseStatus :: GetIntentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIntentsResponse)
{-# DEPRECATED gisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
