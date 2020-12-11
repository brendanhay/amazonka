{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetIntentVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions of an intent.
--
-- The @GetIntentVersions@ operation returns an @IntentMetadata@ object for each version of an intent. For example, if an intent has three numbered versions, the @GetIntentVersions@ operation returns four @IntentMetadata@ objects in the response, one for each numbered version and one for the @> LATEST@ version.
-- The @GetIntentVersions@ operation always returns at least one version, the @> LATEST@ version.
-- This operation requires permissions for the @lex:GetIntentVersions@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetIntentVersions
  ( -- * Creating a request
    GetIntentVersions (..),
    mkGetIntentVersions,

    -- ** Request lenses
    givNextToken,
    givMaxResults,
    givName,

    -- * Destructuring the response
    GetIntentVersionsResponse (..),
    mkGetIntentVersionsResponse,

    -- ** Response lenses
    givrsIntents,
    givrsNextToken,
    givrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetIntentVersions' smart constructor.
data GetIntentVersions = GetIntentVersions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIntentVersions' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of intent versions to return in the response. The default is 10.
-- * 'name' - The name of the intent for which versions should be returned.
-- * 'nextToken' - A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
mkGetIntentVersions ::
  -- | 'name'
  Lude.Text ->
  GetIntentVersions
mkGetIntentVersions pName_ =
  GetIntentVersions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      name = pName_
    }

-- | A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givNextToken :: Lens.Lens' GetIntentVersions (Lude.Maybe Lude.Text)
givNextToken = Lens.lens (nextToken :: GetIntentVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetIntentVersions)
{-# DEPRECATED givNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of intent versions to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givMaxResults :: Lens.Lens' GetIntentVersions (Lude.Maybe Lude.Natural)
givMaxResults = Lens.lens (maxResults :: GetIntentVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetIntentVersions)
{-# DEPRECATED givMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the intent for which versions should be returned.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givName :: Lens.Lens' GetIntentVersions Lude.Text
givName = Lens.lens (name :: GetIntentVersions -> Lude.Text) (\s a -> s {name = a} :: GetIntentVersions)
{-# DEPRECATED givName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Page.AWSPager GetIntentVersions where
  page rq rs
    | Page.stop (rs Lens.^. givrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. givrsIntents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& givNextToken Lens..~ rs Lens.^. givrsNextToken

instance Lude.AWSRequest GetIntentVersions where
  type Rs GetIntentVersions = GetIntentVersionsResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetIntentVersionsResponse'
            Lude.<$> (x Lude..?> "intents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetIntentVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetIntentVersions where
  toPath GetIntentVersions' {..} =
    Lude.mconcat ["/intents/", Lude.toBS name, "/versions/"]

instance Lude.ToQuery GetIntentVersions where
  toQuery GetIntentVersions' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkGetIntentVersionsResponse' smart constructor.
data GetIntentVersionsResponse = GetIntentVersionsResponse'
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

-- | Creates a value of 'GetIntentVersionsResponse' with the minimum fields required to make a request.
--
-- * 'intents' - An array of @IntentMetadata@ objects, one for each numbered version of the intent plus one for the @> LATEST@ version.
-- * 'nextToken' - A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
-- * 'responseStatus' - The response status code.
mkGetIntentVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIntentVersionsResponse
mkGetIntentVersionsResponse pResponseStatus_ =
  GetIntentVersionsResponse'
    { intents = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @IntentMetadata@ objects, one for each numbered version of the intent plus one for the @> LATEST@ version.
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givrsIntents :: Lens.Lens' GetIntentVersionsResponse (Lude.Maybe [IntentMetadata])
givrsIntents = Lens.lens (intents :: GetIntentVersionsResponse -> Lude.Maybe [IntentMetadata]) (\s a -> s {intents = a} :: GetIntentVersionsResponse)
{-# DEPRECATED givrsIntents "Use generic-lens or generic-optics with 'intents' instead." #-}

-- | A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givrsNextToken :: Lens.Lens' GetIntentVersionsResponse (Lude.Maybe Lude.Text)
givrsNextToken = Lens.lens (nextToken :: GetIntentVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetIntentVersionsResponse)
{-# DEPRECATED givrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givrsResponseStatus :: Lens.Lens' GetIntentVersionsResponse Lude.Int
givrsResponseStatus = Lens.lens (responseStatus :: GetIntentVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIntentVersionsResponse)
{-# DEPRECATED givrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
