{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListAPIKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the API keys for a given API.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListAPIKeys
  ( -- * Creating a request
    ListAPIKeys (..),
    mkListAPIKeys,

    -- ** Request lenses
    lakNextToken,
    lakMaxResults,
    lakApiId,

    -- * Destructuring the response
    ListAPIKeysResponse (..),
    mkListAPIKeysResponse,

    -- ** Response lenses
    lakrsApiKeys,
    lakrsNextToken,
    lakrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAPIKeys' smart constructor.
data ListAPIKeys = ListAPIKeys'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    apiId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAPIKeys' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'maxResults' - The maximum number of results you want the request to return.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListAPIKeys ::
  -- | 'apiId'
  Lude.Text ->
  ListAPIKeys
mkListAPIKeys pApiId_ =
  ListAPIKeys'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      apiId = pApiId_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakNextToken :: Lens.Lens' ListAPIKeys (Lude.Maybe Lude.Text)
lakNextToken = Lens.lens (nextToken :: ListAPIKeys -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAPIKeys)
{-# DEPRECATED lakNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakMaxResults :: Lens.Lens' ListAPIKeys (Lude.Maybe Lude.Natural)
lakMaxResults = Lens.lens (maxResults :: ListAPIKeys -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAPIKeys)
{-# DEPRECATED lakMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakApiId :: Lens.Lens' ListAPIKeys Lude.Text
lakApiId = Lens.lens (apiId :: ListAPIKeys -> Lude.Text) (\s a -> s {apiId = a} :: ListAPIKeys)
{-# DEPRECATED lakApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

instance Page.AWSPager ListAPIKeys where
  page rq rs
    | Page.stop (rs Lens.^. lakrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lakrsApiKeys) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lakNextToken Lens..~ rs Lens.^. lakrsNextToken

instance Lude.AWSRequest ListAPIKeys where
  type Rs ListAPIKeys = ListAPIKeysResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAPIKeysResponse'
            Lude.<$> (x Lude..?> "apiKeys" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAPIKeys where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListAPIKeys where
  toPath ListAPIKeys' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/apikeys"]

instance Lude.ToQuery ListAPIKeys where
  toQuery ListAPIKeys' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListAPIKeysResponse' smart constructor.
data ListAPIKeysResponse = ListAPIKeysResponse'
  { apiKeys ::
      Lude.Maybe [APIKey],
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

-- | Creates a value of 'ListAPIKeysResponse' with the minimum fields required to make a request.
--
-- * 'apiKeys' - The @ApiKey@ objects.
-- * 'nextToken' - An identifier to be passed in the next request to this operation to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
mkListAPIKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAPIKeysResponse
mkListAPIKeysResponse pResponseStatus_ =
  ListAPIKeysResponse'
    { apiKeys = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ApiKey@ objects.
--
-- /Note:/ Consider using 'apiKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrsApiKeys :: Lens.Lens' ListAPIKeysResponse (Lude.Maybe [APIKey])
lakrsApiKeys = Lens.lens (apiKeys :: ListAPIKeysResponse -> Lude.Maybe [APIKey]) (\s a -> s {apiKeys = a} :: ListAPIKeysResponse)
{-# DEPRECATED lakrsApiKeys "Use generic-lens or generic-optics with 'apiKeys' instead." #-}

-- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrsNextToken :: Lens.Lens' ListAPIKeysResponse (Lude.Maybe Lude.Text)
lakrsNextToken = Lens.lens (nextToken :: ListAPIKeysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAPIKeysResponse)
{-# DEPRECATED lakrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrsResponseStatus :: Lens.Lens' ListAPIKeysResponse Lude.Int
lakrsResponseStatus = Lens.lens (responseStatus :: ListAPIKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAPIKeysResponse)
{-# DEPRECATED lakrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
