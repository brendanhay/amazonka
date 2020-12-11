{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.ListElasticsearchVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all supported Elasticsearch versions
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.ListElasticsearchVersions
  ( -- * Creating a request
    ListElasticsearchVersions (..),
    mkListElasticsearchVersions,

    -- ** Request lenses
    levNextToken,
    levMaxResults,

    -- * Destructuring the response
    ListElasticsearchVersionsResponse (..),
    mkListElasticsearchVersionsResponse,

    -- ** Response lenses
    levrsNextToken,
    levrsElasticsearchVersions,
    levrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'ListElasticsearchVersions' @ operation. Use @'MaxResults' @ to control the maximum number of results to retrieve in a single call.
--
-- Use @'NextToken' @ in response to retrieve more results. If the received response does not contain a NextToken, then there are no more results to retrieve.
--
--
-- /See:/ 'mkListElasticsearchVersions' smart constructor.
data ListElasticsearchVersions = ListElasticsearchVersions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListElasticsearchVersions' with the minimum fields required to make a request.
--
-- * 'maxResults' - Set this value to limit the number of results returned. Value provided must be greater than 10 else it wont be honored.
-- * 'nextToken' - Undocumented field.
mkListElasticsearchVersions ::
  ListElasticsearchVersions
mkListElasticsearchVersions =
  ListElasticsearchVersions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
levNextToken :: Lens.Lens' ListElasticsearchVersions (Lude.Maybe Lude.Text)
levNextToken = Lens.lens (nextToken :: ListElasticsearchVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListElasticsearchVersions)
{-# DEPRECATED levNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Set this value to limit the number of results returned. Value provided must be greater than 10 else it wont be honored.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
levMaxResults :: Lens.Lens' ListElasticsearchVersions (Lude.Maybe Lude.Int)
levMaxResults = Lens.lens (maxResults :: ListElasticsearchVersions -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListElasticsearchVersions)
{-# DEPRECATED levMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListElasticsearchVersions where
  page rq rs
    | Page.stop (rs Lens.^. levrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. levrsElasticsearchVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& levNextToken Lens..~ rs Lens.^. levrsNextToken

instance Lude.AWSRequest ListElasticsearchVersions where
  type
    Rs ListElasticsearchVersions =
      ListElasticsearchVersionsResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListElasticsearchVersionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ElasticsearchVersions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListElasticsearchVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListElasticsearchVersions where
  toPath = Lude.const "/2015-01-01/es/versions"

instance Lude.ToQuery ListElasticsearchVersions where
  toQuery ListElasticsearchVersions' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Container for the parameters for response received from @'ListElasticsearchVersions' @ operation.
--
-- /See:/ 'mkListElasticsearchVersionsResponse' smart constructor.
data ListElasticsearchVersionsResponse = ListElasticsearchVersionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    elasticsearchVersions ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListElasticsearchVersionsResponse' with the minimum fields required to make a request.
--
-- * 'elasticsearchVersions' - Undocumented field.
-- * 'nextToken' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkListElasticsearchVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListElasticsearchVersionsResponse
mkListElasticsearchVersionsResponse pResponseStatus_ =
  ListElasticsearchVersionsResponse'
    { nextToken = Lude.Nothing,
      elasticsearchVersions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
levrsNextToken :: Lens.Lens' ListElasticsearchVersionsResponse (Lude.Maybe Lude.Text)
levrsNextToken = Lens.lens (nextToken :: ListElasticsearchVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListElasticsearchVersionsResponse)
{-# DEPRECATED levrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'elasticsearchVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
levrsElasticsearchVersions :: Lens.Lens' ListElasticsearchVersionsResponse (Lude.Maybe [Lude.Text])
levrsElasticsearchVersions = Lens.lens (elasticsearchVersions :: ListElasticsearchVersionsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {elasticsearchVersions = a} :: ListElasticsearchVersionsResponse)
{-# DEPRECATED levrsElasticsearchVersions "Use generic-lens or generic-optics with 'elasticsearchVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
levrsResponseStatus :: Lens.Lens' ListElasticsearchVersionsResponse Lude.Int
levrsResponseStatus = Lens.lens (responseStatus :: ListElasticsearchVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListElasticsearchVersionsResponse)
{-# DEPRECATED levrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
