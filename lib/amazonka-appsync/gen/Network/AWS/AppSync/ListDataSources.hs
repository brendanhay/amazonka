{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListDataSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the data sources for a given API.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListDataSources
  ( -- * Creating a request
    ListDataSources (..),
    mkListDataSources,

    -- ** Request lenses
    ldsNextToken,
    ldsMaxResults,
    ldsApiId,

    -- * Destructuring the response
    ListDataSourcesResponse (..),
    mkListDataSourcesResponse,

    -- ** Response lenses
    ldsrsDataSources,
    ldsrsNextToken,
    ldsrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDataSources' smart constructor.
data ListDataSources = ListDataSources'
  { nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListDataSources' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'maxResults' - The maximum number of results you want the request to return.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListDataSources ::
  -- | 'apiId'
  Lude.Text ->
  ListDataSources
mkListDataSources pApiId_ =
  ListDataSources'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      apiId = pApiId_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsNextToken :: Lens.Lens' ListDataSources (Lude.Maybe Lude.Text)
ldsNextToken = Lens.lens (nextToken :: ListDataSources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDataSources)
{-# DEPRECATED ldsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsMaxResults :: Lens.Lens' ListDataSources (Lude.Maybe Lude.Natural)
ldsMaxResults = Lens.lens (maxResults :: ListDataSources -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDataSources)
{-# DEPRECATED ldsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsApiId :: Lens.Lens' ListDataSources Lude.Text
ldsApiId = Lens.lens (apiId :: ListDataSources -> Lude.Text) (\s a -> s {apiId = a} :: ListDataSources)
{-# DEPRECATED ldsApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

instance Page.AWSPager ListDataSources where
  page rq rs
    | Page.stop (rs Lens.^. ldsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldsrsDataSources) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldsNextToken Lens..~ rs Lens.^. ldsrsNextToken

instance Lude.AWSRequest ListDataSources where
  type Rs ListDataSources = ListDataSourcesResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDataSourcesResponse'
            Lude.<$> (x Lude..?> "dataSources" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDataSources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListDataSources where
  toPath ListDataSources' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/datasources"]

instance Lude.ToQuery ListDataSources where
  toQuery ListDataSources' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListDataSourcesResponse' smart constructor.
data ListDataSourcesResponse = ListDataSourcesResponse'
  { dataSources ::
      Lude.Maybe [DataSource],
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

-- | Creates a value of 'ListDataSourcesResponse' with the minimum fields required to make a request.
--
-- * 'dataSources' - The @DataSource@ objects.
-- * 'nextToken' - An identifier to be passed in the next request to this operation to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
mkListDataSourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDataSourcesResponse
mkListDataSourcesResponse pResponseStatus_ =
  ListDataSourcesResponse'
    { dataSources = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @DataSource@ objects.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsrsDataSources :: Lens.Lens' ListDataSourcesResponse (Lude.Maybe [DataSource])
ldsrsDataSources = Lens.lens (dataSources :: ListDataSourcesResponse -> Lude.Maybe [DataSource]) (\s a -> s {dataSources = a} :: ListDataSourcesResponse)
{-# DEPRECATED ldsrsDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsrsNextToken :: Lens.Lens' ListDataSourcesResponse (Lude.Maybe Lude.Text)
ldsrsNextToken = Lens.lens (nextToken :: ListDataSourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDataSourcesResponse)
{-# DEPRECATED ldsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsrsResponseStatus :: Lens.Lens' ListDataSourcesResponse Lude.Int
ldsrsResponseStatus = Lens.lens (responseStatus :: ListDataSourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDataSourcesResponse)
{-# DEPRECATED ldsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
