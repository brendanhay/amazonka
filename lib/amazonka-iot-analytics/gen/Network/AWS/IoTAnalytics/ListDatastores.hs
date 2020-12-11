{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.ListDatastores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of data stores.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListDatastores
  ( -- * Creating a request
    ListDatastores (..),
    mkListDatastores,

    -- ** Request lenses
    ldNextToken,
    ldMaxResults,

    -- * Destructuring the response
    ListDatastoresResponse (..),
    mkListDatastoresResponse,

    -- ** Response lenses
    ldrsNextToken,
    ldrsDatastoreSummaries,
    ldrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDatastores' smart constructor.
data ListDatastores = ListDatastores'
  { nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListDatastores' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return in this request.
--
-- The default value is 100.
-- * 'nextToken' - The token for the next set of results.
mkListDatastores ::
  ListDatastores
mkListDatastores =
  ListDatastores'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDatastores (Lude.Maybe Lude.Text)
ldNextToken = Lens.lens (nextToken :: ListDatastores -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDatastores)
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDatastores (Lude.Maybe Lude.Natural)
ldMaxResults = Lens.lens (maxResults :: ListDatastores -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDatastores)
{-# DEPRECATED ldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDatastores where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDatastoreSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldNextToken Lens..~ rs Lens.^. ldrsNextToken

instance Lude.AWSRequest ListDatastores where
  type Rs ListDatastores = ListDatastoresResponse
  request = Req.get ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDatastoresResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "datastoreSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDatastores where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDatastores where
  toPath = Lude.const "/datastores"

instance Lude.ToQuery ListDatastores where
  toQuery ListDatastores' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListDatastoresResponse' smart constructor.
data ListDatastoresResponse = ListDatastoresResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    datastoreSummaries ::
      Lude.Maybe [DatastoreSummary],
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

-- | Creates a value of 'ListDatastoresResponse' with the minimum fields required to make a request.
--
-- * 'datastoreSummaries' - A list of @DatastoreSummary@ objects.
-- * 'nextToken' - The token to retrieve the next set of results, or @null@ if there are no more results.
-- * 'responseStatus' - The response status code.
mkListDatastoresResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDatastoresResponse
mkListDatastoresResponse pResponseStatus_ =
  ListDatastoresResponse'
    { nextToken = Lude.Nothing,
      datastoreSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextToken :: Lens.Lens' ListDatastoresResponse (Lude.Maybe Lude.Text)
ldrsNextToken = Lens.lens (nextToken :: ListDatastoresResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDatastoresResponse)
{-# DEPRECATED ldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @DatastoreSummary@ objects.
--
-- /Note:/ Consider using 'datastoreSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDatastoreSummaries :: Lens.Lens' ListDatastoresResponse (Lude.Maybe [DatastoreSummary])
ldrsDatastoreSummaries = Lens.lens (datastoreSummaries :: ListDatastoresResponse -> Lude.Maybe [DatastoreSummary]) (\s a -> s {datastoreSummaries = a} :: ListDatastoresResponse)
{-# DEPRECATED ldrsDatastoreSummaries "Use generic-lens or generic-optics with 'datastoreSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDatastoresResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDatastoresResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDatastoresResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
