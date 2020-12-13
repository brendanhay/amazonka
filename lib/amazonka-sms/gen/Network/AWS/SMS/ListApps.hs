{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.ListApps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves summaries for all applications.
--
-- This operation returns paginated results.
module Network.AWS.SMS.ListApps
  ( -- * Creating a request
    ListApps (..),
    mkListApps,

    -- ** Request lenses
    laAppIds,
    laNextToken,
    laMaxResults,

    -- * Destructuring the response
    ListAppsResponse (..),
    mkListAppsResponse,

    -- ** Response lenses
    larsApps,
    larsNextToken,
    larsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkListApps' smart constructor.
data ListApps = ListApps'
  { -- | The unique application IDs.
    appIds :: Lude.Maybe [Lude.Text],
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return in a single call. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApps' with the minimum fields required to make a request.
--
-- * 'appIds' - The unique application IDs.
-- * 'nextToken' - The token for the next set of results.
-- * 'maxResults' - The maximum number of results to return in a single call. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
mkListApps ::
  ListApps
mkListApps =
  ListApps'
    { appIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The unique application IDs.
--
-- /Note:/ Consider using 'appIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAppIds :: Lens.Lens' ListApps (Lude.Maybe [Lude.Text])
laAppIds = Lens.lens (appIds :: ListApps -> Lude.Maybe [Lude.Text]) (\s a -> s {appIds = a} :: ListApps)
{-# DEPRECATED laAppIds "Use generic-lens or generic-optics with 'appIds' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListApps (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListApps -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApps)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListApps (Lude.Maybe Lude.Int)
laMaxResults = Lens.lens (maxResults :: ListApps -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListApps)
{-# DEPRECATED laMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListApps where
  page rq rs
    | Page.stop (rs Lens.^. larsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. larsApps) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laNextToken Lens..~ rs Lens.^. larsNextToken

instance Lude.AWSRequest ListApps where
  type Rs ListApps = ListAppsResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAppsResponse'
            Lude.<$> (x Lude..?> "apps" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListApps where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.ListApps" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListApps where
  toJSON ListApps' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("appIds" Lude..=) Lude.<$> appIds,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListApps where
  toPath = Lude.const "/"

instance Lude.ToQuery ListApps where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { -- | The application summaries.
    apps :: Lude.Maybe [AppSummary],
    -- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAppsResponse' with the minimum fields required to make a request.
--
-- * 'apps' - The application summaries.
-- * 'nextToken' - The token required to retrieve the next set of results. This value is null when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkListAppsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAppsResponse
mkListAppsResponse pResponseStatus_ =
  ListAppsResponse'
    { apps = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The application summaries.
--
-- /Note:/ Consider using 'apps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsApps :: Lens.Lens' ListAppsResponse (Lude.Maybe [AppSummary])
larsApps = Lens.lens (apps :: ListAppsResponse -> Lude.Maybe [AppSummary]) (\s a -> s {apps = a} :: ListAppsResponse)
{-# DEPRECATED larsApps "Use generic-lens or generic-optics with 'apps' instead." #-}

-- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextToken :: Lens.Lens' ListAppsResponse (Lude.Maybe Lude.Text)
larsNextToken = Lens.lens (nextToken :: ListAppsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAppsResponse)
{-# DEPRECATED larsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListAppsResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListAppsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAppsResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
