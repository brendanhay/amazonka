{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListResourceDataSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your resource data sync configurations. Includes information about the last time a sync attempted to start, the last sync status, and the last time a sync successfully completed.
--
-- The number of sync configurations might be too large to return using a single call to @ListResourceDataSync@ . You can limit the number of sync configurations returned by using the @MaxResults@ parameter. To determine whether there are more sync configurations to list, check the value of @NextToken@ in the output. If there are more sync configurations to list, you can request them by specifying the @NextToken@ returned in the call to the parameter of a subsequent call.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListResourceDataSync
  ( -- * Creating a request
    ListResourceDataSync (..),
    mkListResourceDataSync,

    -- ** Request lenses
    lrdsSyncType,
    lrdsNextToken,
    lrdsMaxResults,

    -- * Destructuring the response
    ListResourceDataSyncResponse (..),
    mkListResourceDataSyncResponse,

    -- ** Response lenses
    lrdsrsResourceDataSyncItems,
    lrdsrsNextToken,
    lrdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkListResourceDataSync' smart constructor.
data ListResourceDataSync = ListResourceDataSync'
  { syncType ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListResourceDataSync' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
-- * 'syncType' - View a list of resource data syncs according to the sync type. Specify @SyncToDestination@ to view resource data syncs that synchronize data to an Amazon S3 bucket. Specify @SyncFromSource@ to view resource data syncs from AWS Organizations or from multiple AWS Regions.
mkListResourceDataSync ::
  ListResourceDataSync
mkListResourceDataSync =
  ListResourceDataSync'
    { syncType = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | View a list of resource data syncs according to the sync type. Specify @SyncToDestination@ to view resource data syncs that synchronize data to an Amazon S3 bucket. Specify @SyncFromSource@ to view resource data syncs from AWS Organizations or from multiple AWS Regions.
--
-- /Note:/ Consider using 'syncType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsSyncType :: Lens.Lens' ListResourceDataSync (Lude.Maybe Lude.Text)
lrdsSyncType = Lens.lens (syncType :: ListResourceDataSync -> Lude.Maybe Lude.Text) (\s a -> s {syncType = a} :: ListResourceDataSync)
{-# DEPRECATED lrdsSyncType "Use generic-lens or generic-optics with 'syncType' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsNextToken :: Lens.Lens' ListResourceDataSync (Lude.Maybe Lude.Text)
lrdsNextToken = Lens.lens (nextToken :: ListResourceDataSync -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourceDataSync)
{-# DEPRECATED lrdsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsMaxResults :: Lens.Lens' ListResourceDataSync (Lude.Maybe Lude.Natural)
lrdsMaxResults = Lens.lens (maxResults :: ListResourceDataSync -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListResourceDataSync)
{-# DEPRECATED lrdsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListResourceDataSync where
  page rq rs
    | Page.stop (rs Lens.^. lrdsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrdsrsResourceDataSyncItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrdsNextToken Lens..~ rs Lens.^. lrdsrsNextToken

instance Lude.AWSRequest ListResourceDataSync where
  type Rs ListResourceDataSync = ListResourceDataSyncResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourceDataSyncResponse'
            Lude.<$> (x Lude..?> "ResourceDataSyncItems" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResourceDataSync where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ListResourceDataSync" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListResourceDataSync where
  toJSON ListResourceDataSync' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SyncType" Lude..=) Lude.<$> syncType,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListResourceDataSync where
  toPath = Lude.const "/"

instance Lude.ToQuery ListResourceDataSync where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListResourceDataSyncResponse' smart constructor.
data ListResourceDataSyncResponse = ListResourceDataSyncResponse'
  { resourceDataSyncItems ::
      Lude.Maybe [ResourceDataSyncItem],
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

-- | Creates a value of 'ListResourceDataSyncResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of items to return. Use this token to get the next set of results.
-- * 'resourceDataSyncItems' - A list of your current Resource Data Sync configurations and their statuses.
-- * 'responseStatus' - The response status code.
mkListResourceDataSyncResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourceDataSyncResponse
mkListResourceDataSyncResponse pResponseStatus_ =
  ListResourceDataSyncResponse'
    { resourceDataSyncItems =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of your current Resource Data Sync configurations and their statuses.
--
-- /Note:/ Consider using 'resourceDataSyncItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrsResourceDataSyncItems :: Lens.Lens' ListResourceDataSyncResponse (Lude.Maybe [ResourceDataSyncItem])
lrdsrsResourceDataSyncItems = Lens.lens (resourceDataSyncItems :: ListResourceDataSyncResponse -> Lude.Maybe [ResourceDataSyncItem]) (\s a -> s {resourceDataSyncItems = a} :: ListResourceDataSyncResponse)
{-# DEPRECATED lrdsrsResourceDataSyncItems "Use generic-lens or generic-optics with 'resourceDataSyncItems' instead." #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrsNextToken :: Lens.Lens' ListResourceDataSyncResponse (Lude.Maybe Lude.Text)
lrdsrsNextToken = Lens.lens (nextToken :: ListResourceDataSyncResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourceDataSyncResponse)
{-# DEPRECATED lrdsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrsResponseStatus :: Lens.Lens' ListResourceDataSyncResponse Lude.Int
lrdsrsResponseStatus = Lens.lens (responseStatus :: ListResourceDataSyncResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourceDataSyncResponse)
{-# DEPRECATED lrdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
