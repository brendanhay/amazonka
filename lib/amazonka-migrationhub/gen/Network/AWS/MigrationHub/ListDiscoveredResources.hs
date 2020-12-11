{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.ListDiscoveredResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists discovered resources associated with the given @MigrationTask@ .
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListDiscoveredResources
  ( -- * Creating a request
    ListDiscoveredResources (..),
    mkListDiscoveredResources,

    -- ** Request lenses
    ldrNextToken,
    ldrMaxResults,
    ldrProgressUpdateStream,
    ldrMigrationTaskName,

    -- * Destructuring the response
    ListDiscoveredResourcesResponse (..),
    mkListDiscoveredResourcesResponse,

    -- ** Response lenses
    ldrrsDiscoveredResourceList,
    ldrrsNextToken,
    ldrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDiscoveredResources' smart constructor.
data ListDiscoveredResources = ListDiscoveredResources'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    progressUpdateStream :: Lude.Text,
    migrationTaskName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDiscoveredResources' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results returned per page.
-- * 'migrationTaskName' - The name of the MigrationTask. /Do not store personal data in this field./
-- * 'nextToken' - If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
-- * 'progressUpdateStream' - The name of the ProgressUpdateStream.
mkListDiscoveredResources ::
  -- | 'progressUpdateStream'
  Lude.Text ->
  -- | 'migrationTaskName'
  Lude.Text ->
  ListDiscoveredResources
mkListDiscoveredResources
  pProgressUpdateStream_
  pMigrationTaskName_ =
    ListDiscoveredResources'
      { nextToken = Lude.Nothing,
        maxResults = Lude.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_
      }

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrNextToken :: Lens.Lens' ListDiscoveredResources (Lude.Maybe Lude.Text)
ldrNextToken = Lens.lens (nextToken :: ListDiscoveredResources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDiscoveredResources)
{-# DEPRECATED ldrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results returned per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrMaxResults :: Lens.Lens' ListDiscoveredResources (Lude.Maybe Lude.Natural)
ldrMaxResults = Lens.lens (maxResults :: ListDiscoveredResources -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDiscoveredResources)
{-# DEPRECATED ldrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrProgressUpdateStream :: Lens.Lens' ListDiscoveredResources Lude.Text
ldrProgressUpdateStream = Lens.lens (progressUpdateStream :: ListDiscoveredResources -> Lude.Text) (\s a -> s {progressUpdateStream = a} :: ListDiscoveredResources)
{-# DEPRECATED ldrProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | The name of the MigrationTask. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrMigrationTaskName :: Lens.Lens' ListDiscoveredResources Lude.Text
ldrMigrationTaskName = Lens.lens (migrationTaskName :: ListDiscoveredResources -> Lude.Text) (\s a -> s {migrationTaskName = a} :: ListDiscoveredResources)
{-# DEPRECATED ldrMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

instance Page.AWSPager ListDiscoveredResources where
  page rq rs
    | Page.stop (rs Lens.^. ldrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrrsDiscoveredResourceList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldrNextToken Lens..~ rs Lens.^. ldrrsNextToken

instance Lude.AWSRequest ListDiscoveredResources where
  type Rs ListDiscoveredResources = ListDiscoveredResourcesResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDiscoveredResourcesResponse'
            Lude.<$> (x Lude..?> "DiscoveredResourceList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDiscoveredResources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.ListDiscoveredResources" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDiscoveredResources where
  toJSON ListDiscoveredResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ProgressUpdateStream" Lude..= progressUpdateStream),
            Lude.Just ("MigrationTaskName" Lude..= migrationTaskName)
          ]
      )

instance Lude.ToPath ListDiscoveredResources where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDiscoveredResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDiscoveredResourcesResponse' smart constructor.
data ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse'
  { discoveredResourceList ::
      Lude.Maybe
        [DiscoveredResource],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListDiscoveredResourcesResponse' with the minimum fields required to make a request.
--
-- * 'discoveredResourceList' - Returned list of discovered resources associated with the given MigrationTask.
-- * 'nextToken' - If there are more discovered resources than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
-- * 'responseStatus' - The response status code.
mkListDiscoveredResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDiscoveredResourcesResponse
mkListDiscoveredResourcesResponse pResponseStatus_ =
  ListDiscoveredResourcesResponse'
    { discoveredResourceList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returned list of discovered resources associated with the given MigrationTask.
--
-- /Note:/ Consider using 'discoveredResourceList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDiscoveredResourceList :: Lens.Lens' ListDiscoveredResourcesResponse (Lude.Maybe [DiscoveredResource])
ldrrsDiscoveredResourceList = Lens.lens (discoveredResourceList :: ListDiscoveredResourcesResponse -> Lude.Maybe [DiscoveredResource]) (\s a -> s {discoveredResourceList = a} :: ListDiscoveredResourcesResponse)
{-# DEPRECATED ldrrsDiscoveredResourceList "Use generic-lens or generic-optics with 'discoveredResourceList' instead." #-}

-- | If there are more discovered resources than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextToken :: Lens.Lens' ListDiscoveredResourcesResponse (Lude.Maybe Lude.Text)
ldrrsNextToken = Lens.lens (nextToken :: ListDiscoveredResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDiscoveredResourcesResponse)
{-# DEPRECATED ldrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDiscoveredResourcesResponse Lude.Int
ldrrsResponseStatus = Lens.lens (responseStatus :: ListDiscoveredResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDiscoveredResourcesResponse)
{-# DEPRECATED ldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
