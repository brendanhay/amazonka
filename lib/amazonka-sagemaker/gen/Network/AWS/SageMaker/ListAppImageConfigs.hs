{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListAppImageConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AppImageConfigs in your account and their properties. The list can be filtered by creation time or modified time, and whether the AppImageConfig name contains a specified string.
module Network.AWS.SageMaker.ListAppImageConfigs
  ( -- * Creating a request
    ListAppImageConfigs (..),
    mkListAppImageConfigs,

    -- ** Request lenses
    laicNameContains,
    laicCreationTimeAfter,
    laicModifiedTimeAfter,
    laicNextToken,
    laicSortOrder,
    laicCreationTimeBefore,
    laicModifiedTimeBefore,
    laicMaxResults,
    laicSortBy,

    -- * Destructuring the response
    ListAppImageConfigsResponse (..),
    mkListAppImageConfigsResponse,

    -- ** Response lenses
    laicrsAppImageConfigs,
    laicrsNextToken,
    laicrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListAppImageConfigs' smart constructor.
data ListAppImageConfigs = ListAppImageConfigs'
  { -- | A filter that returns only AppImageConfigs whose name contains the specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | A filter that returns only AppImageConfigs created on or after the specified time.
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only AppImageConfigs modified on or after the specified time.
    modifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | If the previous call to @ListImages@ didn't return the full set of AppImageConfigs, the call returns a token for getting the next set of AppImageConfigs.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order. The default value is @Descending@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | A filter that returns only AppImageConfigs created on or before the specified time.
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only AppImageConfigs modified on or before the specified time.
    modifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | The maximum number of AppImageConfigs to return in the response. The default value is 10.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The property used to sort results. The default value is @CreationTime@ .
    sortBy :: Lude.Maybe AppImageConfigSortKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAppImageConfigs' with the minimum fields required to make a request.
--
-- * 'nameContains' - A filter that returns only AppImageConfigs whose name contains the specified string.
-- * 'creationTimeAfter' - A filter that returns only AppImageConfigs created on or after the specified time.
-- * 'modifiedTimeAfter' - A filter that returns only AppImageConfigs modified on or after the specified time.
-- * 'nextToken' - If the previous call to @ListImages@ didn't return the full set of AppImageConfigs, the call returns a token for getting the next set of AppImageConfigs.
-- * 'sortOrder' - The sort order. The default value is @Descending@ .
-- * 'creationTimeBefore' - A filter that returns only AppImageConfigs created on or before the specified time.
-- * 'modifiedTimeBefore' - A filter that returns only AppImageConfigs modified on or before the specified time.
-- * 'maxResults' - The maximum number of AppImageConfigs to return in the response. The default value is 10.
-- * 'sortBy' - The property used to sort results. The default value is @CreationTime@ .
mkListAppImageConfigs ::
  ListAppImageConfigs
mkListAppImageConfigs =
  ListAppImageConfigs'
    { nameContains = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      modifiedTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      modifiedTimeBefore = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A filter that returns only AppImageConfigs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicNameContains :: Lens.Lens' ListAppImageConfigs (Lude.Maybe Lude.Text)
laicNameContains = Lens.lens (nameContains :: ListAppImageConfigs -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListAppImageConfigs)
{-# DEPRECATED laicNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only AppImageConfigs created on or after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicCreationTimeAfter :: Lens.Lens' ListAppImageConfigs (Lude.Maybe Lude.Timestamp)
laicCreationTimeAfter = Lens.lens (creationTimeAfter :: ListAppImageConfigs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListAppImageConfigs)
{-# DEPRECATED laicCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only AppImageConfigs modified on or after the specified time.
--
-- /Note:/ Consider using 'modifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicModifiedTimeAfter :: Lens.Lens' ListAppImageConfigs (Lude.Maybe Lude.Timestamp)
laicModifiedTimeAfter = Lens.lens (modifiedTimeAfter :: ListAppImageConfigs -> Lude.Maybe Lude.Timestamp) (\s a -> s {modifiedTimeAfter = a} :: ListAppImageConfigs)
{-# DEPRECATED laicModifiedTimeAfter "Use generic-lens or generic-optics with 'modifiedTimeAfter' instead." #-}

-- | If the previous call to @ListImages@ didn't return the full set of AppImageConfigs, the call returns a token for getting the next set of AppImageConfigs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicNextToken :: Lens.Lens' ListAppImageConfigs (Lude.Maybe Lude.Text)
laicNextToken = Lens.lens (nextToken :: ListAppImageConfigs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAppImageConfigs)
{-# DEPRECATED laicNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order. The default value is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicSortOrder :: Lens.Lens' ListAppImageConfigs (Lude.Maybe SortOrder)
laicSortOrder = Lens.lens (sortOrder :: ListAppImageConfigs -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListAppImageConfigs)
{-# DEPRECATED laicSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only AppImageConfigs created on or before the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicCreationTimeBefore :: Lens.Lens' ListAppImageConfigs (Lude.Maybe Lude.Timestamp)
laicCreationTimeBefore = Lens.lens (creationTimeBefore :: ListAppImageConfigs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListAppImageConfigs)
{-# DEPRECATED laicCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns only AppImageConfigs modified on or before the specified time.
--
-- /Note:/ Consider using 'modifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicModifiedTimeBefore :: Lens.Lens' ListAppImageConfigs (Lude.Maybe Lude.Timestamp)
laicModifiedTimeBefore = Lens.lens (modifiedTimeBefore :: ListAppImageConfigs -> Lude.Maybe Lude.Timestamp) (\s a -> s {modifiedTimeBefore = a} :: ListAppImageConfigs)
{-# DEPRECATED laicModifiedTimeBefore "Use generic-lens or generic-optics with 'modifiedTimeBefore' instead." #-}

-- | The maximum number of AppImageConfigs to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicMaxResults :: Lens.Lens' ListAppImageConfigs (Lude.Maybe Lude.Natural)
laicMaxResults = Lens.lens (maxResults :: ListAppImageConfigs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAppImageConfigs)
{-# DEPRECATED laicMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The property used to sort results. The default value is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicSortBy :: Lens.Lens' ListAppImageConfigs (Lude.Maybe AppImageConfigSortKey)
laicSortBy = Lens.lens (sortBy :: ListAppImageConfigs -> Lude.Maybe AppImageConfigSortKey) (\s a -> s {sortBy = a} :: ListAppImageConfigs)
{-# DEPRECATED laicSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Lude.AWSRequest ListAppImageConfigs where
  type Rs ListAppImageConfigs = ListAppImageConfigsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAppImageConfigsResponse'
            Lude.<$> (x Lude..?> "AppImageConfigs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAppImageConfigs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListAppImageConfigs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAppImageConfigs where
  toJSON ListAppImageConfigs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("ModifiedTimeAfter" Lude..=) Lude.<$> modifiedTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("ModifiedTimeBefore" Lude..=) Lude.<$> modifiedTimeBefore,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListAppImageConfigs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAppImageConfigs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAppImageConfigsResponse' smart constructor.
data ListAppImageConfigsResponse = ListAppImageConfigsResponse'
  { -- | A list of AppImageConfigs and their properties.
    appImageConfigs :: Lude.Maybe [AppImageConfigDetails],
    -- | A token for getting the next set of AppImageConfigs, if there are any.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAppImageConfigsResponse' with the minimum fields required to make a request.
--
-- * 'appImageConfigs' - A list of AppImageConfigs and their properties.
-- * 'nextToken' - A token for getting the next set of AppImageConfigs, if there are any.
-- * 'responseStatus' - The response status code.
mkListAppImageConfigsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAppImageConfigsResponse
mkListAppImageConfigsResponse pResponseStatus_ =
  ListAppImageConfigsResponse'
    { appImageConfigs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of AppImageConfigs and their properties.
--
-- /Note:/ Consider using 'appImageConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicrsAppImageConfigs :: Lens.Lens' ListAppImageConfigsResponse (Lude.Maybe [AppImageConfigDetails])
laicrsAppImageConfigs = Lens.lens (appImageConfigs :: ListAppImageConfigsResponse -> Lude.Maybe [AppImageConfigDetails]) (\s a -> s {appImageConfigs = a} :: ListAppImageConfigsResponse)
{-# DEPRECATED laicrsAppImageConfigs "Use generic-lens or generic-optics with 'appImageConfigs' instead." #-}

-- | A token for getting the next set of AppImageConfigs, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicrsNextToken :: Lens.Lens' ListAppImageConfigsResponse (Lude.Maybe Lude.Text)
laicrsNextToken = Lens.lens (nextToken :: ListAppImageConfigsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAppImageConfigsResponse)
{-# DEPRECATED laicrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laicrsResponseStatus :: Lens.Lens' ListAppImageConfigsResponse Lude.Int
laicrsResponseStatus = Lens.lens (responseStatus :: ListAppImageConfigsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAppImageConfigsResponse)
{-# DEPRECATED laicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
