{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListApps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists apps.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListApps
  ( -- * Creating a request
    ListApps (..),
    mkListApps,

    -- ** Request lenses
    laDomainIdEquals,
    laNextToken,
    laSortOrder,
    laUserProfileNameEquals,
    laMaxResults,
    laSortBy,

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
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListApps' smart constructor.
data ListApps = ListApps'
  { domainIdEquals :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe SortOrder,
    userProfileNameEquals :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe AppSortKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApps' with the minimum fields required to make a request.
--
-- * 'domainIdEquals' - A parameter to search for the domain ID.
-- * 'maxResults' - Returns a list up to a specified limit.
-- * 'nextToken' - If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
-- * 'sortBy' - The parameter by which to sort the results. The default is CreationTime.
-- * 'sortOrder' - The sort order for the results. The default is Ascending.
-- * 'userProfileNameEquals' - A parameter to search by user profile name.
mkListApps ::
  ListApps
mkListApps =
  ListApps'
    { domainIdEquals = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      userProfileNameEquals = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A parameter to search for the domain ID.
--
-- /Note:/ Consider using 'domainIdEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laDomainIdEquals :: Lens.Lens' ListApps (Lude.Maybe Lude.Text)
laDomainIdEquals = Lens.lens (domainIdEquals :: ListApps -> Lude.Maybe Lude.Text) (\s a -> s {domainIdEquals = a} :: ListApps)
{-# DEPRECATED laDomainIdEquals "Use generic-lens or generic-optics with 'domainIdEquals' instead." #-}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListApps (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListApps -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApps)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for the results. The default is Ascending.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laSortOrder :: Lens.Lens' ListApps (Lude.Maybe SortOrder)
laSortOrder = Lens.lens (sortOrder :: ListApps -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListApps)
{-# DEPRECATED laSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A parameter to search by user profile name.
--
-- /Note:/ Consider using 'userProfileNameEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laUserProfileNameEquals :: Lens.Lens' ListApps (Lude.Maybe Lude.Text)
laUserProfileNameEquals = Lens.lens (userProfileNameEquals :: ListApps -> Lude.Maybe Lude.Text) (\s a -> s {userProfileNameEquals = a} :: ListApps)
{-# DEPRECATED laUserProfileNameEquals "Use generic-lens or generic-optics with 'userProfileNameEquals' instead." #-}

-- | Returns a list up to a specified limit.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListApps (Lude.Maybe Lude.Natural)
laMaxResults = Lens.lens (maxResults :: ListApps -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListApps)
{-# DEPRECATED laMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter by which to sort the results. The default is CreationTime.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laSortBy :: Lens.Lens' ListApps (Lude.Maybe AppSortKey)
laSortBy = Lens.lens (sortBy :: ListApps -> Lude.Maybe AppSortKey) (\s a -> s {sortBy = a} :: ListApps)
{-# DEPRECATED laSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

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
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAppsResponse'
            Lude.<$> (x Lude..?> "Apps" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListApps where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("SageMaker.ListApps" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListApps where
  toJSON ListApps' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DomainIdEquals" Lude..=) Lude.<$> domainIdEquals,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("UserProfileNameEquals" Lude..=) Lude.<$> userProfileNameEquals,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListApps where
  toPath = Lude.const "/"

instance Lude.ToQuery ListApps where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { apps ::
      Lude.Maybe [AppDetails],
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

-- | Creates a value of 'ListAppsResponse' with the minimum fields required to make a request.
--
-- * 'apps' - The list of apps.
-- * 'nextToken' - If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
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

-- | The list of apps.
--
-- /Note:/ Consider using 'apps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsApps :: Lens.Lens' ListAppsResponse (Lude.Maybe [AppDetails])
larsApps = Lens.lens (apps :: ListAppsResponse -> Lude.Maybe [AppDetails]) (\s a -> s {apps = a} :: ListAppsResponse)
{-# DEPRECATED larsApps "Use generic-lens or generic-optics with 'apps' instead." #-}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
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
