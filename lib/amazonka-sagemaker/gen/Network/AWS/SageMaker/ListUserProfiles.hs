{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListUserProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists user profiles.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListUserProfiles
  ( -- * Creating a request
    ListUserProfiles (..),
    mkListUserProfiles,

    -- ** Request lenses
    lupDomainIdEquals,
    lupUserProfileNameContains,
    lupNextToken,
    lupSortOrder,
    lupMaxResults,
    lupSortBy,

    -- * Destructuring the response
    ListUserProfilesResponse (..),
    mkListUserProfilesResponse,

    -- ** Response lenses
    luprsUserProfiles,
    luprsNextToken,
    luprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListUserProfiles' smart constructor.
data ListUserProfiles = ListUserProfiles'
  { domainIdEquals ::
      Lude.Maybe Lude.Text,
    userProfileNameContains :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe SortOrder,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe UserProfileSortKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUserProfiles' with the minimum fields required to make a request.
--
-- * 'domainIdEquals' - A parameter by which to filter the results.
-- * 'maxResults' - Returns a list up to a specified limit.
-- * 'nextToken' - If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
-- * 'sortBy' - The parameter by which to sort the results. The default is CreationTime.
-- * 'sortOrder' - The sort order for the results. The default is Ascending.
-- * 'userProfileNameContains' - A parameter by which to filter the results.
mkListUserProfiles ::
  ListUserProfiles
mkListUserProfiles =
  ListUserProfiles'
    { domainIdEquals = Lude.Nothing,
      userProfileNameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A parameter by which to filter the results.
--
-- /Note:/ Consider using 'domainIdEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupDomainIdEquals :: Lens.Lens' ListUserProfiles (Lude.Maybe Lude.Text)
lupDomainIdEquals = Lens.lens (domainIdEquals :: ListUserProfiles -> Lude.Maybe Lude.Text) (\s a -> s {domainIdEquals = a} :: ListUserProfiles)
{-# DEPRECATED lupDomainIdEquals "Use generic-lens or generic-optics with 'domainIdEquals' instead." #-}

-- | A parameter by which to filter the results.
--
-- /Note:/ Consider using 'userProfileNameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupUserProfileNameContains :: Lens.Lens' ListUserProfiles (Lude.Maybe Lude.Text)
lupUserProfileNameContains = Lens.lens (userProfileNameContains :: ListUserProfiles -> Lude.Maybe Lude.Text) (\s a -> s {userProfileNameContains = a} :: ListUserProfiles)
{-# DEPRECATED lupUserProfileNameContains "Use generic-lens or generic-optics with 'userProfileNameContains' instead." #-}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupNextToken :: Lens.Lens' ListUserProfiles (Lude.Maybe Lude.Text)
lupNextToken = Lens.lens (nextToken :: ListUserProfiles -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUserProfiles)
{-# DEPRECATED lupNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for the results. The default is Ascending.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupSortOrder :: Lens.Lens' ListUserProfiles (Lude.Maybe SortOrder)
lupSortOrder = Lens.lens (sortOrder :: ListUserProfiles -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListUserProfiles)
{-# DEPRECATED lupSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | Returns a list up to a specified limit.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupMaxResults :: Lens.Lens' ListUserProfiles (Lude.Maybe Lude.Natural)
lupMaxResults = Lens.lens (maxResults :: ListUserProfiles -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListUserProfiles)
{-# DEPRECATED lupMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter by which to sort the results. The default is CreationTime.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupSortBy :: Lens.Lens' ListUserProfiles (Lude.Maybe UserProfileSortKey)
lupSortBy = Lens.lens (sortBy :: ListUserProfiles -> Lude.Maybe UserProfileSortKey) (\s a -> s {sortBy = a} :: ListUserProfiles)
{-# DEPRECATED lupSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListUserProfiles where
  page rq rs
    | Page.stop (rs Lens.^. luprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. luprsUserProfiles) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lupNextToken Lens..~ rs Lens.^. luprsNextToken

instance Lude.AWSRequest ListUserProfiles where
  type Rs ListUserProfiles = ListUserProfilesResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUserProfilesResponse'
            Lude.<$> (x Lude..?> "UserProfiles" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListUserProfiles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListUserProfiles" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListUserProfiles where
  toJSON ListUserProfiles' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DomainIdEquals" Lude..=) Lude.<$> domainIdEquals,
            ("UserProfileNameContains" Lude..=)
              Lude.<$> userProfileNameContains,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListUserProfiles where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUserProfiles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListUserProfilesResponse' smart constructor.
data ListUserProfilesResponse = ListUserProfilesResponse'
  { userProfiles ::
      Lude.Maybe [UserProfileDetails],
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

-- | Creates a value of 'ListUserProfilesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'userProfiles' - The list of user profiles.
mkListUserProfilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUserProfilesResponse
mkListUserProfilesResponse pResponseStatus_ =
  ListUserProfilesResponse'
    { userProfiles = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of user profiles.
--
-- /Note:/ Consider using 'userProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsUserProfiles :: Lens.Lens' ListUserProfilesResponse (Lude.Maybe [UserProfileDetails])
luprsUserProfiles = Lens.lens (userProfiles :: ListUserProfilesResponse -> Lude.Maybe [UserProfileDetails]) (\s a -> s {userProfiles = a} :: ListUserProfilesResponse)
{-# DEPRECATED luprsUserProfiles "Use generic-lens or generic-optics with 'userProfiles' instead." #-}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsNextToken :: Lens.Lens' ListUserProfilesResponse (Lude.Maybe Lude.Text)
luprsNextToken = Lens.lens (nextToken :: ListUserProfilesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUserProfilesResponse)
{-# DEPRECATED luprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsResponseStatus :: Lens.Lens' ListUserProfilesResponse Lude.Int
luprsResponseStatus = Lens.lens (responseStatus :: ListUserProfilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUserProfilesResponse)
{-# DEPRECATED luprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
