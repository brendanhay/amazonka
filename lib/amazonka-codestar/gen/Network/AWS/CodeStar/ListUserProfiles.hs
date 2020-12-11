{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.ListUserProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the user profiles configured for your AWS account in AWS CodeStar.
--
-- This operation returns paginated results.
module Network.AWS.CodeStar.ListUserProfiles
  ( -- * Creating a request
    ListUserProfiles (..),
    mkListUserProfiles,

    -- ** Request lenses
    lupNextToken,
    lupMaxResults,

    -- * Destructuring the response
    ListUserProfilesResponse (..),
    mkListUserProfilesResponse,

    -- ** Response lenses
    luprsNextToken,
    luprsResponseStatus,
    luprsUserProfiles,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListUserProfiles' smart constructor.
data ListUserProfiles = ListUserProfiles'
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

-- | Creates a value of 'ListUserProfiles' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return in a response.
-- * 'nextToken' - The continuation token for the next set of results, if the results cannot be returned in one response.
mkListUserProfiles ::
  ListUserProfiles
mkListUserProfiles =
  ListUserProfiles'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The continuation token for the next set of results, if the results cannot be returned in one response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupNextToken :: Lens.Lens' ListUserProfiles (Lude.Maybe Lude.Text)
lupNextToken = Lens.lens (nextToken :: ListUserProfiles -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUserProfiles)
{-# DEPRECATED lupNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupMaxResults :: Lens.Lens' ListUserProfiles (Lude.Maybe Lude.Natural)
lupMaxResults = Lens.lens (maxResults :: ListUserProfiles -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListUserProfiles)
{-# DEPRECATED lupMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

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
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUserProfilesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "userProfiles" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListUserProfiles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.ListUserProfiles" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListUserProfiles where
  toJSON ListUserProfiles' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListUserProfiles where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUserProfiles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListUserProfilesResponse' smart constructor.
data ListUserProfilesResponse = ListUserProfilesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    userProfiles :: [UserProfileSummary]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUserProfilesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The continuation token to use when requesting the next set of results, if there are more results to be returned.
-- * 'responseStatus' - The response status code.
-- * 'userProfiles' - All the user profiles configured in AWS CodeStar for an AWS account.
mkListUserProfilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUserProfilesResponse
mkListUserProfilesResponse pResponseStatus_ =
  ListUserProfilesResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      userProfiles = Lude.mempty
    }

-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
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

-- | All the user profiles configured in AWS CodeStar for an AWS account.
--
-- /Note:/ Consider using 'userProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsUserProfiles :: Lens.Lens' ListUserProfilesResponse [UserProfileSummary]
luprsUserProfiles = Lens.lens (userProfiles :: ListUserProfilesResponse -> [UserProfileSummary]) (\s a -> s {userProfiles = a} :: ListUserProfilesResponse)
{-# DEPRECATED luprsUserProfiles "Use generic-lens or generic-optics with 'userProfiles' instead." #-}
