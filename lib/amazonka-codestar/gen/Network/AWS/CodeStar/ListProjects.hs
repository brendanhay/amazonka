{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.ListProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all projects in AWS CodeStar associated with your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeStar.ListProjects
  ( -- * Creating a request
    ListProjects (..),
    mkListProjects,

    -- ** Request lenses
    lpNextToken,
    lpMaxResults,

    -- * Destructuring the response
    ListProjectsResponse (..),
    mkListProjectsResponse,

    -- ** Response lenses
    lprsNextToken,
    lprsProjects,
    lprsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListProjects' smart constructor.
data ListProjects = ListProjects'
  { -- | The continuation token to be used to return the next set of results, if the results cannot be returned in one response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum amount of data that can be contained in a single set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProjects' with the minimum fields required to make a request.
--
-- * 'nextToken' - The continuation token to be used to return the next set of results, if the results cannot be returned in one response.
-- * 'maxResults' - The maximum amount of data that can be contained in a single set of results.
mkListProjects ::
  ListProjects
mkListProjects =
  ListProjects'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The continuation token to be used to return the next set of results, if the results cannot be returned in one response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListProjects (Lude.Maybe Lude.Text)
lpNextToken = Lens.lens (nextToken :: ListProjects -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProjects)
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum amount of data that can be contained in a single set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListProjects (Lude.Maybe Lude.Natural)
lpMaxResults = Lens.lens (maxResults :: ListProjects -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListProjects)
{-# DEPRECATED lpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListProjects where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsProjects) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpNextToken Lens..~ rs Lens.^. lprsNextToken

instance Lude.AWSRequest ListProjects where
  type Rs ListProjects = ListProjectsResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProjectsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "projects" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListProjects where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.ListProjects" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListProjects where
  toJSON ListProjects' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListProjects where
  toPath = Lude.const "/"

instance Lude.ToQuery ListProjects where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { -- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of projects.
    projects :: [ProjectSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProjectsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The continuation token to use when requesting the next set of results, if there are more results to be returned.
-- * 'projects' - A list of projects.
-- * 'responseStatus' - The response status code.
mkListProjectsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProjectsResponse
mkListProjectsResponse pResponseStatus_ =
  ListProjectsResponse'
    { nextToken = Lude.Nothing,
      projects = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' ListProjectsResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: ListProjectsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProjectsResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of projects.
--
-- /Note:/ Consider using 'projects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsProjects :: Lens.Lens' ListProjectsResponse [ProjectSummary]
lprsProjects = Lens.lens (projects :: ListProjectsResponse -> [ProjectSummary]) (\s a -> s {projects = a} :: ListProjectsResponse)
{-# DEPRECATED lprsProjects "Use generic-lens or generic-optics with 'projects' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListProjectsResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListProjectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProjectsResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
