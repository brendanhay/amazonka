{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.ListProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists projects in AWS Mobile Hub.
--
-- This operation returns paginated results.
module Network.AWS.Mobile.ListProjects
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request structure used to request projects list in AWS Mobile Hub.
--
-- /See:/ 'mkListProjects' smart constructor.
data ListProjects = ListProjects'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProjects' with the minimum fields required to make a request.
--
-- * 'maxResults' - Maximum number of records to list in a single response.
-- * 'nextToken' - Pagination token. Set to null to start listing projects from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more projects.
mkListProjects ::
  ListProjects
mkListProjects =
  ListProjects'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Pagination token. Set to null to start listing projects from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more projects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListProjects (Lude.Maybe Lude.Text)
lpNextToken = Lens.lens (nextToken :: ListProjects -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProjects)
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of records to list in a single response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListProjects (Lude.Maybe Lude.Int)
lpMaxResults = Lens.lens (maxResults :: ListProjects -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListProjects)
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
  request = Req.get mobileService
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
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListProjects where
  toPath = Lude.const "/projects"

instance Lude.ToQuery ListProjects where
  toQuery ListProjects' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Result structure used for requests to list projects in AWS Mobile Hub.
--
-- /See:/ 'mkListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    projects :: Lude.Maybe [ProjectSummary],
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

-- | Creates a value of 'ListProjectsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Undocumented field.
-- * 'projects' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkListProjectsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProjectsResponse
mkListProjectsResponse pResponseStatus_ =
  ListProjectsResponse'
    { nextToken = Lude.Nothing,
      projects = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' ListProjectsResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: ListProjectsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProjectsResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'projects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsProjects :: Lens.Lens' ListProjectsResponse (Lude.Maybe [ProjectSummary])
lprsProjects = Lens.lens (projects :: ListProjectsResponse -> Lude.Maybe [ProjectSummary]) (\s a -> s {projects = a} :: ListProjectsResponse)
{-# DEPRECATED lprsProjects "Use generic-lens or generic-optics with 'projects' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListProjectsResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListProjectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProjectsResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
