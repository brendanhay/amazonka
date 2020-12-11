{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of build project names, with each build project name representing a single build project.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListProjects
  ( -- * Creating a request
    ListProjects (..),
    mkListProjects,

    -- ** Request lenses
    lpSortOrder,
    lpNextToken,
    lpSortBy,

    -- * Destructuring the response
    ListProjectsResponse (..),
    mkListProjectsResponse,

    -- ** Response lenses
    lprsNextToken,
    lprsProjects,
    lprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListProjects' smart constructor.
data ListProjects = ListProjects'
  { sortOrder ::
      Lude.Maybe SortOrderType,
    nextToken :: Lude.Maybe Lude.Text,
    sortBy :: Lude.Maybe ProjectSortByType
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
-- * 'nextToken' - During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'sortBy' - The criterion to be used to list build project names. Valid values include:
--
--
--     * @CREATED_TIME@ : List based on when each build project was created.
--
--
--     * @LAST_MODIFIED_TIME@ : List based on when information about each build project was last changed.
--
--
--     * @NAME@ : List based on each build project's name.
--
--
-- Use @sortOrder@ to specify in what order to list the build project names based on the preceding criteria.
-- * 'sortOrder' - The order in which to list build projects. Valid values include:
--
--
--     * @ASCENDING@ : List in ascending order.
--
--
--     * @DESCENDING@ : List in descending order.
--
--
-- Use @sortBy@ to specify the criterion to be used to list build project names.
mkListProjects ::
  ListProjects
mkListProjects =
  ListProjects'
    { sortOrder = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | The order in which to list build projects. Valid values include:
--
--
--     * @ASCENDING@ : List in ascending order.
--
--
--     * @DESCENDING@ : List in descending order.
--
--
-- Use @sortBy@ to specify the criterion to be used to list build project names.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpSortOrder :: Lens.Lens' ListProjects (Lude.Maybe SortOrderType)
lpSortOrder = Lens.lens (sortOrder :: ListProjects -> Lude.Maybe SortOrderType) (\s a -> s {sortOrder = a} :: ListProjects)
{-# DEPRECATED lpSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListProjects (Lude.Maybe Lude.Text)
lpNextToken = Lens.lens (nextToken :: ListProjects -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProjects)
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The criterion to be used to list build project names. Valid values include:
--
--
--     * @CREATED_TIME@ : List based on when each build project was created.
--
--
--     * @LAST_MODIFIED_TIME@ : List based on when information about each build project was last changed.
--
--
--     * @NAME@ : List based on each build project's name.
--
--
-- Use @sortOrder@ to specify in what order to list the build project names based on the preceding criteria.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpSortBy :: Lens.Lens' ListProjects (Lude.Maybe ProjectSortByType)
lpSortBy = Lens.lens (sortBy :: ListProjects -> Lude.Maybe ProjectSortByType) (\s a -> s {sortBy = a} :: ListProjects)
{-# DEPRECATED lpSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

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
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProjectsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "projects")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListProjects where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.ListProjects" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListProjects where
  toJSON ListProjects' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sortOrder" Lude..=) Lude.<$> sortOrder,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("sortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListProjects where
  toPath = Lude.const "/"

instance Lude.ToQuery ListProjects where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    projects :: Lude.Maybe (Lude.NonEmpty Lude.Text),
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
-- * 'nextToken' - If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
-- * 'projects' - The list of build project names, with each build project name representing a single build project.
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

-- | If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' ListProjectsResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: ListProjectsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProjectsResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of build project names, with each build project name representing a single build project.
--
-- /Note:/ Consider using 'projects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsProjects :: Lens.Lens' ListProjectsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
lprsProjects = Lens.lens (projects :: ListProjectsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {projects = a} :: ListProjectsResponse)
{-# DEPRECATED lprsProjects "Use generic-lens or generic-optics with 'projects' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListProjectsResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListProjectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProjectsResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
