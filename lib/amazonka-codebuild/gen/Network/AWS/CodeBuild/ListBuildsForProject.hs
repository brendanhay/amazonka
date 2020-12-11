{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListBuildsForProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of build IDs for the specified build project, with each build ID representing a single build.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuildsForProject
  ( -- * Creating a request
    ListBuildsForProject (..),
    mkListBuildsForProject,

    -- ** Request lenses
    lbfpSortOrder,
    lbfpNextToken,
    lbfpProjectName,

    -- * Destructuring the response
    ListBuildsForProjectResponse (..),
    mkListBuildsForProjectResponse,

    -- ** Response lenses
    lbfprsIds,
    lbfprsNextToken,
    lbfprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListBuildsForProject' smart constructor.
data ListBuildsForProject = ListBuildsForProject'
  { sortOrder ::
      Lude.Maybe SortOrderType,
    nextToken :: Lude.Maybe Lude.Text,
    projectName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBuildsForProject' with the minimum fields required to make a request.
--
-- * 'nextToken' - During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'projectName' - The name of the AWS CodeBuild project.
-- * 'sortOrder' - The order to list build IDs. Valid values include:
--
--
--     * @ASCENDING@ : List the build IDs in ascending order by build ID.
--
--
--     * @DESCENDING@ : List the build IDs in descending order by build ID.
mkListBuildsForProject ::
  -- | 'projectName'
  Lude.Text ->
  ListBuildsForProject
mkListBuildsForProject pProjectName_ =
  ListBuildsForProject'
    { sortOrder = Lude.Nothing,
      nextToken = Lude.Nothing,
      projectName = pProjectName_
    }

-- | The order to list build IDs. Valid values include:
--
--
--     * @ASCENDING@ : List the build IDs in ascending order by build ID.
--
--
--     * @DESCENDING@ : List the build IDs in descending order by build ID.
--
--
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfpSortOrder :: Lens.Lens' ListBuildsForProject (Lude.Maybe SortOrderType)
lbfpSortOrder = Lens.lens (sortOrder :: ListBuildsForProject -> Lude.Maybe SortOrderType) (\s a -> s {sortOrder = a} :: ListBuildsForProject)
{-# DEPRECATED lbfpSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfpNextToken :: Lens.Lens' ListBuildsForProject (Lude.Maybe Lude.Text)
lbfpNextToken = Lens.lens (nextToken :: ListBuildsForProject -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBuildsForProject)
{-# DEPRECATED lbfpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the AWS CodeBuild project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfpProjectName :: Lens.Lens' ListBuildsForProject Lude.Text
lbfpProjectName = Lens.lens (projectName :: ListBuildsForProject -> Lude.Text) (\s a -> s {projectName = a} :: ListBuildsForProject)
{-# DEPRECATED lbfpProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

instance Page.AWSPager ListBuildsForProject where
  page rq rs
    | Page.stop (rs Lens.^. lbfprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbfprsIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbfpNextToken Lens..~ rs Lens.^. lbfprsNextToken

instance Lude.AWSRequest ListBuildsForProject where
  type Rs ListBuildsForProject = ListBuildsForProjectResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBuildsForProjectResponse'
            Lude.<$> (x Lude..?> "ids")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBuildsForProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.ListBuildsForProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListBuildsForProject where
  toJSON ListBuildsForProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sortOrder" Lude..=) Lude.<$> sortOrder,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("projectName" Lude..= projectName)
          ]
      )

instance Lude.ToPath ListBuildsForProject where
  toPath = Lude.const "/"

instance Lude.ToQuery ListBuildsForProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListBuildsForProjectResponse' smart constructor.
data ListBuildsForProjectResponse = ListBuildsForProjectResponse'
  { ids ::
      Lude.Maybe
        (Lude.NonEmpty Lude.Text),
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

-- | Creates a value of 'ListBuildsForProjectResponse' with the minimum fields required to make a request.
--
-- * 'ids' - A list of build IDs for the specified build project, with each build ID representing a single build.
-- * 'nextToken' - If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
-- * 'responseStatus' - The response status code.
mkListBuildsForProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBuildsForProjectResponse
mkListBuildsForProjectResponse pResponseStatus_ =
  ListBuildsForProjectResponse'
    { ids = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of build IDs for the specified build project, with each build ID representing a single build.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfprsIds :: Lens.Lens' ListBuildsForProjectResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
lbfprsIds = Lens.lens (ids :: ListBuildsForProjectResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {ids = a} :: ListBuildsForProjectResponse)
{-# DEPRECATED lbfprsIds "Use generic-lens or generic-optics with 'ids' instead." #-}

-- | If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfprsNextToken :: Lens.Lens' ListBuildsForProjectResponse (Lude.Maybe Lude.Text)
lbfprsNextToken = Lens.lens (nextToken :: ListBuildsForProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBuildsForProjectResponse)
{-# DEPRECATED lbfprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfprsResponseStatus :: Lens.Lens' ListBuildsForProjectResponse Lude.Int
lbfprsResponseStatus = Lens.lens (responseStatus :: ListBuildsForProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBuildsForProjectResponse)
{-# DEPRECATED lbfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
