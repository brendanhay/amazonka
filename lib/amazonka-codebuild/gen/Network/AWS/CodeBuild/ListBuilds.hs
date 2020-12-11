{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListBuilds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of build IDs, with each build ID representing a single build.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuilds
  ( -- * Creating a request
    ListBuilds (..),
    mkListBuilds,

    -- ** Request lenses
    lbSortOrder,
    lbNextToken,

    -- * Destructuring the response
    ListBuildsResponse (..),
    mkListBuildsResponse,

    -- ** Response lenses
    lbrsIds,
    lbrsNextToken,
    lbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListBuilds' smart constructor.
data ListBuilds = ListBuilds'
  { sortOrder ::
      Lude.Maybe SortOrderType,
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBuilds' with the minimum fields required to make a request.
--
-- * 'nextToken' - During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'sortOrder' - The order to list build IDs. Valid values include:
--
--
--     * @ASCENDING@ : List the build IDs in ascending order by build ID.
--
--
--     * @DESCENDING@ : List the build IDs in descending order by build ID.
mkListBuilds ::
  ListBuilds
mkListBuilds =
  ListBuilds' {sortOrder = Lude.Nothing, nextToken = Lude.Nothing}

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
lbSortOrder :: Lens.Lens' ListBuilds (Lude.Maybe SortOrderType)
lbSortOrder = Lens.lens (sortOrder :: ListBuilds -> Lude.Maybe SortOrderType) (\s a -> s {sortOrder = a} :: ListBuilds)
{-# DEPRECATED lbSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbNextToken :: Lens.Lens' ListBuilds (Lude.Maybe Lude.Text)
lbNextToken = Lens.lens (nextToken :: ListBuilds -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBuilds)
{-# DEPRECATED lbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListBuilds where
  page rq rs
    | Page.stop (rs Lens.^. lbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbrsIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbNextToken Lens..~ rs Lens.^. lbrsNextToken

instance Lude.AWSRequest ListBuilds where
  type Rs ListBuilds = ListBuildsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBuildsResponse'
            Lude.<$> (x Lude..?> "ids")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBuilds where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.ListBuilds" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListBuilds where
  toJSON ListBuilds' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sortOrder" Lude..=) Lude.<$> sortOrder,
            ("nextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath ListBuilds where
  toPath = Lude.const "/"

instance Lude.ToQuery ListBuilds where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListBuildsResponse' smart constructor.
data ListBuildsResponse = ListBuildsResponse'
  { ids ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
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

-- | Creates a value of 'ListBuildsResponse' with the minimum fields required to make a request.
--
-- * 'ids' - A list of build IDs, with each build ID representing a single build.
-- * 'nextToken' - If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
-- * 'responseStatus' - The response status code.
mkListBuildsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBuildsResponse
mkListBuildsResponse pResponseStatus_ =
  ListBuildsResponse'
    { ids = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of build IDs, with each build ID representing a single build.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsIds :: Lens.Lens' ListBuildsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
lbrsIds = Lens.lens (ids :: ListBuildsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {ids = a} :: ListBuildsResponse)
{-# DEPRECATED lbrsIds "Use generic-lens or generic-optics with 'ids' instead." #-}

-- | If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /nextToken/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsNextToken :: Lens.Lens' ListBuildsResponse (Lude.Maybe Lude.Text)
lbrsNextToken = Lens.lens (nextToken :: ListBuildsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBuildsResponse)
{-# DEPRECATED lbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsResponseStatus :: Lens.Lens' ListBuildsResponse Lude.Int
lbrsResponseStatus = Lens.lens (responseStatus :: ListBuildsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBuildsResponse)
{-# DEPRECATED lbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
