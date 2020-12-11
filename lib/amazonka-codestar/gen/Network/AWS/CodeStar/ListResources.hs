{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.ListResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists resources associated with a project in AWS CodeStar.
--
-- This operation returns paginated results.
module Network.AWS.CodeStar.ListResources
  ( -- * Creating a request
    ListResources (..),
    mkListResources,

    -- ** Request lenses
    lrNextToken,
    lrMaxResults,
    lrProjectId,

    -- * Destructuring the response
    ListResourcesResponse (..),
    mkListResourcesResponse,

    -- ** Response lenses
    lrrsResources,
    lrrsNextToken,
    lrrsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListResources' smart constructor.
data ListResources = ListResources'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    projectId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResources' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum amount of data that can be contained in a single set of results.
-- * 'nextToken' - The continuation token for the next set of results, if the results cannot be returned in one response.
-- * 'projectId' - The ID of the project.
mkListResources ::
  -- | 'projectId'
  Lude.Text ->
  ListResources
mkListResources pProjectId_ =
  ListResources'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      projectId = pProjectId_
    }

-- | The continuation token for the next set of results, if the results cannot be returned in one response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListResources (Lude.Maybe Lude.Text)
lrNextToken = Lens.lens (nextToken :: ListResources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResources)
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum amount of data that can be contained in a single set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListResources (Lude.Maybe Lude.Natural)
lrMaxResults = Lens.lens (maxResults :: ListResources -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListResources)
{-# DEPRECATED lrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the project.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrProjectId :: Lens.Lens' ListResources Lude.Text
lrProjectId = Lens.lens (projectId :: ListResources -> Lude.Text) (\s a -> s {projectId = a} :: ListResources)
{-# DEPRECATED lrProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

instance Page.AWSPager ListResources where
  page rq rs
    | Page.stop (rs Lens.^. lrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrrsResources) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrNextToken Lens..~ rs Lens.^. lrrsNextToken

instance Lude.AWSRequest ListResources where
  type Rs ListResources = ListResourcesResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourcesResponse'
            Lude.<$> (x Lude..?> "resources" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.ListResources" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListResources where
  toJSON ListResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("projectId" Lude..= projectId)
          ]
      )

instance Lude.ToPath ListResources where
  toPath = Lude.const "/"

instance Lude.ToQuery ListResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListResourcesResponse' smart constructor.
data ListResourcesResponse = ListResourcesResponse'
  { resources ::
      Lude.Maybe [Resource],
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

-- | Creates a value of 'ListResourcesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The continuation token to use when requesting the next set of results, if there are more results to be returned.
-- * 'resources' - An array of resources associated with the project.
-- * 'responseStatus' - The response status code.
mkListResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourcesResponse
mkListResourcesResponse pResponseStatus_ =
  ListResourcesResponse'
    { resources = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of resources associated with the project.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResources :: Lens.Lens' ListResourcesResponse (Lude.Maybe [Resource])
lrrsResources = Lens.lens (resources :: ListResourcesResponse -> Lude.Maybe [Resource]) (\s a -> s {resources = a} :: ListResourcesResponse)
{-# DEPRECATED lrrsResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextToken :: Lens.Lens' ListResourcesResponse (Lude.Maybe Lude.Text)
lrrsNextToken = Lens.lens (nextToken :: ListResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourcesResponse)
{-# DEPRECATED lrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResponseStatus :: Lens.Lens' ListResourcesResponse Lude.Int
lrrsResponseStatus = Lens.lens (responseStatus :: ListResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourcesResponse)
{-# DEPRECATED lrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
