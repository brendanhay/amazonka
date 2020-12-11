{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.ListTagsForProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the tags for a project.
module Network.AWS.CodeStar.ListTagsForProject
  ( -- * Creating a request
    ListTagsForProject (..),
    mkListTagsForProject,

    -- ** Request lenses
    ltfpNextToken,
    ltfpMaxResults,
    ltfpId,

    -- * Destructuring the response
    ListTagsForProjectResponse (..),
    mkListTagsForProjectResponse,

    -- ** Response lenses
    ltfprsNextToken,
    ltfprsTags,
    ltfprsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTagsForProject' smart constructor.
data ListTagsForProject = ListTagsForProject'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForProject' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the project to get tags for.
-- * 'maxResults' - Reserved for future use.
-- * 'nextToken' - Reserved for future use.
mkListTagsForProject ::
  -- | 'id'
  Lude.Text ->
  ListTagsForProject
mkListTagsForProject pId_ =
  ListTagsForProject'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      id = pId_
    }

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpNextToken :: Lens.Lens' ListTagsForProject (Lude.Maybe Lude.Text)
ltfpNextToken = Lens.lens (nextToken :: ListTagsForProject -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsForProject)
{-# DEPRECATED ltfpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpMaxResults :: Lens.Lens' ListTagsForProject (Lude.Maybe Lude.Natural)
ltfpMaxResults = Lens.lens (maxResults :: ListTagsForProject -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTagsForProject)
{-# DEPRECATED ltfpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the project to get tags for.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpId :: Lens.Lens' ListTagsForProject Lude.Text
ltfpId = Lens.lens (id :: ListTagsForProject -> Lude.Text) (\s a -> s {id = a} :: ListTagsForProject)
{-# DEPRECATED ltfpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest ListTagsForProject where
  type Rs ListTagsForProject = ListTagsForProjectResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForProjectResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.ListTagsForProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagsForProject where
  toJSON ListTagsForProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("id" Lude..= id)
          ]
      )

instance Lude.ToPath ListTagsForProject where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsForProjectResponse' smart constructor.
data ListTagsForProjectResponse = ListTagsForProjectResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
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

-- | Creates a value of 'ListTagsForProjectResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Reserved for future use.
-- * 'responseStatus' - The response status code.
-- * 'tags' - The tags for the project.
mkListTagsForProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForProjectResponse
mkListTagsForProjectResponse pResponseStatus_ =
  ListTagsForProjectResponse'
    { nextToken = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprsNextToken :: Lens.Lens' ListTagsForProjectResponse (Lude.Maybe Lude.Text)
ltfprsNextToken = Lens.lens (nextToken :: ListTagsForProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsForProjectResponse)
{-# DEPRECATED ltfprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The tags for the project.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprsTags :: Lens.Lens' ListTagsForProjectResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ltfprsTags = Lens.lens (tags :: ListTagsForProjectResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ListTagsForProjectResponse)
{-# DEPRECATED ltfprsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprsResponseStatus :: Lens.Lens' ListTagsForProjectResponse Lude.Int
ltfprsResponseStatus = Lens.lens (responseStatus :: ListTagsForProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForProjectResponse)
{-# DEPRECATED ltfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
