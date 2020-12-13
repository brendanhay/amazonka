{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all active group details.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetGroups
  ( -- * Creating a request
    GetGroups (..),
    mkGetGroups,

    -- ** Request lenses
    ggNextToken,

    -- * Destructuring the response
    GetGroupsResponse (..),
    mkGetGroupsResponse,

    -- ** Response lenses
    grsGroups,
    grsNextToken,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetGroups' smart constructor.
newtype GetGroups = GetGroups'
  { -- | Pagination token.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroups' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token.
mkGetGroups ::
  GetGroups
mkGetGroups = GetGroups' {nextToken = Lude.Nothing}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggNextToken :: Lens.Lens' GetGroups (Lude.Maybe Lude.Text)
ggNextToken = Lens.lens (nextToken :: GetGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetGroups)
{-# DEPRECATED ggNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager GetGroups where
  page rq rs
    | Page.stop (rs Lens.^. grsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grsGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ggNextToken Lens..~ rs Lens.^. grsNextToken

instance Lude.AWSRequest GetGroups where
  type Rs GetGroups = GetGroupsResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGroupsResponse'
            Lude.<$> (x Lude..?> "Groups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetGroups where
  toJSON GetGroups' {..} =
    Lude.object
      (Lude.catMaybes [("NextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath GetGroups where
  toPath = Lude.const "/Groups"

instance Lude.ToQuery GetGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGroupsResponse' smart constructor.
data GetGroupsResponse = GetGroupsResponse'
  { -- | The collection of all active groups.
    groups :: Lude.Maybe [GroupSummary],
    -- | Pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupsResponse' with the minimum fields required to make a request.
--
-- * 'groups' - The collection of all active groups.
-- * 'nextToken' - Pagination token.
-- * 'responseStatus' - The response status code.
mkGetGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGroupsResponse
mkGetGroupsResponse pResponseStatus_ =
  GetGroupsResponse'
    { groups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The collection of all active groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsGroups :: Lens.Lens' GetGroupsResponse (Lude.Maybe [GroupSummary])
grsGroups = Lens.lens (groups :: GetGroupsResponse -> Lude.Maybe [GroupSummary]) (\s a -> s {groups = a} :: GetGroupsResponse)
{-# DEPRECATED grsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsNextToken :: Lens.Lens' GetGroupsResponse (Lude.Maybe Lude.Text)
grsNextToken = Lens.lens (nextToken :: GetGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetGroupsResponse)
{-# DEPRECATED grsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetGroupsResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGroupsResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
