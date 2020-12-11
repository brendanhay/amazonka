{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about projects.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListProjects
  ( -- * Creating a request
    ListProjects (..),
    mkListProjects,

    -- ** Request lenses
    lpArn,
    lpNextToken,

    -- * Destructuring the response
    ListProjectsResponse (..),
    mkListProjectsResponse,

    -- ** Response lenses
    lprsNextToken,
    lprsProjects,
    lprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the list projects operation.
--
-- /See:/ 'mkListProjects' smart constructor.
data ListProjects = ListProjects'
  { arn :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListProjects' with the minimum fields required to make a request.
--
-- * 'arn' - Optional. If no Amazon Resource Name (ARN) is specified, then AWS Device Farm returns a list of all projects for the AWS account. You can also specify a project ARN.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListProjects ::
  ListProjects
mkListProjects =
  ListProjects' {arn = Lude.Nothing, nextToken = Lude.Nothing}

-- | Optional. If no Amazon Resource Name (ARN) is specified, then AWS Device Farm returns a list of all projects for the AWS account. You can also specify a project ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpArn :: Lens.Lens' ListProjects (Lude.Maybe Lude.Text)
lpArn = Lens.lens (arn :: ListProjects -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ListProjects)
{-# DEPRECATED lpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListProjects (Lude.Maybe Lude.Text)
lpNextToken = Lens.lens (nextToken :: ListProjects -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProjects)
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

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
  request = Req.postJSON deviceFarmService
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
              Lude.=# ("DeviceFarm_20150623.ListProjects" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListProjects where
  toJSON ListProjects' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("arn" Lude..=) Lude.<$> arn,
            ("nextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath ListProjects where
  toPath = Lude.const "/"

instance Lude.ToQuery ListProjects where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a list projects request.
--
-- /See:/ 'mkListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    projects :: Lude.Maybe [Project],
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
-- * 'nextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
-- * 'projects' - Information about the projects.
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

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' ListProjectsResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: ListProjectsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProjectsResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the projects.
--
-- /Note:/ Consider using 'projects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsProjects :: Lens.Lens' ListProjectsResponse (Lude.Maybe [Project])
lprsProjects = Lens.lens (projects :: ListProjectsResponse -> Lude.Maybe [Project]) (\s a -> s {projects = a} :: ListProjectsResponse)
{-# DEPRECATED lprsProjects "Use generic-lens or generic-optics with 'projects' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListProjectsResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListProjectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProjectsResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
