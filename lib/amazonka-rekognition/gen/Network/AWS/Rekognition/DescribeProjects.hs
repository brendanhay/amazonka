{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DescribeProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists and gets information about your Amazon Rekognition Custom Labels projects.
--
-- This operation requires permissions to perform the @rekognition:DescribeProjects@ action.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.DescribeProjects
  ( -- * Creating a request
    DescribeProjects (..),
    mkDescribeProjects,

    -- ** Request lenses
    dpNextToken,
    dpMaxResults,

    -- * Destructuring the response
    DescribeProjectsResponse (..),
    mkDescribeProjectsResponse,

    -- ** Response lenses
    dpsrsNextToken,
    dpsrsProjectDescriptions,
    dpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeProjects' smart constructor.
data DescribeProjects = DescribeProjects'
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

-- | Creates a value of 'DescribeProjects' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return per paginated call. The largest value you can specify is 100. If you specify a value greater than 100, a ValidationException error occurs. The default value is 100.
-- * 'nextToken' - If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
mkDescribeProjects ::
  DescribeProjects
mkDescribeProjects =
  DescribeProjects'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpNextToken :: Lens.Lens' DescribeProjects (Lude.Maybe Lude.Text)
dpNextToken = Lens.lens (nextToken :: DescribeProjects -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeProjects)
{-# DEPRECATED dpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return per paginated call. The largest value you can specify is 100. If you specify a value greater than 100, a ValidationException error occurs. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMaxResults :: Lens.Lens' DescribeProjects (Lude.Maybe Lude.Natural)
dpMaxResults = Lens.lens (maxResults :: DescribeProjects -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeProjects)
{-# DEPRECATED dpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeProjects where
  page rq rs
    | Page.stop (rs Lens.^. dpsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dpsrsProjectDescriptions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpNextToken Lens..~ rs Lens.^. dpsrsNextToken

instance Lude.AWSRequest DescribeProjects where
  type Rs DescribeProjects = DescribeProjectsResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProjectsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ProjectDescriptions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProjects where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DescribeProjects" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProjects where
  toJSON DescribeProjects' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeProjects where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProjects where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProjectsResponse' smart constructor.
data DescribeProjectsResponse = DescribeProjectsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    projectDescriptions ::
      Lude.Maybe [ProjectDescription],
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

-- | Creates a value of 'DescribeProjectsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
-- * 'projectDescriptions' - A list of project descriptions. The list is sorted by the date and time the projects are created.
-- * 'responseStatus' - The response status code.
mkDescribeProjectsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProjectsResponse
mkDescribeProjectsResponse pResponseStatus_ =
  DescribeProjectsResponse'
    { nextToken = Lude.Nothing,
      projectDescriptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrsNextToken :: Lens.Lens' DescribeProjectsResponse (Lude.Maybe Lude.Text)
dpsrsNextToken = Lens.lens (nextToken :: DescribeProjectsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeProjectsResponse)
{-# DEPRECATED dpsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of project descriptions. The list is sorted by the date and time the projects are created.
--
-- /Note:/ Consider using 'projectDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrsProjectDescriptions :: Lens.Lens' DescribeProjectsResponse (Lude.Maybe [ProjectDescription])
dpsrsProjectDescriptions = Lens.lens (projectDescriptions :: DescribeProjectsResponse -> Lude.Maybe [ProjectDescription]) (\s a -> s {projectDescriptions = a} :: DescribeProjectsResponse)
{-# DEPRECATED dpsrsProjectDescriptions "Use generic-lens or generic-optics with 'projectDescriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrsResponseStatus :: Lens.Lens' DescribeProjectsResponse Lude.Int
dpsrsResponseStatus = Lens.lens (responseStatus :: DescribeProjectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProjectsResponse)
{-# DEPRECATED dpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
