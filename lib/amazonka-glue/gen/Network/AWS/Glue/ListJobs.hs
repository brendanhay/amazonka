{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all job resources in this AWS account, or the resources with the specified tag. This operation allows you to see which resources are available in your account, and their names.
--
-- This operation takes the optional @Tags@ field, which you can use as a filter on the response so that tagged resources can be retrieved as a group. If you choose to use tags filtering, only resources with the tag are retrieved.
module Network.AWS.Glue.ListJobs
  ( -- * Creating a request
    ListJobs (..),
    mkListJobs,

    -- ** Request lenses
    ljNextToken,
    ljMaxResults,
    ljTags,

    -- * Destructuring the response
    ListJobsResponse (..),
    mkListJobsResponse,

    -- ** Response lenses
    ljrsNextToken,
    ljrsJobNames,
    ljrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum size of a list to return.
-- * 'nextToken' - A continuation token, if this is a continuation request.
-- * 'tags' - Specifies to return only these tagged resources.
mkListJobs ::
  ListJobs
mkListJobs =
  ListJobs'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNextToken :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljNextToken = Lens.lens (nextToken :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobs)
{-# DEPRECATED ljNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMaxResults :: Lens.Lens' ListJobs (Lude.Maybe Lude.Natural)
ljMaxResults = Lens.lens (maxResults :: ListJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListJobs)
{-# DEPRECATED ljMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specifies to return only these tagged resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljTags :: Lens.Lens' ListJobs (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ljTags = Lens.lens (tags :: ListJobs -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ListJobs)
{-# DEPRECATED ljTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest ListJobs where
  type Rs ListJobs = ListJobsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "JobNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.ListJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListJobs where
  toJSON ListJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath ListJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    jobNames :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- * 'jobNames' - The names of all jobs in the account, or the jobs with the specified tags.
-- * 'nextToken' - A continuation token, if the returned list does not contain the last metric available.
-- * 'responseStatus' - The response status code.
mkListJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListJobsResponse
mkListJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { nextToken = Lude.Nothing,
      jobNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsNextToken :: Lens.Lens' ListJobsResponse (Lude.Maybe Lude.Text)
ljrsNextToken = Lens.lens (nextToken :: ListJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobsResponse)
{-# DEPRECATED ljrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of all jobs in the account, or the jobs with the specified tags.
--
-- /Note:/ Consider using 'jobNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsJobNames :: Lens.Lens' ListJobsResponse (Lude.Maybe [Lude.Text])
ljrsJobNames = Lens.lens (jobNames :: ListJobsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {jobNames = a} :: ListJobsResponse)
{-# DEPRECATED ljrsJobNames "Use generic-lens or generic-optics with 'jobNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsResponseStatus :: Lens.Lens' ListJobsResponse Lude.Int
ljrsResponseStatus = Lens.lens (responseStatus :: ListJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListJobsResponse)
{-# DEPRECATED ljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
