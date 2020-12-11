{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists jobs.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListJobs
  ( -- * Creating a request
    ListJobs (..),
    mkListJobs,

    -- ** Request lenses
    ljStatus,
    ljThingGroupId,
    ljNamespaceId,
    ljNextToken,
    ljThingGroupName,
    ljMaxResults,
    ljTargetSelection,

    -- * Destructuring the response
    ListJobsResponse (..),
    mkListJobsResponse,

    -- ** Response lenses
    ljrsJobs,
    ljrsNextToken,
    ljrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { status :: Lude.Maybe JobStatus,
    thingGroupId :: Lude.Maybe Lude.Text,
    namespaceId :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    thingGroupName :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    targetSelection :: Lude.Maybe TargetSelection
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
-- * 'maxResults' - The maximum number of results to return per request.
-- * 'namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
-- * 'nextToken' - The token to retrieve the next set of results.
-- * 'status' - An optional filter that lets you search for jobs that have the specified status.
-- * 'targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
-- * 'thingGroupId' - A filter that limits the returned jobs to those for the specified group.
-- * 'thingGroupName' - A filter that limits the returned jobs to those for the specified group.
mkListJobs ::
  ListJobs
mkListJobs =
  ListJobs'
    { status = Lude.Nothing,
      thingGroupId = Lude.Nothing,
      namespaceId = Lude.Nothing,
      nextToken = Lude.Nothing,
      thingGroupName = Lude.Nothing,
      maxResults = Lude.Nothing,
      targetSelection = Lude.Nothing
    }

-- | An optional filter that lets you search for jobs that have the specified status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljStatus :: Lens.Lens' ListJobs (Lude.Maybe JobStatus)
ljStatus = Lens.lens (status :: ListJobs -> Lude.Maybe JobStatus) (\s a -> s {status = a} :: ListJobs)
{-# DEPRECATED ljStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A filter that limits the returned jobs to those for the specified group.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljThingGroupId :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljThingGroupId = Lens.lens (thingGroupId :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupId = a} :: ListJobs)
{-# DEPRECATED ljThingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNamespaceId :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljNamespaceId = Lens.lens (namespaceId :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {namespaceId = a} :: ListJobs)
{-# DEPRECATED ljNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNextToken :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljNextToken = Lens.lens (nextToken :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobs)
{-# DEPRECATED ljNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A filter that limits the returned jobs to those for the specified group.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljThingGroupName :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljThingGroupName = Lens.lens (thingGroupName :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupName = a} :: ListJobs)
{-# DEPRECATED ljThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The maximum number of results to return per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMaxResults :: Lens.Lens' ListJobs (Lude.Maybe Lude.Natural)
ljMaxResults = Lens.lens (maxResults :: ListJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListJobs)
{-# DEPRECATED ljMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
--
-- /Note:/ Consider using 'targetSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljTargetSelection :: Lens.Lens' ListJobs (Lude.Maybe TargetSelection)
ljTargetSelection = Lens.lens (targetSelection :: ListJobs -> Lude.Maybe TargetSelection) (\s a -> s {targetSelection = a} :: ListJobs)
{-# DEPRECATED ljTargetSelection "Use generic-lens or generic-optics with 'targetSelection' instead." #-}

instance Page.AWSPager ListJobs where
  page rq rs
    | Page.stop (rs Lens.^. ljrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ljrsJobs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ljNextToken Lens..~ rs Lens.^. ljrsNextToken

instance Lude.AWSRequest ListJobs where
  type Rs ListJobs = ListJobsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Lude.<$> (x Lude..?> "jobs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListJobs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListJobs where
  toPath = Lude.const "/jobs"

instance Lude.ToQuery ListJobs where
  toQuery ListJobs' {..} =
    Lude.mconcat
      [ "status" Lude.=: status,
        "thingGroupId" Lude.=: thingGroupId,
        "namespaceId" Lude.=: namespaceId,
        "nextToken" Lude.=: nextToken,
        "thingGroupName" Lude.=: thingGroupName,
        "maxResults" Lude.=: maxResults,
        "targetSelection" Lude.=: targetSelection
      ]

-- | /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { jobs ::
      Lude.Maybe [JobSummary],
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

-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- * 'jobs' - A list of jobs.
-- * 'nextToken' - The token for the next set of results, or __null__ if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListJobsResponse
mkListJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { jobs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of jobs.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsJobs :: Lens.Lens' ListJobsResponse (Lude.Maybe [JobSummary])
ljrsJobs = Lens.lens (jobs :: ListJobsResponse -> Lude.Maybe [JobSummary]) (\s a -> s {jobs = a} :: ListJobsResponse)
{-# DEPRECATED ljrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The token for the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsNextToken :: Lens.Lens' ListJobsResponse (Lude.Maybe Lude.Text)
ljrsNextToken = Lens.lens (nextToken :: ListJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobsResponse)
{-# DEPRECATED ljrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsResponseStatus :: Lens.Lens' ListJobsResponse Lude.Int
ljrsResponseStatus = Lens.lens (responseStatus :: ListJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListJobsResponse)
{-# DEPRECATED ljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
