{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListJobExecutionsForThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the job executions for the specified thing.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListJobExecutionsForThing
  ( -- * Creating a request
    ListJobExecutionsForThing (..),
    mkListJobExecutionsForThing,

    -- ** Request lenses
    ljeftStatus,
    ljeftNamespaceId,
    ljeftNextToken,
    ljeftThingName,
    ljeftMaxResults,

    -- * Destructuring the response
    ListJobExecutionsForThingResponse (..),
    mkListJobExecutionsForThingResponse,

    -- ** Response lenses
    ljeftrsExecutionSummaries,
    ljeftrsNextToken,
    ljeftrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListJobExecutionsForThing' smart constructor.
data ListJobExecutionsForThing = ListJobExecutionsForThing'
  { -- | An optional filter that lets you search for jobs that have the specified status.
    status :: Lude.Maybe JobExecutionStatus,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
    -- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
    namespaceId :: Lude.Maybe Lude.Text,
    -- | The token to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The thing name.
    thingName :: Lude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobExecutionsForThing' with the minimum fields required to make a request.
--
-- * 'status' - An optional filter that lets you search for jobs that have the specified status.
-- * 'namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
-- * 'nextToken' - The token to retrieve the next set of results.
-- * 'thingName' - The thing name.
-- * 'maxResults' - The maximum number of results to be returned per request.
mkListJobExecutionsForThing ::
  -- | 'thingName'
  Lude.Text ->
  ListJobExecutionsForThing
mkListJobExecutionsForThing pThingName_ =
  ListJobExecutionsForThing'
    { status = Lude.Nothing,
      namespaceId = Lude.Nothing,
      nextToken = Lude.Nothing,
      thingName = pThingName_,
      maxResults = Lude.Nothing
    }

-- | An optional filter that lets you search for jobs that have the specified status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftStatus :: Lens.Lens' ListJobExecutionsForThing (Lude.Maybe JobExecutionStatus)
ljeftStatus = Lens.lens (status :: ListJobExecutionsForThing -> Lude.Maybe JobExecutionStatus) (\s a -> s {status = a} :: ListJobExecutionsForThing)
{-# DEPRECATED ljeftStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftNamespaceId :: Lens.Lens' ListJobExecutionsForThing (Lude.Maybe Lude.Text)
ljeftNamespaceId = Lens.lens (namespaceId :: ListJobExecutionsForThing -> Lude.Maybe Lude.Text) (\s a -> s {namespaceId = a} :: ListJobExecutionsForThing)
{-# DEPRECATED ljeftNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftNextToken :: Lens.Lens' ListJobExecutionsForThing (Lude.Maybe Lude.Text)
ljeftNextToken = Lens.lens (nextToken :: ListJobExecutionsForThing -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobExecutionsForThing)
{-# DEPRECATED ljeftNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftThingName :: Lens.Lens' ListJobExecutionsForThing Lude.Text
ljeftThingName = Lens.lens (thingName :: ListJobExecutionsForThing -> Lude.Text) (\s a -> s {thingName = a} :: ListJobExecutionsForThing)
{-# DEPRECATED ljeftThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftMaxResults :: Lens.Lens' ListJobExecutionsForThing (Lude.Maybe Lude.Natural)
ljeftMaxResults = Lens.lens (maxResults :: ListJobExecutionsForThing -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListJobExecutionsForThing)
{-# DEPRECATED ljeftMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListJobExecutionsForThing where
  page rq rs
    | Page.stop (rs Lens.^. ljeftrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ljeftrsExecutionSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ljeftNextToken Lens..~ rs Lens.^. ljeftrsNextToken

instance Lude.AWSRequest ListJobExecutionsForThing where
  type
    Rs ListJobExecutionsForThing =
      ListJobExecutionsForThingResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListJobExecutionsForThingResponse'
            Lude.<$> (x Lude..?> "executionSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListJobExecutionsForThing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListJobExecutionsForThing where
  toPath ListJobExecutionsForThing' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName, "/jobs"]

instance Lude.ToQuery ListJobExecutionsForThing where
  toQuery ListJobExecutionsForThing' {..} =
    Lude.mconcat
      [ "status" Lude.=: status,
        "namespaceId" Lude.=: namespaceId,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListJobExecutionsForThingResponse' smart constructor.
data ListJobExecutionsForThingResponse = ListJobExecutionsForThingResponse'
  { -- | A list of job execution summaries.
    executionSummaries :: Lude.Maybe [JobExecutionSummaryForThing],
    -- | The token for the next set of results, or __null__ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobExecutionsForThingResponse' with the minimum fields required to make a request.
--
-- * 'executionSummaries' - A list of job execution summaries.
-- * 'nextToken' - The token for the next set of results, or __null__ if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListJobExecutionsForThingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListJobExecutionsForThingResponse
mkListJobExecutionsForThingResponse pResponseStatus_ =
  ListJobExecutionsForThingResponse'
    { executionSummaries =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of job execution summaries.
--
-- /Note:/ Consider using 'executionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftrsExecutionSummaries :: Lens.Lens' ListJobExecutionsForThingResponse (Lude.Maybe [JobExecutionSummaryForThing])
ljeftrsExecutionSummaries = Lens.lens (executionSummaries :: ListJobExecutionsForThingResponse -> Lude.Maybe [JobExecutionSummaryForThing]) (\s a -> s {executionSummaries = a} :: ListJobExecutionsForThingResponse)
{-# DEPRECATED ljeftrsExecutionSummaries "Use generic-lens or generic-optics with 'executionSummaries' instead." #-}

-- | The token for the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftrsNextToken :: Lens.Lens' ListJobExecutionsForThingResponse (Lude.Maybe Lude.Text)
ljeftrsNextToken = Lens.lens (nextToken :: ListJobExecutionsForThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobExecutionsForThingResponse)
{-# DEPRECATED ljeftrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftrsResponseStatus :: Lens.Lens' ListJobExecutionsForThingResponse Lude.Int
ljeftrsResponseStatus = Lens.lens (responseStatus :: ListJobExecutionsForThingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListJobExecutionsForThingResponse)
{-# DEPRECATED ljeftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
