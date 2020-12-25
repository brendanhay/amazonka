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
    ljeftThingName,
    ljeftMaxResults,
    ljeftNamespaceId,
    ljeftNextToken,
    ljeftStatus,

    -- * Destructuring the response
    ListJobExecutionsForThingResponse (..),
    mkListJobExecutionsForThingResponse,

    -- ** Response lenses
    ljeftrrsExecutionSummaries,
    ljeftrrsNextToken,
    ljeftrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListJobExecutionsForThing' smart constructor.
data ListJobExecutionsForThing = ListJobExecutionsForThing'
  { -- | The thing name.
    thingName :: Types.ThingName,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Natural,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
    -- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
    namespaceId :: Core.Maybe Types.NamespaceId,
    -- | The token to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | An optional filter that lets you search for jobs that have the specified status.
    status :: Core.Maybe Types.JobExecutionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobExecutionsForThing' value with any optional fields omitted.
mkListJobExecutionsForThing ::
  -- | 'thingName'
  Types.ThingName ->
  ListJobExecutionsForThing
mkListJobExecutionsForThing thingName =
  ListJobExecutionsForThing'
    { thingName,
      maxResults = Core.Nothing,
      namespaceId = Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing
    }

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftThingName :: Lens.Lens' ListJobExecutionsForThing Types.ThingName
ljeftThingName = Lens.field @"thingName"
{-# DEPRECATED ljeftThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftMaxResults :: Lens.Lens' ListJobExecutionsForThing (Core.Maybe Core.Natural)
ljeftMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ljeftMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftNamespaceId :: Lens.Lens' ListJobExecutionsForThing (Core.Maybe Types.NamespaceId)
ljeftNamespaceId = Lens.field @"namespaceId"
{-# DEPRECATED ljeftNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftNextToken :: Lens.Lens' ListJobExecutionsForThing (Core.Maybe Types.NextToken)
ljeftNextToken = Lens.field @"nextToken"
{-# DEPRECATED ljeftNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An optional filter that lets you search for jobs that have the specified status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftStatus :: Lens.Lens' ListJobExecutionsForThing (Core.Maybe Types.JobExecutionStatus)
ljeftStatus = Lens.field @"status"
{-# DEPRECATED ljeftStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.AWSRequest ListJobExecutionsForThing where
  type
    Rs ListJobExecutionsForThing =
      ListJobExecutionsForThingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/things/" Core.<> (Core.toText thingName) Core.<> ("/jobs")),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "namespaceId" Core.<$> namespaceId)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "status" Core.<$> status),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobExecutionsForThingResponse'
            Core.<$> (x Core..:? "executionSummaries")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListJobExecutionsForThing where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"executionSummaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListJobExecutionsForThingResponse' smart constructor.
data ListJobExecutionsForThingResponse = ListJobExecutionsForThingResponse'
  { -- | A list of job execution summaries.
    executionSummaries :: Core.Maybe [Types.JobExecutionSummaryForThing],
    -- | The token for the next set of results, or __null__ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListJobExecutionsForThingResponse' value with any optional fields omitted.
mkListJobExecutionsForThingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListJobExecutionsForThingResponse
mkListJobExecutionsForThingResponse responseStatus =
  ListJobExecutionsForThingResponse'
    { executionSummaries =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of job execution summaries.
--
-- /Note:/ Consider using 'executionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftrrsExecutionSummaries :: Lens.Lens' ListJobExecutionsForThingResponse (Core.Maybe [Types.JobExecutionSummaryForThing])
ljeftrrsExecutionSummaries = Lens.field @"executionSummaries"
{-# DEPRECATED ljeftrrsExecutionSummaries "Use generic-lens or generic-optics with 'executionSummaries' instead." #-}

-- | The token for the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftrrsNextToken :: Lens.Lens' ListJobExecutionsForThingResponse (Core.Maybe Types.NextToken)
ljeftrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ljeftrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftrrsResponseStatus :: Lens.Lens' ListJobExecutionsForThingResponse Core.Int
ljeftrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ljeftrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
