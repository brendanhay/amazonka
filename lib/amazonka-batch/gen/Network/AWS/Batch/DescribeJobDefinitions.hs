{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.DescribeJobDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a list of job definitions. You can specify a @status@ (such as @ACTIVE@ ) to only return job definitions that match that status.
--
-- This operation returns paginated results.
module Network.AWS.Batch.DescribeJobDefinitions
  ( -- * Creating a request
    DescribeJobDefinitions (..),
    mkDescribeJobDefinitions,

    -- ** Request lenses
    djdJobDefinitionName,
    djdJobDefinitions,
    djdMaxResults,
    djdNextToken,
    djdStatus,

    -- * Destructuring the response
    DescribeJobDefinitionsResponse (..),
    mkDescribeJobDefinitionsResponse,

    -- ** Response lenses
    djdrrsJobDefinitions,
    djdrrsNextToken,
    djdrrsResponseStatus,
  )
where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeJobDefinitions' smart constructor.
data DescribeJobDefinitions = DescribeJobDefinitions'
  { -- | The name of the job definition to describe.
    jobDefinitionName :: Core.Maybe Types.String,
    -- | A list of up to 100 job definition names or full Amazon Resource Name (ARN) entries.
    jobDefinitions :: Core.Maybe [Types.String],
    -- | The maximum number of results returned by @DescribeJobDefinitions@ in paginated output. When this parameter is used, @DescribeJobDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Core.Maybe Core.Int,
    -- | The @nextToken@ value returned from a previous paginated @DescribeJobDefinitions@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The status with which to filter job definitions.
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeJobDefinitions' value with any optional fields omitted.
mkDescribeJobDefinitions ::
  DescribeJobDefinitions
mkDescribeJobDefinitions =
  DescribeJobDefinitions'
    { jobDefinitionName = Core.Nothing,
      jobDefinitions = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing
    }

-- | The name of the job definition to describe.
--
-- /Note:/ Consider using 'jobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdJobDefinitionName :: Lens.Lens' DescribeJobDefinitions (Core.Maybe Types.String)
djdJobDefinitionName = Lens.field @"jobDefinitionName"
{-# DEPRECATED djdJobDefinitionName "Use generic-lens or generic-optics with 'jobDefinitionName' instead." #-}

-- | A list of up to 100 job definition names or full Amazon Resource Name (ARN) entries.
--
-- /Note:/ Consider using 'jobDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdJobDefinitions :: Lens.Lens' DescribeJobDefinitions (Core.Maybe [Types.String])
djdJobDefinitions = Lens.field @"jobDefinitions"
{-# DEPRECATED djdJobDefinitions "Use generic-lens or generic-optics with 'jobDefinitions' instead." #-}

-- | The maximum number of results returned by @DescribeJobDefinitions@ in paginated output. When this parameter is used, @DescribeJobDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdMaxResults :: Lens.Lens' DescribeJobDefinitions (Core.Maybe Core.Int)
djdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED djdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeJobDefinitions@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdNextToken :: Lens.Lens' DescribeJobDefinitions (Core.Maybe Types.String)
djdNextToken = Lens.field @"nextToken"
{-# DEPRECATED djdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The status with which to filter job definitions.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdStatus :: Lens.Lens' DescribeJobDefinitions (Core.Maybe Types.String)
djdStatus = Lens.field @"status"
{-# DEPRECATED djdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON DescribeJobDefinitions where
  toJSON DescribeJobDefinitions {..} =
    Core.object
      ( Core.catMaybes
          [ ("jobDefinitionName" Core..=) Core.<$> jobDefinitionName,
            ("jobDefinitions" Core..=) Core.<$> jobDefinitions,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("status" Core..=) Core.<$> status
          ]
      )

instance Core.AWSRequest DescribeJobDefinitions where
  type Rs DescribeJobDefinitions = DescribeJobDefinitionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/describejobdefinitions",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobDefinitionsResponse'
            Core.<$> (x Core..:? "jobDefinitions")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeJobDefinitions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"jobDefinitions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeJobDefinitionsResponse' smart constructor.
data DescribeJobDefinitionsResponse = DescribeJobDefinitionsResponse'
  { -- | The list of job definitions.
    jobDefinitions :: Core.Maybe [Types.JobDefinition],
    -- | The @nextToken@ value to include in a future @DescribeJobDefinitions@ request. When the results of a @DescribeJobDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeJobDefinitionsResponse' value with any optional fields omitted.
mkDescribeJobDefinitionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeJobDefinitionsResponse
mkDescribeJobDefinitionsResponse responseStatus =
  DescribeJobDefinitionsResponse'
    { jobDefinitions = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of job definitions.
--
-- /Note:/ Consider using 'jobDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdrrsJobDefinitions :: Lens.Lens' DescribeJobDefinitionsResponse (Core.Maybe [Types.JobDefinition])
djdrrsJobDefinitions = Lens.field @"jobDefinitions"
{-# DEPRECATED djdrrsJobDefinitions "Use generic-lens or generic-optics with 'jobDefinitions' instead." #-}

-- | The @nextToken@ value to include in a future @DescribeJobDefinitions@ request. When the results of a @DescribeJobDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdrrsNextToken :: Lens.Lens' DescribeJobDefinitionsResponse (Core.Maybe Types.String)
djdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED djdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdrrsResponseStatus :: Lens.Lens' DescribeJobDefinitionsResponse Core.Int
djdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED djdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
