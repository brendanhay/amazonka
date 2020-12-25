{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeQueryDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns a paginated list of your saved CloudWatch Logs Insights query definitions.
--
-- You can use the @queryDefinitionNamePrefix@ parameter to limit the results to only the query definitions that have names that start with a certain string.
module Network.AWS.CloudWatchLogs.DescribeQueryDefinitions
  ( -- * Creating a request
    DescribeQueryDefinitions (..),
    mkDescribeQueryDefinitions,

    -- ** Request lenses
    dqdMaxResults,
    dqdNextToken,
    dqdQueryDefinitionNamePrefix,

    -- * Destructuring the response
    DescribeQueryDefinitionsResponse (..),
    mkDescribeQueryDefinitionsResponse,

    -- ** Response lenses
    dqdrrsNextToken,
    dqdrrsQueryDefinitions,
    dqdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeQueryDefinitions' smart constructor.
data DescribeQueryDefinitions = DescribeQueryDefinitions'
  { -- | Limits the number of returned query definitions to the specified number.
    maxResults :: Core.Maybe Core.Natural,
    nextToken :: Core.Maybe Types.NextToken,
    -- | Use this parameter to filter your results to only the query definitions that have names that start with the prefix you specify.
    queryDefinitionNamePrefix :: Core.Maybe Types.QueryDefinitionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeQueryDefinitions' value with any optional fields omitted.
mkDescribeQueryDefinitions ::
  DescribeQueryDefinitions
mkDescribeQueryDefinitions =
  DescribeQueryDefinitions'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      queryDefinitionNamePrefix = Core.Nothing
    }

-- | Limits the number of returned query definitions to the specified number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdMaxResults :: Lens.Lens' DescribeQueryDefinitions (Core.Maybe Core.Natural)
dqdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dqdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdNextToken :: Lens.Lens' DescribeQueryDefinitions (Core.Maybe Types.NextToken)
dqdNextToken = Lens.field @"nextToken"
{-# DEPRECATED dqdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Use this parameter to filter your results to only the query definitions that have names that start with the prefix you specify.
--
-- /Note:/ Consider using 'queryDefinitionNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdQueryDefinitionNamePrefix :: Lens.Lens' DescribeQueryDefinitions (Core.Maybe Types.QueryDefinitionName)
dqdQueryDefinitionNamePrefix = Lens.field @"queryDefinitionNamePrefix"
{-# DEPRECATED dqdQueryDefinitionNamePrefix "Use generic-lens or generic-optics with 'queryDefinitionNamePrefix' instead." #-}

instance Core.FromJSON DescribeQueryDefinitions where
  toJSON DescribeQueryDefinitions {..} =
    Core.object
      ( Core.catMaybes
          [ ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("queryDefinitionNamePrefix" Core..=)
              Core.<$> queryDefinitionNamePrefix
          ]
      )

instance Core.AWSRequest DescribeQueryDefinitions where
  type Rs DescribeQueryDefinitions = DescribeQueryDefinitionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Logs_20140328.DescribeQueryDefinitions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeQueryDefinitionsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "queryDefinitions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeQueryDefinitionsResponse' smart constructor.
data DescribeQueryDefinitionsResponse = DescribeQueryDefinitionsResponse'
  { nextToken :: Core.Maybe Types.NextToken,
    -- | The list of query definitions that match your request.
    queryDefinitions :: Core.Maybe [Types.QueryDefinition],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeQueryDefinitionsResponse' value with any optional fields omitted.
mkDescribeQueryDefinitionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeQueryDefinitionsResponse
mkDescribeQueryDefinitionsResponse responseStatus =
  DescribeQueryDefinitionsResponse'
    { nextToken = Core.Nothing,
      queryDefinitions = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdrrsNextToken :: Lens.Lens' DescribeQueryDefinitionsResponse (Core.Maybe Types.NextToken)
dqdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dqdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of query definitions that match your request.
--
-- /Note:/ Consider using 'queryDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdrrsQueryDefinitions :: Lens.Lens' DescribeQueryDefinitionsResponse (Core.Maybe [Types.QueryDefinition])
dqdrrsQueryDefinitions = Lens.field @"queryDefinitions"
{-# DEPRECATED dqdrrsQueryDefinitions "Use generic-lens or generic-optics with 'queryDefinitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdrrsResponseStatus :: Lens.Lens' DescribeQueryDefinitionsResponse Core.Int
dqdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dqdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
