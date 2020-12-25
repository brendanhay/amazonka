{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeRemediationExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a detailed view of a Remediation Execution for a set of resources including state, timestamps for when steps for the remediation execution occur, and any error messages for steps that have failed. When you specify the limit and the next token, you receive a paginated response.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeRemediationExecutionStatus
  ( -- * Creating a request
    DescribeRemediationExecutionStatus (..),
    mkDescribeRemediationExecutionStatus,

    -- ** Request lenses
    dresConfigRuleName,
    dresLimit,
    dresNextToken,
    dresResourceKeys,

    -- * Destructuring the response
    DescribeRemediationExecutionStatusResponse (..),
    mkDescribeRemediationExecutionStatusResponse,

    -- ** Response lenses
    dresrrsNextToken,
    dresrrsRemediationExecutionStatuses,
    dresrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRemediationExecutionStatus' smart constructor.
data DescribeRemediationExecutionStatus = DescribeRemediationExecutionStatus'
  { -- | A list of AWS Config rule names.
    configRuleName :: Types.ConfigRuleName,
    -- | The maximum number of RemediationExecutionStatuses returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String,
    -- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
    resourceKeys :: Core.Maybe (Core.NonEmpty Types.ResourceKey)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRemediationExecutionStatus' value with any optional fields omitted.
mkDescribeRemediationExecutionStatus ::
  -- | 'configRuleName'
  Types.ConfigRuleName ->
  DescribeRemediationExecutionStatus
mkDescribeRemediationExecutionStatus configRuleName =
  DescribeRemediationExecutionStatus'
    { configRuleName,
      limit = Core.Nothing,
      nextToken = Core.Nothing,
      resourceKeys = Core.Nothing
    }

-- | A list of AWS Config rule names.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresConfigRuleName :: Lens.Lens' DescribeRemediationExecutionStatus Types.ConfigRuleName
dresConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED dresConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The maximum number of RemediationExecutionStatuses returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresLimit :: Lens.Lens' DescribeRemediationExecutionStatus (Core.Maybe Core.Natural)
dresLimit = Lens.field @"limit"
{-# DEPRECATED dresLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresNextToken :: Lens.Lens' DescribeRemediationExecutionStatus (Core.Maybe Types.String)
dresNextToken = Lens.field @"nextToken"
{-# DEPRECATED dresNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresResourceKeys :: Lens.Lens' DescribeRemediationExecutionStatus (Core.Maybe (Core.NonEmpty Types.ResourceKey))
dresResourceKeys = Lens.field @"resourceKeys"
{-# DEPRECATED dresResourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead." #-}

instance Core.FromJSON DescribeRemediationExecutionStatus where
  toJSON DescribeRemediationExecutionStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ConfigRuleName" Core..= configRuleName),
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ResourceKeys" Core..=) Core.<$> resourceKeys
          ]
      )

instance Core.AWSRequest DescribeRemediationExecutionStatus where
  type
    Rs DescribeRemediationExecutionStatus =
      DescribeRemediationExecutionStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DescribeRemediationExecutionStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRemediationExecutionStatusResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "RemediationExecutionStatuses")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeRemediationExecutionStatus where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"remediationExecutionStatuses" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeRemediationExecutionStatusResponse' smart constructor.
data DescribeRemediationExecutionStatusResponse = DescribeRemediationExecutionStatusResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Returns a list of remediation execution statuses objects.
    remediationExecutionStatuses :: Core.Maybe [Types.RemediationExecutionStatus],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeRemediationExecutionStatusResponse' value with any optional fields omitted.
mkDescribeRemediationExecutionStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeRemediationExecutionStatusResponse
mkDescribeRemediationExecutionStatusResponse responseStatus =
  DescribeRemediationExecutionStatusResponse'
    { nextToken =
        Core.Nothing,
      remediationExecutionStatuses = Core.Nothing,
      responseStatus
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresrrsNextToken :: Lens.Lens' DescribeRemediationExecutionStatusResponse (Core.Maybe Types.NextToken)
dresrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dresrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a list of remediation execution statuses objects.
--
-- /Note:/ Consider using 'remediationExecutionStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresrrsRemediationExecutionStatuses :: Lens.Lens' DescribeRemediationExecutionStatusResponse (Core.Maybe [Types.RemediationExecutionStatus])
dresrrsRemediationExecutionStatuses = Lens.field @"remediationExecutionStatuses"
{-# DEPRECATED dresrrsRemediationExecutionStatuses "Use generic-lens or generic-optics with 'remediationExecutionStatuses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresrrsResponseStatus :: Lens.Lens' DescribeRemediationExecutionStatusResponse Core.Int
dresrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dresrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
