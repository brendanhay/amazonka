{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackResourceDrifts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns drift information for the resources that have been checked for drift in the specified stack. This includes actual and expected configuration values for resources where AWS CloudFormation detects configuration drift.
--
-- For a given stack, there will be one @StackResourceDrift@ for each stack resource that has been checked for drift. Resources that have not yet been checked for drift are not included. Resources that do not currently support drift detection are not checked, and so not included. For a list of resources that support drift detection, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
-- Use 'DetectStackResourceDrift' to detect drift on individual resources, or 'DetectStackDrift' to detect drift on all supported resources for a given stack.
module Network.AWS.CloudFormation.DescribeStackResourceDrifts
  ( -- * Creating a request
    DescribeStackResourceDrifts (..),
    mkDescribeStackResourceDrifts,

    -- ** Request lenses
    dsrdStackName,
    dsrdMaxResults,
    dsrdNextToken,
    dsrdStackResourceDriftStatusFilters,

    -- * Destructuring the response
    DescribeStackResourceDriftsResponse (..),
    mkDescribeStackResourceDriftsResponse,

    -- ** Response lenses
    dsrdrrsStackResourceDrifts,
    dsrdrrsNextToken,
    dsrdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStackResourceDrifts' smart constructor.
data DescribeStackResourceDrifts = DescribeStackResourceDrifts'
  { -- | The name of the stack for which you want drift information.
    stackName :: Types.StackName,
    -- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string that identifies the next page of stack resource drift results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The resource drift status values to use as filters for the resource drift results returned.
    --
    --
    --     * @DELETED@ : The resource differs from its expected template configuration in that the resource has been deleted.
    --
    --
    --     * @MODIFIED@ : One or more resource properties differ from their expected template values.
    --
    --
    --     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.
    --
    --
    --     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
    stackResourceDriftStatusFilters :: Core.Maybe (Core.NonEmpty Types.StackResourceDriftStatus)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStackResourceDrifts' value with any optional fields omitted.
mkDescribeStackResourceDrifts ::
  -- | 'stackName'
  Types.StackName ->
  DescribeStackResourceDrifts
mkDescribeStackResourceDrifts stackName =
  DescribeStackResourceDrifts'
    { stackName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      stackResourceDriftStatusFilters = Core.Nothing
    }

-- | The name of the stack for which you want drift information.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdStackName :: Lens.Lens' DescribeStackResourceDrifts Types.StackName
dsrdStackName = Lens.field @"stackName"
{-# DEPRECATED dsrdStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdMaxResults :: Lens.Lens' DescribeStackResourceDrifts (Core.Maybe Core.Natural)
dsrdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dsrdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A string that identifies the next page of stack resource drift results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdNextToken :: Lens.Lens' DescribeStackResourceDrifts (Core.Maybe Types.NextToken)
dsrdNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsrdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The resource drift status values to use as filters for the resource drift results returned.
--
--
--     * @DELETED@ : The resource differs from its expected template configuration in that the resource has been deleted.
--
--
--     * @MODIFIED@ : One or more resource properties differ from their expected template values.
--
--
--     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
--
--
--
-- /Note:/ Consider using 'stackResourceDriftStatusFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdStackResourceDriftStatusFilters :: Lens.Lens' DescribeStackResourceDrifts (Core.Maybe (Core.NonEmpty Types.StackResourceDriftStatus))
dsrdStackResourceDriftStatusFilters = Lens.field @"stackResourceDriftStatusFilters"
{-# DEPRECATED dsrdStackResourceDriftStatusFilters "Use generic-lens or generic-optics with 'stackResourceDriftStatusFilters' instead." #-}

instance Core.AWSRequest DescribeStackResourceDrifts where
  type
    Rs DescribeStackResourceDrifts =
      DescribeStackResourceDriftsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeStackResourceDrifts")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackName" stackName)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryValue
                            "StackResourceDriftStatusFilters"
                            ( Core.toQueryList "member"
                                Core.<$> stackResourceDriftStatusFilters
                            )
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeStackResourceDriftsResult"
      ( \s h x ->
          DescribeStackResourceDriftsResponse'
            Core.<$> ( x Core..@? "StackResourceDrifts" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeStackResourceDriftsResponse' smart constructor.
data DescribeStackResourceDriftsResponse = DescribeStackResourceDriftsResponse'
  { -- | Drift information for the resources that have been checked for drift in the specified stack. This includes actual and expected configuration values for resources where AWS CloudFormation detects drift.
    --
    -- For a given stack, there will be one @StackResourceDrift@ for each stack resource that has been checked for drift. Resources that have not yet been checked for drift are not included. Resources that do not currently support drift detection are not checked, and so not included. For a list of resources that support drift detection, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
    stackResourceDrifts :: [Types.StackResourceDrift],
    -- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @DescribeStackResourceDrifts@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStackResourceDriftsResponse' value with any optional fields omitted.
mkDescribeStackResourceDriftsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStackResourceDriftsResponse
mkDescribeStackResourceDriftsResponse responseStatus =
  DescribeStackResourceDriftsResponse'
    { stackResourceDrifts =
        Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Drift information for the resources that have been checked for drift in the specified stack. This includes actual and expected configuration values for resources where AWS CloudFormation detects drift.
--
-- For a given stack, there will be one @StackResourceDrift@ for each stack resource that has been checked for drift. Resources that have not yet been checked for drift are not included. Resources that do not currently support drift detection are not checked, and so not included. For a list of resources that support drift detection, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
--
-- /Note:/ Consider using 'stackResourceDrifts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdrrsStackResourceDrifts :: Lens.Lens' DescribeStackResourceDriftsResponse [Types.StackResourceDrift]
dsrdrrsStackResourceDrifts = Lens.field @"stackResourceDrifts"
{-# DEPRECATED dsrdrrsStackResourceDrifts "Use generic-lens or generic-optics with 'stackResourceDrifts' instead." #-}

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @DescribeStackResourceDrifts@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdrrsNextToken :: Lens.Lens' DescribeStackResourceDriftsResponse (Core.Maybe Types.NextToken)
dsrdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsrdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrdrrsResponseStatus :: Lens.Lens' DescribeStackResourceDriftsResponse Core.Int
dsrdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
