{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.CountClosedWorkflowExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of closed workflow executions within the given domain that meet the specified filtering criteria.
--
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @tagFilter.tag@ : String constraint. The key is @swf:tagFilter.tag@ .
--
--
--     * @typeFilter.name@ : String constraint. The key is @swf:typeFilter.name@ .
--
--
--     * @typeFilter.version@ : String constraint. The key is @swf:typeFilter.version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.CountClosedWorkflowExecutions
  ( -- * Creating a request
    CountClosedWorkflowExecutions (..),
    mkCountClosedWorkflowExecutions,

    -- ** Request lenses
    ccweDomain,
    ccweCloseStatusFilter,
    ccweCloseTimeFilter,
    ccweExecutionFilter,
    ccweStartTimeFilter,
    ccweTagFilter,
    ccweTypeFilter,

    -- * Destructuring the response
    Types.WorkflowExecutionCount (..),
    Types.mkWorkflowExecutionCount,

    -- ** Response lenses
    Types.wecCount,
    Types.wecTruncated,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkCountClosedWorkflowExecutions' smart constructor.
data CountClosedWorkflowExecutions = CountClosedWorkflowExecutions'
  { -- | The name of the domain containing the workflow executions to count.
    domain :: Types.DomainName,
    -- | If specified, only workflow executions that match this close status are counted. This filter has an affect only if @executionStatus@ is specified as @CLOSED@ .
    closeStatusFilter :: Core.Maybe Types.CloseStatusFilter,
    -- | If specified, only workflow executions that meet the close time criteria of the filter are counted.
    closeTimeFilter :: Core.Maybe Types.ExecutionTimeFilter,
    -- | If specified, only workflow executions matching the @WorkflowId@ in the filter are counted.
    executionFilter :: Core.Maybe Types.WorkflowExecutionFilter,
    -- | If specified, only workflow executions that meet the start time criteria of the filter are counted.
    startTimeFilter :: Core.Maybe Types.ExecutionTimeFilter,
    -- | If specified, only executions that have a tag that matches the filter are counted.
    tagFilter :: Core.Maybe Types.TagFilter,
    -- | If specified, indicates the type of the workflow executions to be counted.
    typeFilter :: Core.Maybe Types.WorkflowTypeFilter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CountClosedWorkflowExecutions' value with any optional fields omitted.
mkCountClosedWorkflowExecutions ::
  -- | 'domain'
  Types.DomainName ->
  CountClosedWorkflowExecutions
mkCountClosedWorkflowExecutions domain =
  CountClosedWorkflowExecutions'
    { domain,
      closeStatusFilter = Core.Nothing,
      closeTimeFilter = Core.Nothing,
      executionFilter = Core.Nothing,
      startTimeFilter = Core.Nothing,
      tagFilter = Core.Nothing,
      typeFilter = Core.Nothing
    }

-- | The name of the domain containing the workflow executions to count.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweDomain :: Lens.Lens' CountClosedWorkflowExecutions Types.DomainName
ccweDomain = Lens.field @"domain"
{-# DEPRECATED ccweDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | If specified, only workflow executions that match this close status are counted. This filter has an affect only if @executionStatus@ is specified as @CLOSED@ .
--
-- /Note:/ Consider using 'closeStatusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweCloseStatusFilter :: Lens.Lens' CountClosedWorkflowExecutions (Core.Maybe Types.CloseStatusFilter)
ccweCloseStatusFilter = Lens.field @"closeStatusFilter"
{-# DEPRECATED ccweCloseStatusFilter "Use generic-lens or generic-optics with 'closeStatusFilter' instead." #-}

-- | If specified, only workflow executions that meet the close time criteria of the filter are counted.
--
-- /Note:/ Consider using 'closeTimeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweCloseTimeFilter :: Lens.Lens' CountClosedWorkflowExecutions (Core.Maybe Types.ExecutionTimeFilter)
ccweCloseTimeFilter = Lens.field @"closeTimeFilter"
{-# DEPRECATED ccweCloseTimeFilter "Use generic-lens or generic-optics with 'closeTimeFilter' instead." #-}

-- | If specified, only workflow executions matching the @WorkflowId@ in the filter are counted.
--
-- /Note:/ Consider using 'executionFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweExecutionFilter :: Lens.Lens' CountClosedWorkflowExecutions (Core.Maybe Types.WorkflowExecutionFilter)
ccweExecutionFilter = Lens.field @"executionFilter"
{-# DEPRECATED ccweExecutionFilter "Use generic-lens or generic-optics with 'executionFilter' instead." #-}

-- | If specified, only workflow executions that meet the start time criteria of the filter are counted.
--
-- /Note:/ Consider using 'startTimeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweStartTimeFilter :: Lens.Lens' CountClosedWorkflowExecutions (Core.Maybe Types.ExecutionTimeFilter)
ccweStartTimeFilter = Lens.field @"startTimeFilter"
{-# DEPRECATED ccweStartTimeFilter "Use generic-lens or generic-optics with 'startTimeFilter' instead." #-}

-- | If specified, only executions that have a tag that matches the filter are counted.
--
-- /Note:/ Consider using 'tagFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweTagFilter :: Lens.Lens' CountClosedWorkflowExecutions (Core.Maybe Types.TagFilter)
ccweTagFilter = Lens.field @"tagFilter"
{-# DEPRECATED ccweTagFilter "Use generic-lens or generic-optics with 'tagFilter' instead." #-}

-- | If specified, indicates the type of the workflow executions to be counted.
--
-- /Note:/ Consider using 'typeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweTypeFilter :: Lens.Lens' CountClosedWorkflowExecutions (Core.Maybe Types.WorkflowTypeFilter)
ccweTypeFilter = Lens.field @"typeFilter"
{-# DEPRECATED ccweTypeFilter "Use generic-lens or generic-optics with 'typeFilter' instead." #-}

instance Core.FromJSON CountClosedWorkflowExecutions where
  toJSON CountClosedWorkflowExecutions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            ("closeStatusFilter" Core..=) Core.<$> closeStatusFilter,
            ("closeTimeFilter" Core..=) Core.<$> closeTimeFilter,
            ("executionFilter" Core..=) Core.<$> executionFilter,
            ("startTimeFilter" Core..=) Core.<$> startTimeFilter,
            ("tagFilter" Core..=) Core.<$> tagFilter,
            ("typeFilter" Core..=) Core.<$> typeFilter
          ]
      )

instance Core.AWSRequest CountClosedWorkflowExecutions where
  type
    Rs CountClosedWorkflowExecutions =
      Types.WorkflowExecutionCount
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "SimpleWorkflowService.CountClosedWorkflowExecutions"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
