{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.CountOpenWorkflowExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of open workflow executions within the given domain that meet the specified filtering criteria.
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
module Network.AWS.SWF.CountOpenWorkflowExecutions
  ( -- * Creating a request
    CountOpenWorkflowExecutions (..),
    mkCountOpenWorkflowExecutions,

    -- ** Request lenses
    coweDomain,
    coweStartTimeFilter,
    coweExecutionFilter,
    coweTagFilter,
    coweTypeFilter,

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

-- | /See:/ 'mkCountOpenWorkflowExecutions' smart constructor.
data CountOpenWorkflowExecutions = CountOpenWorkflowExecutions'
  { -- | The name of the domain containing the workflow executions to count.
    domain :: Types.Domain,
    -- | Specifies the start time criteria that workflow executions must meet in order to be counted.
    startTimeFilter :: Types.ExecutionTimeFilter,
    -- | If specified, only workflow executions matching the @WorkflowId@ in the filter are counted.
    executionFilter :: Core.Maybe Types.WorkflowExecutionFilter,
    -- | If specified, only executions that have a tag that matches the filter are counted.
    tagFilter :: Core.Maybe Types.TagFilter,
    -- | Specifies the type of the workflow executions to be counted.
    typeFilter :: Core.Maybe Types.WorkflowTypeFilter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CountOpenWorkflowExecutions' value with any optional fields omitted.
mkCountOpenWorkflowExecutions ::
  -- | 'domain'
  Types.Domain ->
  -- | 'startTimeFilter'
  Types.ExecutionTimeFilter ->
  CountOpenWorkflowExecutions
mkCountOpenWorkflowExecutions domain startTimeFilter =
  CountOpenWorkflowExecutions'
    { domain,
      startTimeFilter,
      executionFilter = Core.Nothing,
      tagFilter = Core.Nothing,
      typeFilter = Core.Nothing
    }

-- | The name of the domain containing the workflow executions to count.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coweDomain :: Lens.Lens' CountOpenWorkflowExecutions Types.Domain
coweDomain = Lens.field @"domain"
{-# DEPRECATED coweDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specifies the start time criteria that workflow executions must meet in order to be counted.
--
-- /Note:/ Consider using 'startTimeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coweStartTimeFilter :: Lens.Lens' CountOpenWorkflowExecutions Types.ExecutionTimeFilter
coweStartTimeFilter = Lens.field @"startTimeFilter"
{-# DEPRECATED coweStartTimeFilter "Use generic-lens or generic-optics with 'startTimeFilter' instead." #-}

-- | If specified, only workflow executions matching the @WorkflowId@ in the filter are counted.
--
-- /Note:/ Consider using 'executionFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coweExecutionFilter :: Lens.Lens' CountOpenWorkflowExecutions (Core.Maybe Types.WorkflowExecutionFilter)
coweExecutionFilter = Lens.field @"executionFilter"
{-# DEPRECATED coweExecutionFilter "Use generic-lens or generic-optics with 'executionFilter' instead." #-}

-- | If specified, only executions that have a tag that matches the filter are counted.
--
-- /Note:/ Consider using 'tagFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coweTagFilter :: Lens.Lens' CountOpenWorkflowExecutions (Core.Maybe Types.TagFilter)
coweTagFilter = Lens.field @"tagFilter"
{-# DEPRECATED coweTagFilter "Use generic-lens or generic-optics with 'tagFilter' instead." #-}

-- | Specifies the type of the workflow executions to be counted.
--
-- /Note:/ Consider using 'typeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coweTypeFilter :: Lens.Lens' CountOpenWorkflowExecutions (Core.Maybe Types.WorkflowTypeFilter)
coweTypeFilter = Lens.field @"typeFilter"
{-# DEPRECATED coweTypeFilter "Use generic-lens or generic-optics with 'typeFilter' instead." #-}

instance Core.FromJSON CountOpenWorkflowExecutions where
  toJSON CountOpenWorkflowExecutions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("startTimeFilter" Core..= startTimeFilter),
            ("executionFilter" Core..=) Core.<$> executionFilter,
            ("tagFilter" Core..=) Core.<$> tagFilter,
            ("typeFilter" Core..=) Core.<$> typeFilter
          ]
      )

instance Core.AWSRequest CountOpenWorkflowExecutions where
  type Rs CountOpenWorkflowExecutions = Types.WorkflowExecutionCount
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "SimpleWorkflowService.CountOpenWorkflowExecutions"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
