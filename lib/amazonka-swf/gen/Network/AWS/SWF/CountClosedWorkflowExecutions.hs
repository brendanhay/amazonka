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
    ccweExecutionFilter,
    ccweCloseStatusFilter,
    ccweTypeFilter,
    ccweDomain,
    ccweCloseTimeFilter,
    ccweTagFilter,
    ccweStartTimeFilter,

    -- * Destructuring the response
    WorkflowExecutionCount (..),
    mkWorkflowExecutionCount,

    -- ** Response lenses
    wecTruncated,
    wecCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkCountClosedWorkflowExecutions' smart constructor.
data CountClosedWorkflowExecutions = CountClosedWorkflowExecutions'
  { -- | If specified, only workflow executions matching the @WorkflowId@ in the filter are counted.
    executionFilter :: Lude.Maybe WorkflowExecutionFilter,
    -- | If specified, only workflow executions that match this close status are counted. This filter has an affect only if @executionStatus@ is specified as @CLOSED@ .
    closeStatusFilter :: Lude.Maybe CloseStatusFilter,
    -- | If specified, indicates the type of the workflow executions to be counted.
    typeFilter :: Lude.Maybe WorkflowTypeFilter,
    -- | The name of the domain containing the workflow executions to count.
    domain :: Lude.Text,
    -- | If specified, only workflow executions that meet the close time criteria of the filter are counted.
    closeTimeFilter :: Lude.Maybe ExecutionTimeFilter,
    -- | If specified, only executions that have a tag that matches the filter are counted.
    tagFilter :: Lude.Maybe TagFilter,
    -- | If specified, only workflow executions that meet the start time criteria of the filter are counted.
    startTimeFilter :: Lude.Maybe ExecutionTimeFilter
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CountClosedWorkflowExecutions' with the minimum fields required to make a request.
--
-- * 'executionFilter' - If specified, only workflow executions matching the @WorkflowId@ in the filter are counted.
-- * 'closeStatusFilter' - If specified, only workflow executions that match this close status are counted. This filter has an affect only if @executionStatus@ is specified as @CLOSED@ .
-- * 'typeFilter' - If specified, indicates the type of the workflow executions to be counted.
-- * 'domain' - The name of the domain containing the workflow executions to count.
-- * 'closeTimeFilter' - If specified, only workflow executions that meet the close time criteria of the filter are counted.
-- * 'tagFilter' - If specified, only executions that have a tag that matches the filter are counted.
-- * 'startTimeFilter' - If specified, only workflow executions that meet the start time criteria of the filter are counted.
mkCountClosedWorkflowExecutions ::
  -- | 'domain'
  Lude.Text ->
  CountClosedWorkflowExecutions
mkCountClosedWorkflowExecutions pDomain_ =
  CountClosedWorkflowExecutions'
    { executionFilter = Lude.Nothing,
      closeStatusFilter = Lude.Nothing,
      typeFilter = Lude.Nothing,
      domain = pDomain_,
      closeTimeFilter = Lude.Nothing,
      tagFilter = Lude.Nothing,
      startTimeFilter = Lude.Nothing
    }

-- | If specified, only workflow executions matching the @WorkflowId@ in the filter are counted.
--
-- /Note:/ Consider using 'executionFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweExecutionFilter :: Lens.Lens' CountClosedWorkflowExecutions (Lude.Maybe WorkflowExecutionFilter)
ccweExecutionFilter = Lens.lens (executionFilter :: CountClosedWorkflowExecutions -> Lude.Maybe WorkflowExecutionFilter) (\s a -> s {executionFilter = a} :: CountClosedWorkflowExecutions)
{-# DEPRECATED ccweExecutionFilter "Use generic-lens or generic-optics with 'executionFilter' instead." #-}

-- | If specified, only workflow executions that match this close status are counted. This filter has an affect only if @executionStatus@ is specified as @CLOSED@ .
--
-- /Note:/ Consider using 'closeStatusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweCloseStatusFilter :: Lens.Lens' CountClosedWorkflowExecutions (Lude.Maybe CloseStatusFilter)
ccweCloseStatusFilter = Lens.lens (closeStatusFilter :: CountClosedWorkflowExecutions -> Lude.Maybe CloseStatusFilter) (\s a -> s {closeStatusFilter = a} :: CountClosedWorkflowExecutions)
{-# DEPRECATED ccweCloseStatusFilter "Use generic-lens or generic-optics with 'closeStatusFilter' instead." #-}

-- | If specified, indicates the type of the workflow executions to be counted.
--
-- /Note:/ Consider using 'typeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweTypeFilter :: Lens.Lens' CountClosedWorkflowExecutions (Lude.Maybe WorkflowTypeFilter)
ccweTypeFilter = Lens.lens (typeFilter :: CountClosedWorkflowExecutions -> Lude.Maybe WorkflowTypeFilter) (\s a -> s {typeFilter = a} :: CountClosedWorkflowExecutions)
{-# DEPRECATED ccweTypeFilter "Use generic-lens or generic-optics with 'typeFilter' instead." #-}

-- | The name of the domain containing the workflow executions to count.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweDomain :: Lens.Lens' CountClosedWorkflowExecutions Lude.Text
ccweDomain = Lens.lens (domain :: CountClosedWorkflowExecutions -> Lude.Text) (\s a -> s {domain = a} :: CountClosedWorkflowExecutions)
{-# DEPRECATED ccweDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | If specified, only workflow executions that meet the close time criteria of the filter are counted.
--
-- /Note:/ Consider using 'closeTimeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweCloseTimeFilter :: Lens.Lens' CountClosedWorkflowExecutions (Lude.Maybe ExecutionTimeFilter)
ccweCloseTimeFilter = Lens.lens (closeTimeFilter :: CountClosedWorkflowExecutions -> Lude.Maybe ExecutionTimeFilter) (\s a -> s {closeTimeFilter = a} :: CountClosedWorkflowExecutions)
{-# DEPRECATED ccweCloseTimeFilter "Use generic-lens or generic-optics with 'closeTimeFilter' instead." #-}

-- | If specified, only executions that have a tag that matches the filter are counted.
--
-- /Note:/ Consider using 'tagFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweTagFilter :: Lens.Lens' CountClosedWorkflowExecutions (Lude.Maybe TagFilter)
ccweTagFilter = Lens.lens (tagFilter :: CountClosedWorkflowExecutions -> Lude.Maybe TagFilter) (\s a -> s {tagFilter = a} :: CountClosedWorkflowExecutions)
{-# DEPRECATED ccweTagFilter "Use generic-lens or generic-optics with 'tagFilter' instead." #-}

-- | If specified, only workflow executions that meet the start time criteria of the filter are counted.
--
-- /Note:/ Consider using 'startTimeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccweStartTimeFilter :: Lens.Lens' CountClosedWorkflowExecutions (Lude.Maybe ExecutionTimeFilter)
ccweStartTimeFilter = Lens.lens (startTimeFilter :: CountClosedWorkflowExecutions -> Lude.Maybe ExecutionTimeFilter) (\s a -> s {startTimeFilter = a} :: CountClosedWorkflowExecutions)
{-# DEPRECATED ccweStartTimeFilter "Use generic-lens or generic-optics with 'startTimeFilter' instead." #-}

instance Lude.AWSRequest CountClosedWorkflowExecutions where
  type Rs CountClosedWorkflowExecutions = WorkflowExecutionCount
  request = Req.postJSON swfService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CountClosedWorkflowExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.CountClosedWorkflowExecutions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CountClosedWorkflowExecutions where
  toJSON CountClosedWorkflowExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("executionFilter" Lude..=) Lude.<$> executionFilter,
            ("closeStatusFilter" Lude..=) Lude.<$> closeStatusFilter,
            ("typeFilter" Lude..=) Lude.<$> typeFilter,
            Lude.Just ("domain" Lude..= domain),
            ("closeTimeFilter" Lude..=) Lude.<$> closeTimeFilter,
            ("tagFilter" Lude..=) Lude.<$> tagFilter,
            ("startTimeFilter" Lude..=) Lude.<$> startTimeFilter
          ]
      )

instance Lude.ToPath CountClosedWorkflowExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery CountClosedWorkflowExecutions where
  toQuery = Lude.const Lude.mempty
