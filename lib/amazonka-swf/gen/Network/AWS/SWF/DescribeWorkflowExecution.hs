{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DescribeWorkflowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified workflow execution including its type and some statistics.
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
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.DescribeWorkflowExecution
  ( -- * Creating a request
    DescribeWorkflowExecution (..),
    mkDescribeWorkflowExecution,

    -- ** Request lenses
    dweDomain,
    dweExecution,

    -- * Destructuring the response
    DescribeWorkflowExecutionResponse (..),
    mkDescribeWorkflowExecutionResponse,

    -- ** Response lenses
    dwersLatestActivityTaskTimestamp,
    dwersLatestExecutionContext,
    dwersResponseStatus,
    dwersExecutionInfo,
    dwersExecutionConfiguration,
    dwersOpenCounts,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkDescribeWorkflowExecution' smart constructor.
data DescribeWorkflowExecution = DescribeWorkflowExecution'
  { domain ::
      Lude.Text,
    execution :: WorkflowExecution
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkflowExecution' with the minimum fields required to make a request.
--
-- * 'domain' - The name of the domain containing the workflow execution.
-- * 'execution' - The workflow execution to describe.
mkDescribeWorkflowExecution ::
  -- | 'domain'
  Lude.Text ->
  -- | 'execution'
  WorkflowExecution ->
  DescribeWorkflowExecution
mkDescribeWorkflowExecution pDomain_ pExecution_ =
  DescribeWorkflowExecution'
    { domain = pDomain_,
      execution = pExecution_
    }

-- | The name of the domain containing the workflow execution.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dweDomain :: Lens.Lens' DescribeWorkflowExecution Lude.Text
dweDomain = Lens.lens (domain :: DescribeWorkflowExecution -> Lude.Text) (\s a -> s {domain = a} :: DescribeWorkflowExecution)
{-# DEPRECATED dweDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The workflow execution to describe.
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dweExecution :: Lens.Lens' DescribeWorkflowExecution WorkflowExecution
dweExecution = Lens.lens (execution :: DescribeWorkflowExecution -> WorkflowExecution) (\s a -> s {execution = a} :: DescribeWorkflowExecution)
{-# DEPRECATED dweExecution "Use generic-lens or generic-optics with 'execution' instead." #-}

instance Lude.AWSRequest DescribeWorkflowExecution where
  type
    Rs DescribeWorkflowExecution =
      DescribeWorkflowExecutionResponse
  request = Req.postJSON swfService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeWorkflowExecutionResponse'
            Lude.<$> (x Lude..?> "latestActivityTaskTimestamp")
            Lude.<*> (x Lude..?> "latestExecutionContext")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "executionInfo")
            Lude.<*> (x Lude..:> "executionConfiguration")
            Lude.<*> (x Lude..:> "openCounts")
      )

instance Lude.ToHeaders DescribeWorkflowExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.DescribeWorkflowExecution" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeWorkflowExecution where
  toJSON DescribeWorkflowExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("domain" Lude..= domain),
            Lude.Just ("execution" Lude..= execution)
          ]
      )

instance Lude.ToPath DescribeWorkflowExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeWorkflowExecution where
  toQuery = Lude.const Lude.mempty

-- | Contains details about a workflow execution.
--
-- /See:/ 'mkDescribeWorkflowExecutionResponse' smart constructor.
data DescribeWorkflowExecutionResponse = DescribeWorkflowExecutionResponse'
  { latestActivityTaskTimestamp ::
      Lude.Maybe
        Lude.Timestamp,
    latestExecutionContext ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int,
    executionInfo ::
      WorkflowExecutionInfo,
    executionConfiguration ::
      WorkflowExecutionConfiguration,
    openCounts ::
      WorkflowExecutionOpenCounts
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkflowExecutionResponse' with the minimum fields required to make a request.
--
-- * 'executionConfiguration' - The configuration settings for this workflow execution including timeout values, tasklist etc.
-- * 'executionInfo' - Information about the workflow execution.
-- * 'latestActivityTaskTimestamp' - The time when the last activity task was scheduled for this workflow execution. You can use this information to determine if the workflow has not made progress for an unusually long period of time and might require a corrective action.
-- * 'latestExecutionContext' - The latest executionContext provided by the decider for this workflow execution. A decider can provide an executionContext (a free-form string) when closing a decision task using 'RespondDecisionTaskCompleted' .
-- * 'openCounts' - The number of tasks for this workflow execution. This includes open and closed tasks of all types.
-- * 'responseStatus' - The response status code.
mkDescribeWorkflowExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'executionInfo'
  WorkflowExecutionInfo ->
  -- | 'executionConfiguration'
  WorkflowExecutionConfiguration ->
  -- | 'openCounts'
  WorkflowExecutionOpenCounts ->
  DescribeWorkflowExecutionResponse
mkDescribeWorkflowExecutionResponse
  pResponseStatus_
  pExecutionInfo_
  pExecutionConfiguration_
  pOpenCounts_ =
    DescribeWorkflowExecutionResponse'
      { latestActivityTaskTimestamp =
          Lude.Nothing,
        latestExecutionContext = Lude.Nothing,
        responseStatus = pResponseStatus_,
        executionInfo = pExecutionInfo_,
        executionConfiguration = pExecutionConfiguration_,
        openCounts = pOpenCounts_
      }

-- | The time when the last activity task was scheduled for this workflow execution. You can use this information to determine if the workflow has not made progress for an unusually long period of time and might require a corrective action.
--
-- /Note:/ Consider using 'latestActivityTaskTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwersLatestActivityTaskTimestamp :: Lens.Lens' DescribeWorkflowExecutionResponse (Lude.Maybe Lude.Timestamp)
dwersLatestActivityTaskTimestamp = Lens.lens (latestActivityTaskTimestamp :: DescribeWorkflowExecutionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestActivityTaskTimestamp = a} :: DescribeWorkflowExecutionResponse)
{-# DEPRECATED dwersLatestActivityTaskTimestamp "Use generic-lens or generic-optics with 'latestActivityTaskTimestamp' instead." #-}

-- | The latest executionContext provided by the decider for this workflow execution. A decider can provide an executionContext (a free-form string) when closing a decision task using 'RespondDecisionTaskCompleted' .
--
-- /Note:/ Consider using 'latestExecutionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwersLatestExecutionContext :: Lens.Lens' DescribeWorkflowExecutionResponse (Lude.Maybe Lude.Text)
dwersLatestExecutionContext = Lens.lens (latestExecutionContext :: DescribeWorkflowExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestExecutionContext = a} :: DescribeWorkflowExecutionResponse)
{-# DEPRECATED dwersLatestExecutionContext "Use generic-lens or generic-optics with 'latestExecutionContext' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwersResponseStatus :: Lens.Lens' DescribeWorkflowExecutionResponse Lude.Int
dwersResponseStatus = Lens.lens (responseStatus :: DescribeWorkflowExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeWorkflowExecutionResponse)
{-# DEPRECATED dwersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Information about the workflow execution.
--
-- /Note:/ Consider using 'executionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwersExecutionInfo :: Lens.Lens' DescribeWorkflowExecutionResponse WorkflowExecutionInfo
dwersExecutionInfo = Lens.lens (executionInfo :: DescribeWorkflowExecutionResponse -> WorkflowExecutionInfo) (\s a -> s {executionInfo = a} :: DescribeWorkflowExecutionResponse)
{-# DEPRECATED dwersExecutionInfo "Use generic-lens or generic-optics with 'executionInfo' instead." #-}

-- | The configuration settings for this workflow execution including timeout values, tasklist etc.
--
-- /Note:/ Consider using 'executionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwersExecutionConfiguration :: Lens.Lens' DescribeWorkflowExecutionResponse WorkflowExecutionConfiguration
dwersExecutionConfiguration = Lens.lens (executionConfiguration :: DescribeWorkflowExecutionResponse -> WorkflowExecutionConfiguration) (\s a -> s {executionConfiguration = a} :: DescribeWorkflowExecutionResponse)
{-# DEPRECATED dwersExecutionConfiguration "Use generic-lens or generic-optics with 'executionConfiguration' instead." #-}

-- | The number of tasks for this workflow execution. This includes open and closed tasks of all types.
--
-- /Note:/ Consider using 'openCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwersOpenCounts :: Lens.Lens' DescribeWorkflowExecutionResponse WorkflowExecutionOpenCounts
dwersOpenCounts = Lens.lens (openCounts :: DescribeWorkflowExecutionResponse -> WorkflowExecutionOpenCounts) (\s a -> s {openCounts = a} :: DescribeWorkflowExecutionResponse)
{-# DEPRECATED dwersOpenCounts "Use generic-lens or generic-optics with 'openCounts' instead." #-}
