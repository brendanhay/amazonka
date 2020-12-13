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
    coweExecutionFilter,
    coweTypeFilter,
    coweDomain,
    coweTagFilter,
    coweStartTimeFilter,

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

-- | /See:/ 'mkCountOpenWorkflowExecutions' smart constructor.
data CountOpenWorkflowExecutions = CountOpenWorkflowExecutions'
  { -- | If specified, only workflow executions matching the @WorkflowId@ in the filter are counted.
    executionFilter :: Lude.Maybe WorkflowExecutionFilter,
    -- | Specifies the type of the workflow executions to be counted.
    typeFilter :: Lude.Maybe WorkflowTypeFilter,
    -- | The name of the domain containing the workflow executions to count.
    domain :: Lude.Text,
    -- | If specified, only executions that have a tag that matches the filter are counted.
    tagFilter :: Lude.Maybe TagFilter,
    -- | Specifies the start time criteria that workflow executions must meet in order to be counted.
    startTimeFilter :: ExecutionTimeFilter
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CountOpenWorkflowExecutions' with the minimum fields required to make a request.
--
-- * 'executionFilter' - If specified, only workflow executions matching the @WorkflowId@ in the filter are counted.
-- * 'typeFilter' - Specifies the type of the workflow executions to be counted.
-- * 'domain' - The name of the domain containing the workflow executions to count.
-- * 'tagFilter' - If specified, only executions that have a tag that matches the filter are counted.
-- * 'startTimeFilter' - Specifies the start time criteria that workflow executions must meet in order to be counted.
mkCountOpenWorkflowExecutions ::
  -- | 'domain'
  Lude.Text ->
  -- | 'startTimeFilter'
  ExecutionTimeFilter ->
  CountOpenWorkflowExecutions
mkCountOpenWorkflowExecutions pDomain_ pStartTimeFilter_ =
  CountOpenWorkflowExecutions'
    { executionFilter = Lude.Nothing,
      typeFilter = Lude.Nothing,
      domain = pDomain_,
      tagFilter = Lude.Nothing,
      startTimeFilter = pStartTimeFilter_
    }

-- | If specified, only workflow executions matching the @WorkflowId@ in the filter are counted.
--
-- /Note:/ Consider using 'executionFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coweExecutionFilter :: Lens.Lens' CountOpenWorkflowExecutions (Lude.Maybe WorkflowExecutionFilter)
coweExecutionFilter = Lens.lens (executionFilter :: CountOpenWorkflowExecutions -> Lude.Maybe WorkflowExecutionFilter) (\s a -> s {executionFilter = a} :: CountOpenWorkflowExecutions)
{-# DEPRECATED coweExecutionFilter "Use generic-lens or generic-optics with 'executionFilter' instead." #-}

-- | Specifies the type of the workflow executions to be counted.
--
-- /Note:/ Consider using 'typeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coweTypeFilter :: Lens.Lens' CountOpenWorkflowExecutions (Lude.Maybe WorkflowTypeFilter)
coweTypeFilter = Lens.lens (typeFilter :: CountOpenWorkflowExecutions -> Lude.Maybe WorkflowTypeFilter) (\s a -> s {typeFilter = a} :: CountOpenWorkflowExecutions)
{-# DEPRECATED coweTypeFilter "Use generic-lens or generic-optics with 'typeFilter' instead." #-}

-- | The name of the domain containing the workflow executions to count.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coweDomain :: Lens.Lens' CountOpenWorkflowExecutions Lude.Text
coweDomain = Lens.lens (domain :: CountOpenWorkflowExecutions -> Lude.Text) (\s a -> s {domain = a} :: CountOpenWorkflowExecutions)
{-# DEPRECATED coweDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | If specified, only executions that have a tag that matches the filter are counted.
--
-- /Note:/ Consider using 'tagFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coweTagFilter :: Lens.Lens' CountOpenWorkflowExecutions (Lude.Maybe TagFilter)
coweTagFilter = Lens.lens (tagFilter :: CountOpenWorkflowExecutions -> Lude.Maybe TagFilter) (\s a -> s {tagFilter = a} :: CountOpenWorkflowExecutions)
{-# DEPRECATED coweTagFilter "Use generic-lens or generic-optics with 'tagFilter' instead." #-}

-- | Specifies the start time criteria that workflow executions must meet in order to be counted.
--
-- /Note:/ Consider using 'startTimeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coweStartTimeFilter :: Lens.Lens' CountOpenWorkflowExecutions ExecutionTimeFilter
coweStartTimeFilter = Lens.lens (startTimeFilter :: CountOpenWorkflowExecutions -> ExecutionTimeFilter) (\s a -> s {startTimeFilter = a} :: CountOpenWorkflowExecutions)
{-# DEPRECATED coweStartTimeFilter "Use generic-lens or generic-optics with 'startTimeFilter' instead." #-}

instance Lude.AWSRequest CountOpenWorkflowExecutions where
  type Rs CountOpenWorkflowExecutions = WorkflowExecutionCount
  request = Req.postJSON swfService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CountOpenWorkflowExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.CountOpenWorkflowExecutions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CountOpenWorkflowExecutions where
  toJSON CountOpenWorkflowExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("executionFilter" Lude..=) Lude.<$> executionFilter,
            ("typeFilter" Lude..=) Lude.<$> typeFilter,
            Lude.Just ("domain" Lude..= domain),
            ("tagFilter" Lude..=) Lude.<$> tagFilter,
            Lude.Just ("startTimeFilter" Lude..= startTimeFilter)
          ]
      )

instance Lude.ToPath CountOpenWorkflowExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery CountOpenWorkflowExecutions where
  toQuery = Lude.const Lude.mempty
