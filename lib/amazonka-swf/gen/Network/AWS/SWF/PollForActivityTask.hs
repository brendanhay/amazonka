{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.PollForActivityTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to get an 'ActivityTask' from the specified activity @taskList@ . This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available. The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns an empty result. An empty result, in this context, means that an ActivityTask is returned, but that the value of taskToken is an empty string. If a task is returned, the worker should use its type to identify and process it correctly.
--
-- /Important:/ Workers should set their client side socket timeout to at least 70 seconds (10 seconds higher than the maximum time service may hold the poll request).
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the @taskList.name@ parameter by using a @Condition@ element with the @swf:taskList.name@ key to allow the action to access only certain task lists.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.PollForActivityTask
  ( -- * Creating a request
    PollForActivityTask (..),
    mkPollForActivityTask,

    -- ** Request lenses
    pfatDomain,
    pfatTaskList,
    pfatIdentity,

    -- * Destructuring the response
    PollForActivityTaskResponse (..),
    mkPollForActivityTaskResponse,

    -- ** Response lenses
    pfatrsActivityType,
    pfatrsActivityId,
    pfatrsInput,
    pfatrsStartedEventId,
    pfatrsTaskToken,
    pfatrsWorkflowExecution,
    pfatrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkPollForActivityTask' smart constructor.
data PollForActivityTask = PollForActivityTask'
  { -- | The name of the domain that contains the task lists being polled.
    domain :: Lude.Text,
    -- | Specifies the task list to poll for activity tasks.
    --
    -- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
    taskList :: TaskList,
    -- | Identity of the worker making the request, recorded in the @ActivityTaskStarted@ event in the workflow history. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
    identity :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PollForActivityTask' with the minimum fields required to make a request.
--
-- * 'domain' - The name of the domain that contains the task lists being polled.
-- * 'taskList' - Specifies the task list to poll for activity tasks.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
-- * 'identity' - Identity of the worker making the request, recorded in the @ActivityTaskStarted@ event in the workflow history. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
mkPollForActivityTask ::
  -- | 'domain'
  Lude.Text ->
  -- | 'taskList'
  TaskList ->
  PollForActivityTask
mkPollForActivityTask pDomain_ pTaskList_ =
  PollForActivityTask'
    { domain = pDomain_,
      taskList = pTaskList_,
      identity = Lude.Nothing
    }

-- | The name of the domain that contains the task lists being polled.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatDomain :: Lens.Lens' PollForActivityTask Lude.Text
pfatDomain = Lens.lens (domain :: PollForActivityTask -> Lude.Text) (\s a -> s {domain = a} :: PollForActivityTask)
{-# DEPRECATED pfatDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specifies the task list to poll for activity tasks.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatTaskList :: Lens.Lens' PollForActivityTask TaskList
pfatTaskList = Lens.lens (taskList :: PollForActivityTask -> TaskList) (\s a -> s {taskList = a} :: PollForActivityTask)
{-# DEPRECATED pfatTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | Identity of the worker making the request, recorded in the @ActivityTaskStarted@ event in the workflow history. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatIdentity :: Lens.Lens' PollForActivityTask (Lude.Maybe Lude.Text)
pfatIdentity = Lens.lens (identity :: PollForActivityTask -> Lude.Maybe Lude.Text) (\s a -> s {identity = a} :: PollForActivityTask)
{-# DEPRECATED pfatIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

instance Lude.AWSRequest PollForActivityTask where
  type Rs PollForActivityTask = PollForActivityTaskResponse
  request = Req.postJSON swfService
  response =
    Res.receiveJSON
      ( \s h x ->
          PollForActivityTaskResponse'
            Lude.<$> (x Lude..?> "activityType")
            Lude.<*> (x Lude..?> "activityId")
            Lude.<*> (x Lude..?> "input")
            Lude.<*> (x Lude..:> "startedEventId")
            Lude.<*> (x Lude..?> "taskToken")
            Lude.<*> (x Lude..?> "workflowExecution")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PollForActivityTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.PollForActivityTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PollForActivityTask where
  toJSON PollForActivityTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("domain" Lude..= domain),
            Lude.Just ("taskList" Lude..= taskList),
            ("identity" Lude..=) Lude.<$> identity
          ]
      )

instance Lude.ToPath PollForActivityTask where
  toPath = Lude.const "/"

instance Lude.ToQuery PollForActivityTask where
  toQuery = Lude.const Lude.mempty

-- | Unit of work sent to an activity worker.
--
-- /See:/ 'mkPollForActivityTaskResponse' smart constructor.
data PollForActivityTaskResponse = PollForActivityTaskResponse'
  { -- | The type of this activity task.
    activityType :: Lude.Maybe ActivityType,
    -- | The unique ID of the task.
    activityId :: Lude.Maybe Lude.Text,
    -- | The inputs provided when the activity task was scheduled. The form of the input is user defined and should be meaningful to the activity implementation.
    input :: Lude.Maybe Lude.Text,
    -- | The ID of the @ActivityTaskStarted@ event recorded in the history.
    startedEventId :: Lude.Integer,
    -- | The opaque string used as a handle on the task. This token is used by workers to communicate progress and response information back to the system about the task.
    taskToken :: Lude.Maybe Lude.Text,
    -- | The workflow execution that started this activity task.
    workflowExecution :: Lude.Maybe WorkflowExecution,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PollForActivityTaskResponse' with the minimum fields required to make a request.
--
-- * 'activityType' - The type of this activity task.
-- * 'activityId' - The unique ID of the task.
-- * 'input' - The inputs provided when the activity task was scheduled. The form of the input is user defined and should be meaningful to the activity implementation.
-- * 'startedEventId' - The ID of the @ActivityTaskStarted@ event recorded in the history.
-- * 'taskToken' - The opaque string used as a handle on the task. This token is used by workers to communicate progress and response information back to the system about the task.
-- * 'workflowExecution' - The workflow execution that started this activity task.
-- * 'responseStatus' - The response status code.
mkPollForActivityTaskResponse ::
  -- | 'startedEventId'
  Lude.Integer ->
  -- | 'responseStatus'
  Lude.Int ->
  PollForActivityTaskResponse
mkPollForActivityTaskResponse pStartedEventId_ pResponseStatus_ =
  PollForActivityTaskResponse'
    { activityType = Lude.Nothing,
      activityId = Lude.Nothing,
      input = Lude.Nothing,
      startedEventId = pStartedEventId_,
      taskToken = Lude.Nothing,
      workflowExecution = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The type of this activity task.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrsActivityType :: Lens.Lens' PollForActivityTaskResponse (Lude.Maybe ActivityType)
pfatrsActivityType = Lens.lens (activityType :: PollForActivityTaskResponse -> Lude.Maybe ActivityType) (\s a -> s {activityType = a} :: PollForActivityTaskResponse)
{-# DEPRECATED pfatrsActivityType "Use generic-lens or generic-optics with 'activityType' instead." #-}

-- | The unique ID of the task.
--
-- /Note:/ Consider using 'activityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrsActivityId :: Lens.Lens' PollForActivityTaskResponse (Lude.Maybe Lude.Text)
pfatrsActivityId = Lens.lens (activityId :: PollForActivityTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {activityId = a} :: PollForActivityTaskResponse)
{-# DEPRECATED pfatrsActivityId "Use generic-lens or generic-optics with 'activityId' instead." #-}

-- | The inputs provided when the activity task was scheduled. The form of the input is user defined and should be meaningful to the activity implementation.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrsInput :: Lens.Lens' PollForActivityTaskResponse (Lude.Maybe Lude.Text)
pfatrsInput = Lens.lens (input :: PollForActivityTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: PollForActivityTaskResponse)
{-# DEPRECATED pfatrsInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The ID of the @ActivityTaskStarted@ event recorded in the history.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrsStartedEventId :: Lens.Lens' PollForActivityTaskResponse Lude.Integer
pfatrsStartedEventId = Lens.lens (startedEventId :: PollForActivityTaskResponse -> Lude.Integer) (\s a -> s {startedEventId = a} :: PollForActivityTaskResponse)
{-# DEPRECATED pfatrsStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | The opaque string used as a handle on the task. This token is used by workers to communicate progress and response information back to the system about the task.
--
-- /Note:/ Consider using 'taskToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrsTaskToken :: Lens.Lens' PollForActivityTaskResponse (Lude.Maybe Lude.Text)
pfatrsTaskToken = Lens.lens (taskToken :: PollForActivityTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskToken = a} :: PollForActivityTaskResponse)
{-# DEPRECATED pfatrsTaskToken "Use generic-lens or generic-optics with 'taskToken' instead." #-}

-- | The workflow execution that started this activity task.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrsWorkflowExecution :: Lens.Lens' PollForActivityTaskResponse (Lude.Maybe WorkflowExecution)
pfatrsWorkflowExecution = Lens.lens (workflowExecution :: PollForActivityTaskResponse -> Lude.Maybe WorkflowExecution) (\s a -> s {workflowExecution = a} :: PollForActivityTaskResponse)
{-# DEPRECATED pfatrsWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfatrsResponseStatus :: Lens.Lens' PollForActivityTaskResponse Lude.Int
pfatrsResponseStatus = Lens.lens (responseStatus :: PollForActivityTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PollForActivityTaskResponse)
{-# DEPRECATED pfatrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
