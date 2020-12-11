{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.StartExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a state machine execution.
module Network.AWS.StepFunctions.StartExecution
  ( -- * Creating a request
    StartExecution (..),
    mkStartExecution,

    -- ** Request lenses
    seInput,
    seName,
    seTraceHeader,
    seStateMachineARN,

    -- * Destructuring the response
    StartExecutionResponse (..),
    mkStartExecutionResponse,

    -- ** Response lenses
    srsResponseStatus,
    srsExecutionARN,
    srsStartDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkStartExecution' smart constructor.
data StartExecution = StartExecution'
  { input ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    name :: Lude.Maybe Lude.Text,
    traceHeader :: Lude.Maybe Lude.Text,
    stateMachineARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartExecution' with the minimum fields required to make a request.
--
-- * 'input' - The string that contains the JSON input data for the execution, for example:
--
-- @"input": "{\"first_name\" : \"test\"}"@
-- Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'name' - The name of the execution. This name must be unique for your AWS account, region, and state machine for 90 days. For more information, see <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions> in the /AWS Step Functions Developer Guide/ .
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) of the state machine to execute.
-- * 'traceHeader' - Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
mkStartExecution ::
  -- | 'stateMachineARN'
  Lude.Text ->
  StartExecution
mkStartExecution pStateMachineARN_ =
  StartExecution'
    { input = Lude.Nothing,
      name = Lude.Nothing,
      traceHeader = Lude.Nothing,
      stateMachineARN = pStateMachineARN_
    }

-- | The string that contains the JSON input data for the execution, for example:
--
-- @"input": "{\"first_name\" : \"test\"}"@
-- Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seInput :: Lens.Lens' StartExecution (Lude.Maybe (Lude.Sensitive Lude.Text))
seInput = Lens.lens (input :: StartExecution -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {input = a} :: StartExecution)
{-# DEPRECATED seInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The name of the execution. This name must be unique for your AWS account, region, and state machine for 90 days. For more information, see <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions> in the /AWS Step Functions Developer Guide/ .
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seName :: Lens.Lens' StartExecution (Lude.Maybe Lude.Text)
seName = Lens.lens (name :: StartExecution -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StartExecution)
{-# DEPRECATED seName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
--
-- /Note:/ Consider using 'traceHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTraceHeader :: Lens.Lens' StartExecution (Lude.Maybe Lude.Text)
seTraceHeader = Lens.lens (traceHeader :: StartExecution -> Lude.Maybe Lude.Text) (\s a -> s {traceHeader = a} :: StartExecution)
{-# DEPRECATED seTraceHeader "Use generic-lens or generic-optics with 'traceHeader' instead." #-}

-- | The Amazon Resource Name (ARN) of the state machine to execute.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStateMachineARN :: Lens.Lens' StartExecution Lude.Text
seStateMachineARN = Lens.lens (stateMachineARN :: StartExecution -> Lude.Text) (\s a -> s {stateMachineARN = a} :: StartExecution)
{-# DEPRECATED seStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

instance Lude.AWSRequest StartExecution where
  type Rs StartExecution = StartExecutionResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartExecutionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "executionArn")
            Lude.<*> (x Lude..:> "startDate")
      )

instance Lude.ToHeaders StartExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.StartExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartExecution where
  toJSON StartExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("input" Lude..=) Lude.<$> input,
            ("name" Lude..=) Lude.<$> name,
            ("traceHeader" Lude..=) Lude.<$> traceHeader,
            Lude.Just ("stateMachineArn" Lude..= stateMachineARN)
          ]
      )

instance Lude.ToPath StartExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StartExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartExecutionResponse' smart constructor.
data StartExecutionResponse = StartExecutionResponse'
  { responseStatus ::
      Lude.Int,
    executionARN :: Lude.Text,
    startDate :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartExecutionResponse' with the minimum fields required to make a request.
--
-- * 'executionARN' - The Amazon Resource Name (ARN) that identifies the execution.
-- * 'responseStatus' - The response status code.
-- * 'startDate' - The date the execution is started.
mkStartExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'executionARN'
  Lude.Text ->
  -- | 'startDate'
  Lude.Timestamp ->
  StartExecutionResponse
mkStartExecutionResponse
  pResponseStatus_
  pExecutionARN_
  pStartDate_ =
    StartExecutionResponse'
      { responseStatus = pResponseStatus_,
        executionARN = pExecutionARN_,
        startDate = pStartDate_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartExecutionResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartExecutionResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the execution.
--
-- /Note:/ Consider using 'executionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsExecutionARN :: Lens.Lens' StartExecutionResponse Lude.Text
srsExecutionARN = Lens.lens (executionARN :: StartExecutionResponse -> Lude.Text) (\s a -> s {executionARN = a} :: StartExecutionResponse)
{-# DEPRECATED srsExecutionARN "Use generic-lens or generic-optics with 'executionARN' instead." #-}

-- | The date the execution is started.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsStartDate :: Lens.Lens' StartExecutionResponse Lude.Timestamp
srsStartDate = Lens.lens (startDate :: StartExecutionResponse -> Lude.Timestamp) (\s a -> s {startDate = a} :: StartExecutionResponse)
{-# DEPRECATED srsStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}
