{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.StartSyncExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a Synchronous Express state machine execution.
module Network.AWS.StepFunctions.StartSyncExecution
  ( -- * Creating a request
    StartSyncExecution (..),
    mkStartSyncExecution,

    -- ** Request lenses
    sseInput,
    sseName,
    sseTraceHeader,
    sseStateMachineARN,

    -- * Destructuring the response
    StartSyncExecutionResponse (..),
    mkStartSyncExecutionResponse,

    -- ** Response lenses
    ssersInputDetails,
    ssersError,
    ssersInput,
    ssersCause,
    ssersName,
    ssersStateMachineARN,
    ssersOutput,
    ssersOutputDetails,
    ssersTraceHeader,
    ssersBillingDetails,
    ssersResponseStatus,
    ssersExecutionARN,
    ssersStartDate,
    ssersStopDate,
    ssersStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkStartSyncExecution' smart constructor.
data StartSyncExecution = StartSyncExecution'
  { input ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    name :: Lude.Maybe Lude.Text,
    traceHeader :: Lude.Maybe Lude.Text,
    stateMachineARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSyncExecution' with the minimum fields required to make a request.
--
-- * 'input' - The string that contains the JSON input data for the execution, for example:
--
-- @"input": "{\"first_name\" : \"test\"}"@
-- Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'name' - The name of the execution.
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) of the state machine to execute.
-- * 'traceHeader' - Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
mkStartSyncExecution ::
  -- | 'stateMachineARN'
  Lude.Text ->
  StartSyncExecution
mkStartSyncExecution pStateMachineARN_ =
  StartSyncExecution'
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
sseInput :: Lens.Lens' StartSyncExecution (Lude.Maybe (Lude.Sensitive Lude.Text))
sseInput = Lens.lens (input :: StartSyncExecution -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {input = a} :: StartSyncExecution)
{-# DEPRECATED sseInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The name of the execution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseName :: Lens.Lens' StartSyncExecution (Lude.Maybe Lude.Text)
sseName = Lens.lens (name :: StartSyncExecution -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StartSyncExecution)
{-# DEPRECATED sseName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
--
-- /Note:/ Consider using 'traceHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseTraceHeader :: Lens.Lens' StartSyncExecution (Lude.Maybe Lude.Text)
sseTraceHeader = Lens.lens (traceHeader :: StartSyncExecution -> Lude.Maybe Lude.Text) (\s a -> s {traceHeader = a} :: StartSyncExecution)
{-# DEPRECATED sseTraceHeader "Use generic-lens or generic-optics with 'traceHeader' instead." #-}

-- | The Amazon Resource Name (ARN) of the state machine to execute.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseStateMachineARN :: Lens.Lens' StartSyncExecution Lude.Text
sseStateMachineARN = Lens.lens (stateMachineARN :: StartSyncExecution -> Lude.Text) (\s a -> s {stateMachineARN = a} :: StartSyncExecution)
{-# DEPRECATED sseStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

instance Lude.AWSRequest StartSyncExecution where
  type Rs StartSyncExecution = StartSyncExecutionResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartSyncExecutionResponse'
            Lude.<$> (x Lude..?> "inputDetails")
            Lude.<*> (x Lude..?> "error")
            Lude.<*> (x Lude..?> "input")
            Lude.<*> (x Lude..?> "cause")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "stateMachineArn")
            Lude.<*> (x Lude..?> "output")
            Lude.<*> (x Lude..?> "outputDetails")
            Lude.<*> (x Lude..?> "traceHeader")
            Lude.<*> (x Lude..?> "billingDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "executionArn")
            Lude.<*> (x Lude..:> "startDate")
            Lude.<*> (x Lude..:> "stopDate")
            Lude.<*> (x Lude..:> "status")
      )

instance Lude.ToHeaders StartSyncExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.StartSyncExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartSyncExecution where
  toJSON StartSyncExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("input" Lude..=) Lude.<$> input,
            ("name" Lude..=) Lude.<$> name,
            ("traceHeader" Lude..=) Lude.<$> traceHeader,
            Lude.Just ("stateMachineArn" Lude..= stateMachineARN)
          ]
      )

instance Lude.ToPath StartSyncExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StartSyncExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartSyncExecutionResponse' smart constructor.
data StartSyncExecutionResponse = StartSyncExecutionResponse'
  { inputDetails ::
      Lude.Maybe
        CloudWatchEventsExecutionDataDetails,
    error ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    input ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    cause ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    name :: Lude.Maybe Lude.Text,
    stateMachineARN ::
      Lude.Maybe Lude.Text,
    output ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    outputDetails ::
      Lude.Maybe
        CloudWatchEventsExecutionDataDetails,
    traceHeader :: Lude.Maybe Lude.Text,
    billingDetails ::
      Lude.Maybe BillingDetails,
    responseStatus :: Lude.Int,
    executionARN :: Lude.Text,
    startDate :: Lude.Timestamp,
    stopDate :: Lude.Timestamp,
    status :: SyncExecutionStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSyncExecutionResponse' with the minimum fields required to make a request.
--
-- * 'billingDetails' - An object that describes workflow billing details, including billed duration and memory use.
-- * 'cause' - A more detailed explanation of the cause of the failure.
-- * 'error' - The error code of the failure.
-- * 'executionARN' - The Amazon Resource Name (ARN) that identifies the execution.
-- * 'input' - The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'inputDetails' - Undocumented field.
-- * 'name' - The name of the execution.
-- * 'output' - The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'outputDetails' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'startDate' - The date the execution is started.
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) that identifies the state machine.
-- * 'status' - The current status of the execution.
-- * 'stopDate' - If the execution has already ended, the date the execution stopped.
-- * 'traceHeader' - The AWS X-Ray trace header that was passed to the execution.
mkStartSyncExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'executionARN'
  Lude.Text ->
  -- | 'startDate'
  Lude.Timestamp ->
  -- | 'stopDate'
  Lude.Timestamp ->
  -- | 'status'
  SyncExecutionStatus ->
  StartSyncExecutionResponse
mkStartSyncExecutionResponse
  pResponseStatus_
  pExecutionARN_
  pStartDate_
  pStopDate_
  pStatus_ =
    StartSyncExecutionResponse'
      { inputDetails = Lude.Nothing,
        error = Lude.Nothing,
        input = Lude.Nothing,
        cause = Lude.Nothing,
        name = Lude.Nothing,
        stateMachineARN = Lude.Nothing,
        output = Lude.Nothing,
        outputDetails = Lude.Nothing,
        traceHeader = Lude.Nothing,
        billingDetails = Lude.Nothing,
        responseStatus = pResponseStatus_,
        executionARN = pExecutionARN_,
        startDate = pStartDate_,
        stopDate = pStopDate_,
        status = pStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersInputDetails :: Lens.Lens' StartSyncExecutionResponse (Lude.Maybe CloudWatchEventsExecutionDataDetails)
ssersInputDetails = Lens.lens (inputDetails :: StartSyncExecutionResponse -> Lude.Maybe CloudWatchEventsExecutionDataDetails) (\s a -> s {inputDetails = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersInputDetails "Use generic-lens or generic-optics with 'inputDetails' instead." #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersError :: Lens.Lens' StartSyncExecutionResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
ssersError = Lens.lens (error :: StartSyncExecutionResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersInput :: Lens.Lens' StartSyncExecutionResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
ssersInput = Lens.lens (input :: StartSyncExecutionResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {input = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersCause :: Lens.Lens' StartSyncExecutionResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
ssersCause = Lens.lens (cause :: StartSyncExecutionResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The name of the execution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersName :: Lens.Lens' StartSyncExecutionResponse (Lude.Maybe Lude.Text)
ssersName = Lens.lens (name :: StartSyncExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the state machine.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersStateMachineARN :: Lens.Lens' StartSyncExecutionResponse (Lude.Maybe Lude.Text)
ssersStateMachineARN = Lens.lens (stateMachineARN :: StartSyncExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {stateMachineARN = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

-- | The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersOutput :: Lens.Lens' StartSyncExecutionResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
ssersOutput = Lens.lens (output :: StartSyncExecutionResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {output = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersOutputDetails :: Lens.Lens' StartSyncExecutionResponse (Lude.Maybe CloudWatchEventsExecutionDataDetails)
ssersOutputDetails = Lens.lens (outputDetails :: StartSyncExecutionResponse -> Lude.Maybe CloudWatchEventsExecutionDataDetails) (\s a -> s {outputDetails = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersOutputDetails "Use generic-lens or generic-optics with 'outputDetails' instead." #-}

-- | The AWS X-Ray trace header that was passed to the execution.
--
-- /Note:/ Consider using 'traceHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersTraceHeader :: Lens.Lens' StartSyncExecutionResponse (Lude.Maybe Lude.Text)
ssersTraceHeader = Lens.lens (traceHeader :: StartSyncExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {traceHeader = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersTraceHeader "Use generic-lens or generic-optics with 'traceHeader' instead." #-}

-- | An object that describes workflow billing details, including billed duration and memory use.
--
-- /Note:/ Consider using 'billingDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersBillingDetails :: Lens.Lens' StartSyncExecutionResponse (Lude.Maybe BillingDetails)
ssersBillingDetails = Lens.lens (billingDetails :: StartSyncExecutionResponse -> Lude.Maybe BillingDetails) (\s a -> s {billingDetails = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersBillingDetails "Use generic-lens or generic-optics with 'billingDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersResponseStatus :: Lens.Lens' StartSyncExecutionResponse Lude.Int
ssersResponseStatus = Lens.lens (responseStatus :: StartSyncExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the execution.
--
-- /Note:/ Consider using 'executionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersExecutionARN :: Lens.Lens' StartSyncExecutionResponse Lude.Text
ssersExecutionARN = Lens.lens (executionARN :: StartSyncExecutionResponse -> Lude.Text) (\s a -> s {executionARN = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersExecutionARN "Use generic-lens or generic-optics with 'executionARN' instead." #-}

-- | The date the execution is started.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersStartDate :: Lens.Lens' StartSyncExecutionResponse Lude.Timestamp
ssersStartDate = Lens.lens (startDate :: StartSyncExecutionResponse -> Lude.Timestamp) (\s a -> s {startDate = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | If the execution has already ended, the date the execution stopped.
--
-- /Note:/ Consider using 'stopDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersStopDate :: Lens.Lens' StartSyncExecutionResponse Lude.Timestamp
ssersStopDate = Lens.lens (stopDate :: StartSyncExecutionResponse -> Lude.Timestamp) (\s a -> s {stopDate = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersStopDate "Use generic-lens or generic-optics with 'stopDate' instead." #-}

-- | The current status of the execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersStatus :: Lens.Lens' StartSyncExecutionResponse SyncExecutionStatus
ssersStatus = Lens.lens (status :: StartSyncExecutionResponse -> SyncExecutionStatus) (\s a -> s {status = a} :: StartSyncExecutionResponse)
{-# DEPRECATED ssersStatus "Use generic-lens or generic-optics with 'status' instead." #-}
