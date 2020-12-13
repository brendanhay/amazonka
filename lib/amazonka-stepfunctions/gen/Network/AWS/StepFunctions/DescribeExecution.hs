{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.DescribeExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an execution.
--
-- This API action is not supported by @EXPRESS@ state machines.
module Network.AWS.StepFunctions.DescribeExecution
  ( -- * Creating a request
    DescribeExecution (..),
    mkDescribeExecution,

    -- ** Request lenses
    deExecutionARN,

    -- * Destructuring the response
    DescribeExecutionResponse (..),
    mkDescribeExecutionResponse,

    -- ** Response lenses
    dersStopDate,
    dersInputDetails,
    dersStatus,
    dersInput,
    dersStartDate,
    dersName,
    dersStateMachineARN,
    dersOutput,
    dersExecutionARN,
    dersOutputDetails,
    dersTraceHeader,
    dersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkDescribeExecution' smart constructor.
newtype DescribeExecution = DescribeExecution'
  { -- | The Amazon Resource Name (ARN) of the execution to describe.
    executionARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExecution' with the minimum fields required to make a request.
--
-- * 'executionARN' - The Amazon Resource Name (ARN) of the execution to describe.
mkDescribeExecution ::
  -- | 'executionARN'
  Lude.Text ->
  DescribeExecution
mkDescribeExecution pExecutionARN_ =
  DescribeExecution' {executionARN = pExecutionARN_}

-- | The Amazon Resource Name (ARN) of the execution to describe.
--
-- /Note:/ Consider using 'executionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExecutionARN :: Lens.Lens' DescribeExecution Lude.Text
deExecutionARN = Lens.lens (executionARN :: DescribeExecution -> Lude.Text) (\s a -> s {executionARN = a} :: DescribeExecution)
{-# DEPRECATED deExecutionARN "Use generic-lens or generic-optics with 'executionARN' instead." #-}

instance Lude.AWSRequest DescribeExecution where
  type Rs DescribeExecution = DescribeExecutionResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeExecutionResponse'
            Lude.<$> (x Lude..?> "stopDate")
            Lude.<*> (x Lude..?> "inputDetails")
            Lude.<*> (x Lude..:> "status")
            Lude.<*> (x Lude..?> "input")
            Lude.<*> (x Lude..:> "startDate")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..:> "stateMachineArn")
            Lude.<*> (x Lude..?> "output")
            Lude.<*> (x Lude..:> "executionArn")
            Lude.<*> (x Lude..?> "outputDetails")
            Lude.<*> (x Lude..?> "traceHeader")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.DescribeExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeExecution where
  toJSON DescribeExecution' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("executionArn" Lude..= executionARN)])

instance Lude.ToPath DescribeExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeExecutionResponse' smart constructor.
data DescribeExecutionResponse = DescribeExecutionResponse'
  { -- | If the execution has already ended, the date the execution stopped.
    stopDate :: Lude.Maybe Lude.Timestamp,
    inputDetails :: Lude.Maybe CloudWatchEventsExecutionDataDetails,
    -- | The current status of the execution.
    status :: ExecutionStatus,
    -- | The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The date the execution is started.
    startDate :: Lude.Timestamp,
    -- | The name of the execution.
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
    name :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the executed stated machine.
    stateMachineARN :: Lude.Text,
    -- | The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The Amazon Resource Name (ARN) that identifies the execution.
    executionARN :: Lude.Text,
    outputDetails :: Lude.Maybe CloudWatchEventsExecutionDataDetails,
    -- | The AWS X-Ray trace header that was passed to the execution.
    traceHeader :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExecutionResponse' with the minimum fields required to make a request.
--
-- * 'stopDate' - If the execution has already ended, the date the execution stopped.
-- * 'inputDetails' -
-- * 'status' - The current status of the execution.
-- * 'input' - The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'startDate' - The date the execution is started.
-- * 'name' - The name of the execution.
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
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) of the executed stated machine.
-- * 'output' - The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'executionARN' - The Amazon Resource Name (ARN) that identifies the execution.
-- * 'outputDetails' -
-- * 'traceHeader' - The AWS X-Ray trace header that was passed to the execution.
-- * 'responseStatus' - The response status code.
mkDescribeExecutionResponse ::
  -- | 'status'
  ExecutionStatus ->
  -- | 'startDate'
  Lude.Timestamp ->
  -- | 'stateMachineARN'
  Lude.Text ->
  -- | 'executionARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeExecutionResponse
mkDescribeExecutionResponse
  pStatus_
  pStartDate_
  pStateMachineARN_
  pExecutionARN_
  pResponseStatus_ =
    DescribeExecutionResponse'
      { stopDate = Lude.Nothing,
        inputDetails = Lude.Nothing,
        status = pStatus_,
        input = Lude.Nothing,
        startDate = pStartDate_,
        name = Lude.Nothing,
        stateMachineARN = pStateMachineARN_,
        output = Lude.Nothing,
        executionARN = pExecutionARN_,
        outputDetails = Lude.Nothing,
        traceHeader = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | If the execution has already ended, the date the execution stopped.
--
-- /Note:/ Consider using 'stopDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersStopDate :: Lens.Lens' DescribeExecutionResponse (Lude.Maybe Lude.Timestamp)
dersStopDate = Lens.lens (stopDate :: DescribeExecutionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {stopDate = a} :: DescribeExecutionResponse)
{-# DEPRECATED dersStopDate "Use generic-lens or generic-optics with 'stopDate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersInputDetails :: Lens.Lens' DescribeExecutionResponse (Lude.Maybe CloudWatchEventsExecutionDataDetails)
dersInputDetails = Lens.lens (inputDetails :: DescribeExecutionResponse -> Lude.Maybe CloudWatchEventsExecutionDataDetails) (\s a -> s {inputDetails = a} :: DescribeExecutionResponse)
{-# DEPRECATED dersInputDetails "Use generic-lens or generic-optics with 'inputDetails' instead." #-}

-- | The current status of the execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersStatus :: Lens.Lens' DescribeExecutionResponse ExecutionStatus
dersStatus = Lens.lens (status :: DescribeExecutionResponse -> ExecutionStatus) (\s a -> s {status = a} :: DescribeExecutionResponse)
{-# DEPRECATED dersStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersInput :: Lens.Lens' DescribeExecutionResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
dersInput = Lens.lens (input :: DescribeExecutionResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {input = a} :: DescribeExecutionResponse)
{-# DEPRECATED dersInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The date the execution is started.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersStartDate :: Lens.Lens' DescribeExecutionResponse Lude.Timestamp
dersStartDate = Lens.lens (startDate :: DescribeExecutionResponse -> Lude.Timestamp) (\s a -> s {startDate = a} :: DescribeExecutionResponse)
{-# DEPRECATED dersStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The name of the execution.
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
dersName :: Lens.Lens' DescribeExecutionResponse (Lude.Maybe Lude.Text)
dersName = Lens.lens (name :: DescribeExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeExecutionResponse)
{-# DEPRECATED dersName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) of the executed stated machine.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersStateMachineARN :: Lens.Lens' DescribeExecutionResponse Lude.Text
dersStateMachineARN = Lens.lens (stateMachineARN :: DescribeExecutionResponse -> Lude.Text) (\s a -> s {stateMachineARN = a} :: DescribeExecutionResponse)
{-# DEPRECATED dersStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

-- | The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersOutput :: Lens.Lens' DescribeExecutionResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
dersOutput = Lens.lens (output :: DescribeExecutionResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {output = a} :: DescribeExecutionResponse)
{-# DEPRECATED dersOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the execution.
--
-- /Note:/ Consider using 'executionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersExecutionARN :: Lens.Lens' DescribeExecutionResponse Lude.Text
dersExecutionARN = Lens.lens (executionARN :: DescribeExecutionResponse -> Lude.Text) (\s a -> s {executionARN = a} :: DescribeExecutionResponse)
{-# DEPRECATED dersExecutionARN "Use generic-lens or generic-optics with 'executionARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersOutputDetails :: Lens.Lens' DescribeExecutionResponse (Lude.Maybe CloudWatchEventsExecutionDataDetails)
dersOutputDetails = Lens.lens (outputDetails :: DescribeExecutionResponse -> Lude.Maybe CloudWatchEventsExecutionDataDetails) (\s a -> s {outputDetails = a} :: DescribeExecutionResponse)
{-# DEPRECATED dersOutputDetails "Use generic-lens or generic-optics with 'outputDetails' instead." #-}

-- | The AWS X-Ray trace header that was passed to the execution.
--
-- /Note:/ Consider using 'traceHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersTraceHeader :: Lens.Lens' DescribeExecutionResponse (Lude.Maybe Lude.Text)
dersTraceHeader = Lens.lens (traceHeader :: DescribeExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {traceHeader = a} :: DescribeExecutionResponse)
{-# DEPRECATED dersTraceHeader "Use generic-lens or generic-optics with 'traceHeader' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeExecutionResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeExecutionResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
