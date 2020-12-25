{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    sseStateMachineArn,
    sseInput,
    sseName,
    sseTraceHeader,

    -- * Destructuring the response
    StartSyncExecutionResponse (..),
    mkStartSyncExecutionResponse,

    -- ** Response lenses
    sserrsExecutionArn,
    sserrsStartDate,
    sserrsStopDate,
    sserrsStatus,
    sserrsBillingDetails,
    sserrsCause,
    sserrsError,
    sserrsInput,
    sserrsInputDetails,
    sserrsName,
    sserrsOutput,
    sserrsOutputDetails,
    sserrsStateMachineArn,
    sserrsTraceHeader,
    sserrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkStartSyncExecution' smart constructor.
data StartSyncExecution = StartSyncExecution'
  { -- | The Amazon Resource Name (ARN) of the state machine to execute.
    stateMachineArn :: Types.Arn,
    -- | The string that contains the JSON input data for the execution, for example:
    --
    -- @"input": "{\"first_name\" : \"test\"}"@
    -- Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Core.Maybe Types.SensitiveData,
    -- | The name of the execution.
    name :: Core.Maybe Types.Name,
    -- | Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
    traceHeader :: Core.Maybe Types.TraceHeader
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSyncExecution' value with any optional fields omitted.
mkStartSyncExecution ::
  -- | 'stateMachineArn'
  Types.Arn ->
  StartSyncExecution
mkStartSyncExecution stateMachineArn =
  StartSyncExecution'
    { stateMachineArn,
      input = Core.Nothing,
      name = Core.Nothing,
      traceHeader = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the state machine to execute.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseStateMachineArn :: Lens.Lens' StartSyncExecution Types.Arn
sseStateMachineArn = Lens.field @"stateMachineArn"
{-# DEPRECATED sseStateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead." #-}

-- | The string that contains the JSON input data for the execution, for example:
--
-- @"input": "{\"first_name\" : \"test\"}"@
-- Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseInput :: Lens.Lens' StartSyncExecution (Core.Maybe Types.SensitiveData)
sseInput = Lens.field @"input"
{-# DEPRECATED sseInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The name of the execution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseName :: Lens.Lens' StartSyncExecution (Core.Maybe Types.Name)
sseName = Lens.field @"name"
{-# DEPRECATED sseName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
--
-- /Note:/ Consider using 'traceHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseTraceHeader :: Lens.Lens' StartSyncExecution (Core.Maybe Types.TraceHeader)
sseTraceHeader = Lens.field @"traceHeader"
{-# DEPRECATED sseTraceHeader "Use generic-lens or generic-optics with 'traceHeader' instead." #-}

instance Core.FromJSON StartSyncExecution where
  toJSON StartSyncExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("stateMachineArn" Core..= stateMachineArn),
            ("input" Core..=) Core.<$> input,
            ("name" Core..=) Core.<$> name,
            ("traceHeader" Core..=) Core.<$> traceHeader
          ]
      )

instance Core.AWSRequest StartSyncExecution where
  type Rs StartSyncExecution = StartSyncExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSStepFunctions.StartSyncExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSyncExecutionResponse'
            Core.<$> (x Core..: "executionArn")
            Core.<*> (x Core..: "startDate")
            Core.<*> (x Core..: "stopDate")
            Core.<*> (x Core..: "status")
            Core.<*> (x Core..:? "billingDetails")
            Core.<*> (x Core..:? "cause")
            Core.<*> (x Core..:? "error")
            Core.<*> (x Core..:? "input")
            Core.<*> (x Core..:? "inputDetails")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "output")
            Core.<*> (x Core..:? "outputDetails")
            Core.<*> (x Core..:? "stateMachineArn")
            Core.<*> (x Core..:? "traceHeader")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartSyncExecutionResponse' smart constructor.
data StartSyncExecutionResponse = StartSyncExecutionResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the execution.
    executionArn :: Types.ExecutionArn,
    -- | The date the execution is started.
    startDate :: Core.NominalDiffTime,
    -- | If the execution has already ended, the date the execution stopped.
    stopDate :: Core.NominalDiffTime,
    -- | The current status of the execution.
    status :: Types.SyncExecutionStatus,
    -- | An object that describes workflow billing details, including billed duration and memory use.
    billingDetails :: Core.Maybe Types.BillingDetails,
    -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe Types.Cause,
    -- | The error code of the failure.
    error :: Core.Maybe Types.SensitiveError,
    -- | The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Core.Maybe Types.SensitiveData,
    inputDetails :: Core.Maybe Types.CloudWatchEventsExecutionDataDetails,
    -- | The name of the execution.
    name :: Core.Maybe Types.Name,
    -- | The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Core.Maybe Types.SensitiveData,
    outputDetails :: Core.Maybe Types.CloudWatchEventsExecutionDataDetails,
    -- | The Amazon Resource Name (ARN) that identifies the state machine.
    stateMachineArn :: Core.Maybe Types.StateMachineArn,
    -- | The AWS X-Ray trace header that was passed to the execution.
    traceHeader :: Core.Maybe Types.TraceHeader,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartSyncExecutionResponse' value with any optional fields omitted.
mkStartSyncExecutionResponse ::
  -- | 'executionArn'
  Types.ExecutionArn ->
  -- | 'startDate'
  Core.NominalDiffTime ->
  -- | 'stopDate'
  Core.NominalDiffTime ->
  -- | 'status'
  Types.SyncExecutionStatus ->
  -- | 'responseStatus'
  Core.Int ->
  StartSyncExecutionResponse
mkStartSyncExecutionResponse
  executionArn
  startDate
  stopDate
  status
  responseStatus =
    StartSyncExecutionResponse'
      { executionArn,
        startDate,
        stopDate,
        status,
        billingDetails = Core.Nothing,
        cause = Core.Nothing,
        error = Core.Nothing,
        input = Core.Nothing,
        inputDetails = Core.Nothing,
        name = Core.Nothing,
        output = Core.Nothing,
        outputDetails = Core.Nothing,
        stateMachineArn = Core.Nothing,
        traceHeader = Core.Nothing,
        responseStatus
      }

-- | The Amazon Resource Name (ARN) that identifies the execution.
--
-- /Note:/ Consider using 'executionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsExecutionArn :: Lens.Lens' StartSyncExecutionResponse Types.ExecutionArn
sserrsExecutionArn = Lens.field @"executionArn"
{-# DEPRECATED sserrsExecutionArn "Use generic-lens or generic-optics with 'executionArn' instead." #-}

-- | The date the execution is started.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsStartDate :: Lens.Lens' StartSyncExecutionResponse Core.NominalDiffTime
sserrsStartDate = Lens.field @"startDate"
{-# DEPRECATED sserrsStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | If the execution has already ended, the date the execution stopped.
--
-- /Note:/ Consider using 'stopDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsStopDate :: Lens.Lens' StartSyncExecutionResponse Core.NominalDiffTime
sserrsStopDate = Lens.field @"stopDate"
{-# DEPRECATED sserrsStopDate "Use generic-lens or generic-optics with 'stopDate' instead." #-}

-- | The current status of the execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsStatus :: Lens.Lens' StartSyncExecutionResponse Types.SyncExecutionStatus
sserrsStatus = Lens.field @"status"
{-# DEPRECATED sserrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An object that describes workflow billing details, including billed duration and memory use.
--
-- /Note:/ Consider using 'billingDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsBillingDetails :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.BillingDetails)
sserrsBillingDetails = Lens.field @"billingDetails"
{-# DEPRECATED sserrsBillingDetails "Use generic-lens or generic-optics with 'billingDetails' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsCause :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.Cause)
sserrsCause = Lens.field @"cause"
{-# DEPRECATED sserrsCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsError :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.SensitiveError)
sserrsError = Lens.field @"error"
{-# DEPRECATED sserrsError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsInput :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.SensitiveData)
sserrsInput = Lens.field @"input"
{-# DEPRECATED sserrsInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsInputDetails :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.CloudWatchEventsExecutionDataDetails)
sserrsInputDetails = Lens.field @"inputDetails"
{-# DEPRECATED sserrsInputDetails "Use generic-lens or generic-optics with 'inputDetails' instead." #-}

-- | The name of the execution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsName :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.Name)
sserrsName = Lens.field @"name"
{-# DEPRECATED sserrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsOutput :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.SensitiveData)
sserrsOutput = Lens.field @"output"
{-# DEPRECATED sserrsOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsOutputDetails :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.CloudWatchEventsExecutionDataDetails)
sserrsOutputDetails = Lens.field @"outputDetails"
{-# DEPRECATED sserrsOutputDetails "Use generic-lens or generic-optics with 'outputDetails' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the state machine.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsStateMachineArn :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.StateMachineArn)
sserrsStateMachineArn = Lens.field @"stateMachineArn"
{-# DEPRECATED sserrsStateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead." #-}

-- | The AWS X-Ray trace header that was passed to the execution.
--
-- /Note:/ Consider using 'traceHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsTraceHeader :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.TraceHeader)
sserrsTraceHeader = Lens.field @"traceHeader"
{-# DEPRECATED sserrsTraceHeader "Use generic-lens or generic-optics with 'traceHeader' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsResponseStatus :: Lens.Lens' StartSyncExecutionResponse Core.Int
sserrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sserrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
