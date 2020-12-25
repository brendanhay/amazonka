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
    deExecutionArn,

    -- * Destructuring the response
    DescribeExecutionResponse (..),
    mkDescribeExecutionResponse,

    -- ** Response lenses
    derrsExecutionArn,
    derrsStateMachineArn,
    derrsStatus,
    derrsStartDate,
    derrsInput,
    derrsInputDetails,
    derrsName,
    derrsOutput,
    derrsOutputDetails,
    derrsStopDate,
    derrsTraceHeader,
    derrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkDescribeExecution' smart constructor.
newtype DescribeExecution = DescribeExecution'
  { -- | The Amazon Resource Name (ARN) of the execution to describe.
    executionArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExecution' value with any optional fields omitted.
mkDescribeExecution ::
  -- | 'executionArn'
  Types.Arn ->
  DescribeExecution
mkDescribeExecution executionArn = DescribeExecution' {executionArn}

-- | The Amazon Resource Name (ARN) of the execution to describe.
--
-- /Note:/ Consider using 'executionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExecutionArn :: Lens.Lens' DescribeExecution Types.Arn
deExecutionArn = Lens.field @"executionArn"
{-# DEPRECATED deExecutionArn "Use generic-lens or generic-optics with 'executionArn' instead." #-}

instance Core.FromJSON DescribeExecution where
  toJSON DescribeExecution {..} =
    Core.object
      (Core.catMaybes [Core.Just ("executionArn" Core..= executionArn)])

instance Core.AWSRequest DescribeExecution where
  type Rs DescribeExecution = DescribeExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSStepFunctions.DescribeExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExecutionResponse'
            Core.<$> (x Core..: "executionArn")
            Core.<*> (x Core..: "stateMachineArn")
            Core.<*> (x Core..: "status")
            Core.<*> (x Core..: "startDate")
            Core.<*> (x Core..:? "input")
            Core.<*> (x Core..:? "inputDetails")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "output")
            Core.<*> (x Core..:? "outputDetails")
            Core.<*> (x Core..:? "stopDate")
            Core.<*> (x Core..:? "traceHeader")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeExecutionResponse' smart constructor.
data DescribeExecutionResponse = DescribeExecutionResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the execution.
    executionArn :: Types.Arn,
    -- | The Amazon Resource Name (ARN) of the executed stated machine.
    stateMachineArn :: Types.Arn,
    -- | The current status of the execution.
    status :: Types.ExecutionStatus,
    -- | The date the execution is started.
    startDate :: Core.NominalDiffTime,
    -- | The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Core.Maybe Types.SensitiveData,
    inputDetails :: Core.Maybe Types.CloudWatchEventsExecutionDataDetails,
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
    name :: Core.Maybe Types.Name,
    -- | The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Core.Maybe Types.SensitiveData,
    outputDetails :: Core.Maybe Types.CloudWatchEventsExecutionDataDetails,
    -- | If the execution has already ended, the date the execution stopped.
    stopDate :: Core.Maybe Core.NominalDiffTime,
    -- | The AWS X-Ray trace header that was passed to the execution.
    traceHeader :: Core.Maybe Types.TraceHeader,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeExecutionResponse' value with any optional fields omitted.
mkDescribeExecutionResponse ::
  -- | 'executionArn'
  Types.Arn ->
  -- | 'stateMachineArn'
  Types.Arn ->
  -- | 'status'
  Types.ExecutionStatus ->
  -- | 'startDate'
  Core.NominalDiffTime ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeExecutionResponse
mkDescribeExecutionResponse
  executionArn
  stateMachineArn
  status
  startDate
  responseStatus =
    DescribeExecutionResponse'
      { executionArn,
        stateMachineArn,
        status,
        startDate,
        input = Core.Nothing,
        inputDetails = Core.Nothing,
        name = Core.Nothing,
        output = Core.Nothing,
        outputDetails = Core.Nothing,
        stopDate = Core.Nothing,
        traceHeader = Core.Nothing,
        responseStatus
      }

-- | The Amazon Resource Name (ARN) that identifies the execution.
--
-- /Note:/ Consider using 'executionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsExecutionArn :: Lens.Lens' DescribeExecutionResponse Types.Arn
derrsExecutionArn = Lens.field @"executionArn"
{-# DEPRECATED derrsExecutionArn "Use generic-lens or generic-optics with 'executionArn' instead." #-}

-- | The Amazon Resource Name (ARN) of the executed stated machine.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsStateMachineArn :: Lens.Lens' DescribeExecutionResponse Types.Arn
derrsStateMachineArn = Lens.field @"stateMachineArn"
{-# DEPRECATED derrsStateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead." #-}

-- | The current status of the execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsStatus :: Lens.Lens' DescribeExecutionResponse Types.ExecutionStatus
derrsStatus = Lens.field @"status"
{-# DEPRECATED derrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date the execution is started.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsStartDate :: Lens.Lens' DescribeExecutionResponse Core.NominalDiffTime
derrsStartDate = Lens.field @"startDate"
{-# DEPRECATED derrsStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsInput :: Lens.Lens' DescribeExecutionResponse (Core.Maybe Types.SensitiveData)
derrsInput = Lens.field @"input"
{-# DEPRECATED derrsInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsInputDetails :: Lens.Lens' DescribeExecutionResponse (Core.Maybe Types.CloudWatchEventsExecutionDataDetails)
derrsInputDetails = Lens.field @"inputDetails"
{-# DEPRECATED derrsInputDetails "Use generic-lens or generic-optics with 'inputDetails' instead." #-}

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
derrsName :: Lens.Lens' DescribeExecutionResponse (Core.Maybe Types.Name)
derrsName = Lens.field @"name"
{-# DEPRECATED derrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsOutput :: Lens.Lens' DescribeExecutionResponse (Core.Maybe Types.SensitiveData)
derrsOutput = Lens.field @"output"
{-# DEPRECATED derrsOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsOutputDetails :: Lens.Lens' DescribeExecutionResponse (Core.Maybe Types.CloudWatchEventsExecutionDataDetails)
derrsOutputDetails = Lens.field @"outputDetails"
{-# DEPRECATED derrsOutputDetails "Use generic-lens or generic-optics with 'outputDetails' instead." #-}

-- | If the execution has already ended, the date the execution stopped.
--
-- /Note:/ Consider using 'stopDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsStopDate :: Lens.Lens' DescribeExecutionResponse (Core.Maybe Core.NominalDiffTime)
derrsStopDate = Lens.field @"stopDate"
{-# DEPRECATED derrsStopDate "Use generic-lens or generic-optics with 'stopDate' instead." #-}

-- | The AWS X-Ray trace header that was passed to the execution.
--
-- /Note:/ Consider using 'traceHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsTraceHeader :: Lens.Lens' DescribeExecutionResponse (Core.Maybe Types.TraceHeader)
derrsTraceHeader = Lens.field @"traceHeader"
{-# DEPRECATED derrsTraceHeader "Use generic-lens or generic-optics with 'traceHeader' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DescribeExecutionResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
