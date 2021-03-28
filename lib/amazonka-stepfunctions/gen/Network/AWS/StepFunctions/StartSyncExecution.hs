{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartSyncExecution (..)
    , mkStartSyncExecution
    -- ** Request lenses
    , sseStateMachineArn
    , sseInput
    , sseName
    , sseTraceHeader

    -- * Destructuring the response
    , StartSyncExecutionResponse (..)
    , mkStartSyncExecutionResponse
    -- ** Response lenses
    , sserrsExecutionArn
    , sserrsStartDate
    , sserrsStopDate
    , sserrsStatus
    , sserrsBillingDetails
    , sserrsCause
    , sserrsError
    , sserrsInput
    , sserrsInputDetails
    , sserrsName
    , sserrsOutput
    , sserrsOutputDetails
    , sserrsStateMachineArn
    , sserrsTraceHeader
    , sserrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkStartSyncExecution' smart constructor.
data StartSyncExecution = StartSyncExecution'
  { stateMachineArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the state machine to execute.
  , input :: Core.Maybe Types.SensitiveData
    -- ^ The string that contains the JSON input data for the execution, for example:
--
-- @"input": "{\"first_name\" : \"test\"}"@ 
-- Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the execution.
  , traceHeader :: Core.Maybe Types.TraceHeader
    -- ^ Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSyncExecution' value with any optional fields omitted.
mkStartSyncExecution
    :: Types.Arn -- ^ 'stateMachineArn'
    -> StartSyncExecution
mkStartSyncExecution stateMachineArn
  = StartSyncExecution'{stateMachineArn, input = Core.Nothing,
                        name = Core.Nothing, traceHeader = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the state machine to execute.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseStateMachineArn :: Lens.Lens' StartSyncExecution Types.Arn
sseStateMachineArn = Lens.field @"stateMachineArn"
{-# INLINEABLE sseStateMachineArn #-}
{-# DEPRECATED stateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead"  #-}

-- | The string that contains the JSON input data for the execution, for example:
--
-- @"input": "{\"first_name\" : \"test\"}"@ 
-- Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseInput :: Lens.Lens' StartSyncExecution (Core.Maybe Types.SensitiveData)
sseInput = Lens.field @"input"
{-# INLINEABLE sseInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | The name of the execution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseName :: Lens.Lens' StartSyncExecution (Core.Maybe Types.Name)
sseName = Lens.field @"name"
{-# INLINEABLE sseName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
--
-- /Note:/ Consider using 'traceHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseTraceHeader :: Lens.Lens' StartSyncExecution (Core.Maybe Types.TraceHeader)
sseTraceHeader = Lens.field @"traceHeader"
{-# INLINEABLE sseTraceHeader #-}
{-# DEPRECATED traceHeader "Use generic-lens or generic-optics with 'traceHeader' instead"  #-}

instance Core.ToQuery StartSyncExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartSyncExecution where
        toHeaders StartSyncExecution{..}
          = Core.pure ("X-Amz-Target", "AWSStepFunctions.StartSyncExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON StartSyncExecution where
        toJSON StartSyncExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("stateMachineArn" Core..= stateMachineArn),
                  ("input" Core..=) Core.<$> input, ("name" Core..=) Core.<$> name,
                  ("traceHeader" Core..=) Core.<$> traceHeader])

instance Core.AWSRequest StartSyncExecution where
        type Rs StartSyncExecution = StartSyncExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartSyncExecutionResponse' Core.<$>
                   (x Core..: "executionArn") Core.<*> x Core..: "startDate" Core.<*>
                     x Core..: "stopDate"
                     Core.<*> x Core..: "status"
                     Core.<*> x Core..:? "billingDetails"
                     Core.<*> x Core..:? "cause"
                     Core.<*> x Core..:? "error"
                     Core.<*> x Core..:? "input"
                     Core.<*> x Core..:? "inputDetails"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "output"
                     Core.<*> x Core..:? "outputDetails"
                     Core.<*> x Core..:? "stateMachineArn"
                     Core.<*> x Core..:? "traceHeader"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartSyncExecutionResponse' smart constructor.
data StartSyncExecutionResponse = StartSyncExecutionResponse'
  { executionArn :: Types.ExecutionArn
    -- ^ The Amazon Resource Name (ARN) that identifies the execution.
  , startDate :: Core.NominalDiffTime
    -- ^ The date the execution is started.
  , stopDate :: Core.NominalDiffTime
    -- ^ If the execution has already ended, the date the execution stopped.
  , status :: Types.SyncExecutionStatus
    -- ^ The current status of the execution.
  , billingDetails :: Core.Maybe Types.BillingDetails
    -- ^ An object that describes workflow billing details, including billed duration and memory use.
  , cause :: Core.Maybe Types.Cause
    -- ^ A more detailed explanation of the cause of the failure.
  , error :: Core.Maybe Types.SensitiveError
    -- ^ The error code of the failure.
  , input :: Core.Maybe Types.SensitiveData
    -- ^ The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
  , inputDetails :: Core.Maybe Types.CloudWatchEventsExecutionDataDetails
  , name :: Core.Maybe Types.Name
    -- ^ The name of the execution.
  , output :: Core.Maybe Types.SensitiveData
    -- ^ The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
  , outputDetails :: Core.Maybe Types.CloudWatchEventsExecutionDataDetails
  , stateMachineArn :: Core.Maybe Types.StateMachineArn
    -- ^ The Amazon Resource Name (ARN) that identifies the state machine.
  , traceHeader :: Core.Maybe Types.TraceHeader
    -- ^ The AWS X-Ray trace header that was passed to the execution.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartSyncExecutionResponse' value with any optional fields omitted.
mkStartSyncExecutionResponse
    :: Types.ExecutionArn -- ^ 'executionArn'
    -> Core.NominalDiffTime -- ^ 'startDate'
    -> Core.NominalDiffTime -- ^ 'stopDate'
    -> Types.SyncExecutionStatus -- ^ 'status'
    -> Core.Int -- ^ 'responseStatus'
    -> StartSyncExecutionResponse
mkStartSyncExecutionResponse executionArn startDate stopDate status
  responseStatus
  = StartSyncExecutionResponse'{executionArn, startDate, stopDate,
                                status, billingDetails = Core.Nothing, cause = Core.Nothing,
                                error = Core.Nothing, input = Core.Nothing,
                                inputDetails = Core.Nothing, name = Core.Nothing,
                                output = Core.Nothing, outputDetails = Core.Nothing,
                                stateMachineArn = Core.Nothing, traceHeader = Core.Nothing,
                                responseStatus}

-- | The Amazon Resource Name (ARN) that identifies the execution.
--
-- /Note:/ Consider using 'executionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsExecutionArn :: Lens.Lens' StartSyncExecutionResponse Types.ExecutionArn
sserrsExecutionArn = Lens.field @"executionArn"
{-# INLINEABLE sserrsExecutionArn #-}
{-# DEPRECATED executionArn "Use generic-lens or generic-optics with 'executionArn' instead"  #-}

-- | The date the execution is started.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsStartDate :: Lens.Lens' StartSyncExecutionResponse Core.NominalDiffTime
sserrsStartDate = Lens.field @"startDate"
{-# INLINEABLE sserrsStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

-- | If the execution has already ended, the date the execution stopped.
--
-- /Note:/ Consider using 'stopDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsStopDate :: Lens.Lens' StartSyncExecutionResponse Core.NominalDiffTime
sserrsStopDate = Lens.field @"stopDate"
{-# INLINEABLE sserrsStopDate #-}
{-# DEPRECATED stopDate "Use generic-lens or generic-optics with 'stopDate' instead"  #-}

-- | The current status of the execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsStatus :: Lens.Lens' StartSyncExecutionResponse Types.SyncExecutionStatus
sserrsStatus = Lens.field @"status"
{-# INLINEABLE sserrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | An object that describes workflow billing details, including billed duration and memory use.
--
-- /Note:/ Consider using 'billingDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsBillingDetails :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.BillingDetails)
sserrsBillingDetails = Lens.field @"billingDetails"
{-# INLINEABLE sserrsBillingDetails #-}
{-# DEPRECATED billingDetails "Use generic-lens or generic-optics with 'billingDetails' instead"  #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsCause :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.Cause)
sserrsCause = Lens.field @"cause"
{-# INLINEABLE sserrsCause #-}
{-# DEPRECATED cause "Use generic-lens or generic-optics with 'cause' instead"  #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsError :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.SensitiveError)
sserrsError = Lens.field @"error"
{-# INLINEABLE sserrsError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

-- | The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsInput :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.SensitiveData)
sserrsInput = Lens.field @"input"
{-# INLINEABLE sserrsInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsInputDetails :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.CloudWatchEventsExecutionDataDetails)
sserrsInputDetails = Lens.field @"inputDetails"
{-# INLINEABLE sserrsInputDetails #-}
{-# DEPRECATED inputDetails "Use generic-lens or generic-optics with 'inputDetails' instead"  #-}

-- | The name of the execution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsName :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.Name)
sserrsName = Lens.field @"name"
{-# INLINEABLE sserrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsOutput :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.SensitiveData)
sserrsOutput = Lens.field @"output"
{-# INLINEABLE sserrsOutput #-}
{-# DEPRECATED output "Use generic-lens or generic-optics with 'output' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsOutputDetails :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.CloudWatchEventsExecutionDataDetails)
sserrsOutputDetails = Lens.field @"outputDetails"
{-# INLINEABLE sserrsOutputDetails #-}
{-# DEPRECATED outputDetails "Use generic-lens or generic-optics with 'outputDetails' instead"  #-}

-- | The Amazon Resource Name (ARN) that identifies the state machine.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsStateMachineArn :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.StateMachineArn)
sserrsStateMachineArn = Lens.field @"stateMachineArn"
{-# INLINEABLE sserrsStateMachineArn #-}
{-# DEPRECATED stateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead"  #-}

-- | The AWS X-Ray trace header that was passed to the execution.
--
-- /Note:/ Consider using 'traceHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsTraceHeader :: Lens.Lens' StartSyncExecutionResponse (Core.Maybe Types.TraceHeader)
sserrsTraceHeader = Lens.field @"traceHeader"
{-# INLINEABLE sserrsTraceHeader #-}
{-# DEPRECATED traceHeader "Use generic-lens or generic-optics with 'traceHeader' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsResponseStatus :: Lens.Lens' StartSyncExecutionResponse Core.Int
sserrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sserrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
