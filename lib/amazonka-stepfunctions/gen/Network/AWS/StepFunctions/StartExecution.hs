{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartExecution (..)
    , mkStartExecution
    -- ** Request lenses
    , seStateMachineArn
    , seInput
    , seName
    , seTraceHeader

    -- * Destructuring the response
    , StartExecutionResponse (..)
    , mkStartExecutionResponse
    -- ** Response lenses
    , srsExecutionArn
    , srsStartDate
    , srsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkStartExecution' smart constructor.
data StartExecution = StartExecution'
  { stateMachineArn :: Types.StateMachineArn
    -- ^ The Amazon Resource Name (ARN) of the state machine to execute.
  , input :: Core.Maybe Types.Input
    -- ^ The string that contains the JSON input data for the execution, for example:
--
-- @"input": "{\"first_name\" : \"test\"}"@ 
-- Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the execution. This name must be unique for your AWS account, region, and state machine for 90 days. For more information, see <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions> in the /AWS Step Functions Developer Guide/ .
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
  , traceHeader :: Core.Maybe Types.TraceHeader
    -- ^ Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartExecution' value with any optional fields omitted.
mkStartExecution
    :: Types.StateMachineArn -- ^ 'stateMachineArn'
    -> StartExecution
mkStartExecution stateMachineArn
  = StartExecution'{stateMachineArn, input = Core.Nothing,
                    name = Core.Nothing, traceHeader = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the state machine to execute.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStateMachineArn :: Lens.Lens' StartExecution Types.StateMachineArn
seStateMachineArn = Lens.field @"stateMachineArn"
{-# INLINEABLE seStateMachineArn #-}
{-# DEPRECATED stateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead"  #-}

-- | The string that contains the JSON input data for the execution, for example:
--
-- @"input": "{\"first_name\" : \"test\"}"@ 
-- Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seInput :: Lens.Lens' StartExecution (Core.Maybe Types.Input)
seInput = Lens.field @"input"
{-# INLINEABLE seInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

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
seName :: Lens.Lens' StartExecution (Core.Maybe Types.Name)
seName = Lens.field @"name"
{-# INLINEABLE seName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
--
-- /Note:/ Consider using 'traceHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTraceHeader :: Lens.Lens' StartExecution (Core.Maybe Types.TraceHeader)
seTraceHeader = Lens.field @"traceHeader"
{-# INLINEABLE seTraceHeader #-}
{-# DEPRECATED traceHeader "Use generic-lens or generic-optics with 'traceHeader' instead"  #-}

instance Core.ToQuery StartExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartExecution where
        toHeaders StartExecution{..}
          = Core.pure ("X-Amz-Target", "AWSStepFunctions.StartExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON StartExecution where
        toJSON StartExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("stateMachineArn" Core..= stateMachineArn),
                  ("input" Core..=) Core.<$> input, ("name" Core..=) Core.<$> name,
                  ("traceHeader" Core..=) Core.<$> traceHeader])

instance Core.AWSRequest StartExecution where
        type Rs StartExecution = StartExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartExecutionResponse' Core.<$>
                   (x Core..: "executionArn") Core.<*> x Core..: "startDate" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartExecutionResponse' smart constructor.
data StartExecutionResponse = StartExecutionResponse'
  { executionArn :: Types.ExecutionArn
    -- ^ The Amazon Resource Name (ARN) that identifies the execution.
  , startDate :: Core.NominalDiffTime
    -- ^ The date the execution is started.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartExecutionResponse' value with any optional fields omitted.
mkStartExecutionResponse
    :: Types.ExecutionArn -- ^ 'executionArn'
    -> Core.NominalDiffTime -- ^ 'startDate'
    -> Core.Int -- ^ 'responseStatus'
    -> StartExecutionResponse
mkStartExecutionResponse executionArn startDate responseStatus
  = StartExecutionResponse'{executionArn, startDate, responseStatus}

-- | The Amazon Resource Name (ARN) that identifies the execution.
--
-- /Note:/ Consider using 'executionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsExecutionArn :: Lens.Lens' StartExecutionResponse Types.ExecutionArn
srsExecutionArn = Lens.field @"executionArn"
{-# INLINEABLE srsExecutionArn #-}
{-# DEPRECATED executionArn "Use generic-lens or generic-optics with 'executionArn' instead"  #-}

-- | The date the execution is started.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsStartDate :: Lens.Lens' StartExecutionResponse Core.NominalDiffTime
srsStartDate = Lens.field @"startDate"
{-# INLINEABLE srsStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartExecutionResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
