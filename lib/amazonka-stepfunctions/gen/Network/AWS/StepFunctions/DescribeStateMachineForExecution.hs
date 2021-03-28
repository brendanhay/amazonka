{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.DescribeStateMachineForExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the state machine associated with a specific execution.
--
-- This API action is not supported by @EXPRESS@ state machines.
module Network.AWS.StepFunctions.DescribeStateMachineForExecution
    (
    -- * Creating a request
      DescribeStateMachineForExecution (..)
    , mkDescribeStateMachineForExecution
    -- ** Request lenses
    , dsmfeExecutionArn

    -- * Destructuring the response
    , DescribeStateMachineForExecutionResponse (..)
    , mkDescribeStateMachineForExecutionResponse
    -- ** Response lenses
    , dsmferrsStateMachineArn
    , dsmferrsName
    , dsmferrsDefinition
    , dsmferrsRoleArn
    , dsmferrsUpdateDate
    , dsmferrsLoggingConfiguration
    , dsmferrsTracingConfiguration
    , dsmferrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkDescribeStateMachineForExecution' smart constructor.
newtype DescribeStateMachineForExecution = DescribeStateMachineForExecution'
  { executionArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the execution you want state machine information for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStateMachineForExecution' value with any optional fields omitted.
mkDescribeStateMachineForExecution
    :: Types.Arn -- ^ 'executionArn'
    -> DescribeStateMachineForExecution
mkDescribeStateMachineForExecution executionArn
  = DescribeStateMachineForExecution'{executionArn}

-- | The Amazon Resource Name (ARN) of the execution you want state machine information for.
--
-- /Note:/ Consider using 'executionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmfeExecutionArn :: Lens.Lens' DescribeStateMachineForExecution Types.Arn
dsmfeExecutionArn = Lens.field @"executionArn"
{-# INLINEABLE dsmfeExecutionArn #-}
{-# DEPRECATED executionArn "Use generic-lens or generic-optics with 'executionArn' instead"  #-}

instance Core.ToQuery DescribeStateMachineForExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeStateMachineForExecution where
        toHeaders DescribeStateMachineForExecution{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSStepFunctions.DescribeStateMachineForExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DescribeStateMachineForExecution where
        toJSON DescribeStateMachineForExecution{..}
          = Core.object
              (Core.catMaybes [Core.Just ("executionArn" Core..= executionArn)])

instance Core.AWSRequest DescribeStateMachineForExecution where
        type Rs DescribeStateMachineForExecution =
             DescribeStateMachineForExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeStateMachineForExecutionResponse' Core.<$>
                   (x Core..: "stateMachineArn") Core.<*> x Core..: "name" Core.<*>
                     x Core..: "definition"
                     Core.<*> x Core..: "roleArn"
                     Core.<*> x Core..: "updateDate"
                     Core.<*> x Core..:? "loggingConfiguration"
                     Core.<*> x Core..:? "tracingConfiguration"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeStateMachineForExecutionResponse' smart constructor.
data DescribeStateMachineForExecutionResponse = DescribeStateMachineForExecutionResponse'
  { stateMachineArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the state machine associated with the execution.
  , name :: Types.Name
    -- ^ The name of the state machine associated with the execution.
  , definition :: Types.Definition
    -- ^ The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
  , roleArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the IAM role of the State Machine for the execution. 
  , updateDate :: Core.NominalDiffTime
    -- ^ The date and time the state machine associated with an execution was updated. For a newly created state machine, this is the creation date.
  , loggingConfiguration :: Core.Maybe Types.LoggingConfiguration
  , tracingConfiguration :: Core.Maybe Types.TracingConfiguration
    -- ^ Selects whether AWS X-Ray tracing is enabled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeStateMachineForExecutionResponse' value with any optional fields omitted.
mkDescribeStateMachineForExecutionResponse
    :: Types.Arn -- ^ 'stateMachineArn'
    -> Types.Name -- ^ 'name'
    -> Types.Definition -- ^ 'definition'
    -> Types.Arn -- ^ 'roleArn'
    -> Core.NominalDiffTime -- ^ 'updateDate'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeStateMachineForExecutionResponse
mkDescribeStateMachineForExecutionResponse stateMachineArn name
  definition roleArn updateDate responseStatus
  = DescribeStateMachineForExecutionResponse'{stateMachineArn, name,
                                              definition, roleArn, updateDate,
                                              loggingConfiguration = Core.Nothing,
                                              tracingConfiguration = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the state machine associated with the execution.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmferrsStateMachineArn :: Lens.Lens' DescribeStateMachineForExecutionResponse Types.Arn
dsmferrsStateMachineArn = Lens.field @"stateMachineArn"
{-# INLINEABLE dsmferrsStateMachineArn #-}
{-# DEPRECATED stateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead"  #-}

-- | The name of the state machine associated with the execution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmferrsName :: Lens.Lens' DescribeStateMachineForExecutionResponse Types.Name
dsmferrsName = Lens.field @"name"
{-# INLINEABLE dsmferrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmferrsDefinition :: Lens.Lens' DescribeStateMachineForExecutionResponse Types.Definition
dsmferrsDefinition = Lens.field @"definition"
{-# INLINEABLE dsmferrsDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role of the State Machine for the execution. 
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmferrsRoleArn :: Lens.Lens' DescribeStateMachineForExecutionResponse Types.Arn
dsmferrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dsmferrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The date and time the state machine associated with an execution was updated. For a newly created state machine, this is the creation date.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmferrsUpdateDate :: Lens.Lens' DescribeStateMachineForExecutionResponse Core.NominalDiffTime
dsmferrsUpdateDate = Lens.field @"updateDate"
{-# INLINEABLE dsmferrsUpdateDate #-}
{-# DEPRECATED updateDate "Use generic-lens or generic-optics with 'updateDate' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmferrsLoggingConfiguration :: Lens.Lens' DescribeStateMachineForExecutionResponse (Core.Maybe Types.LoggingConfiguration)
dsmferrsLoggingConfiguration = Lens.field @"loggingConfiguration"
{-# INLINEABLE dsmferrsLoggingConfiguration #-}
{-# DEPRECATED loggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead"  #-}

-- | Selects whether AWS X-Ray tracing is enabled.
--
-- /Note:/ Consider using 'tracingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmferrsTracingConfiguration :: Lens.Lens' DescribeStateMachineForExecutionResponse (Core.Maybe Types.TracingConfiguration)
dsmferrsTracingConfiguration = Lens.field @"tracingConfiguration"
{-# INLINEABLE dsmferrsTracingConfiguration #-}
{-# DEPRECATED tracingConfiguration "Use generic-lens or generic-optics with 'tracingConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmferrsResponseStatus :: Lens.Lens' DescribeStateMachineForExecutionResponse Core.Int
dsmferrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsmferrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
