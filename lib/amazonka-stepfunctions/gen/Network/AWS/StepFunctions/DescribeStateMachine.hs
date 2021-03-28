{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.DescribeStateMachine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a state machine.
module Network.AWS.StepFunctions.DescribeStateMachine
    (
    -- * Creating a request
      DescribeStateMachine (..)
    , mkDescribeStateMachine
    -- ** Request lenses
    , dsmStateMachineArn

    -- * Destructuring the response
    , DescribeStateMachineResponse (..)
    , mkDescribeStateMachineResponse
    -- ** Response lenses
    , dsmrrsStateMachineArn
    , dsmrrsName
    , dsmrrsDefinition
    , dsmrrsRoleArn
    , dsmrrsType
    , dsmrrsCreationDate
    , dsmrrsLoggingConfiguration
    , dsmrrsStatus
    , dsmrrsTracingConfiguration
    , dsmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkDescribeStateMachine' smart constructor.
newtype DescribeStateMachine = DescribeStateMachine'
  { stateMachineArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the state machine to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStateMachine' value with any optional fields omitted.
mkDescribeStateMachine
    :: Types.Arn -- ^ 'stateMachineArn'
    -> DescribeStateMachine
mkDescribeStateMachine stateMachineArn
  = DescribeStateMachine'{stateMachineArn}

-- | The Amazon Resource Name (ARN) of the state machine to describe.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmStateMachineArn :: Lens.Lens' DescribeStateMachine Types.Arn
dsmStateMachineArn = Lens.field @"stateMachineArn"
{-# INLINEABLE dsmStateMachineArn #-}
{-# DEPRECATED stateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead"  #-}

instance Core.ToQuery DescribeStateMachine where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeStateMachine where
        toHeaders DescribeStateMachine{..}
          = Core.pure
              ("X-Amz-Target", "AWSStepFunctions.DescribeStateMachine")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DescribeStateMachine where
        toJSON DescribeStateMachine{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("stateMachineArn" Core..= stateMachineArn)])

instance Core.AWSRequest DescribeStateMachine where
        type Rs DescribeStateMachine = DescribeStateMachineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeStateMachineResponse' Core.<$>
                   (x Core..: "stateMachineArn") Core.<*> x Core..: "name" Core.<*>
                     x Core..: "definition"
                     Core.<*> x Core..: "roleArn"
                     Core.<*> x Core..: "type"
                     Core.<*> x Core..: "creationDate"
                     Core.<*> x Core..:? "loggingConfiguration"
                     Core.<*> x Core..:? "status"
                     Core.<*> x Core..:? "tracingConfiguration"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeStateMachineResponse' smart constructor.
data DescribeStateMachineResponse = DescribeStateMachineResponse'
  { stateMachineArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that identifies the state machine.
  , name :: Types.Name
    -- ^ The name of the state machine.
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
  , definition :: Types.Definition
    -- ^ The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
  , roleArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the IAM role used when creating this state machine. (The IAM role maintains security by granting Step Functions access to AWS resources.)
  , type' :: Types.StateMachineType
    -- ^ The @type@ of the state machine (@STANDARD@ or @EXPRESS@ ).
  , creationDate :: Core.NominalDiffTime
    -- ^ The date the state machine is created.
  , loggingConfiguration :: Core.Maybe Types.LoggingConfiguration
  , status :: Core.Maybe Types.StateMachineStatus
    -- ^ The current status of the state machine.
  , tracingConfiguration :: Core.Maybe Types.TracingConfiguration
    -- ^ Selects whether AWS X-Ray tracing is enabled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeStateMachineResponse' value with any optional fields omitted.
mkDescribeStateMachineResponse
    :: Types.Arn -- ^ 'stateMachineArn'
    -> Types.Name -- ^ 'name'
    -> Types.Definition -- ^ 'definition'
    -> Types.Arn -- ^ 'roleArn'
    -> Types.StateMachineType -- ^ 'type\''
    -> Core.NominalDiffTime -- ^ 'creationDate'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeStateMachineResponse
mkDescribeStateMachineResponse stateMachineArn name definition
  roleArn type' creationDate responseStatus
  = DescribeStateMachineResponse'{stateMachineArn, name, definition,
                                  roleArn, type', creationDate, loggingConfiguration = Core.Nothing,
                                  status = Core.Nothing, tracingConfiguration = Core.Nothing,
                                  responseStatus}

-- | The Amazon Resource Name (ARN) that identifies the state machine.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsStateMachineArn :: Lens.Lens' DescribeStateMachineResponse Types.Arn
dsmrrsStateMachineArn = Lens.field @"stateMachineArn"
{-# INLINEABLE dsmrrsStateMachineArn #-}
{-# DEPRECATED stateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead"  #-}

-- | The name of the state machine.
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
dsmrrsName :: Lens.Lens' DescribeStateMachineResponse Types.Name
dsmrrsName = Lens.field @"name"
{-# INLINEABLE dsmrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsDefinition :: Lens.Lens' DescribeStateMachineResponse Types.Definition
dsmrrsDefinition = Lens.field @"definition"
{-# INLINEABLE dsmrrsDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role used when creating this state machine. (The IAM role maintains security by granting Step Functions access to AWS resources.)
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsRoleArn :: Lens.Lens' DescribeStateMachineResponse Types.Arn
dsmrrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dsmrrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The @type@ of the state machine (@STANDARD@ or @EXPRESS@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsType :: Lens.Lens' DescribeStateMachineResponse Types.StateMachineType
dsmrrsType = Lens.field @"type'"
{-# INLINEABLE dsmrrsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The date the state machine is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsCreationDate :: Lens.Lens' DescribeStateMachineResponse Core.NominalDiffTime
dsmrrsCreationDate = Lens.field @"creationDate"
{-# INLINEABLE dsmrrsCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsLoggingConfiguration :: Lens.Lens' DescribeStateMachineResponse (Core.Maybe Types.LoggingConfiguration)
dsmrrsLoggingConfiguration = Lens.field @"loggingConfiguration"
{-# INLINEABLE dsmrrsLoggingConfiguration #-}
{-# DEPRECATED loggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead"  #-}

-- | The current status of the state machine.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsStatus :: Lens.Lens' DescribeStateMachineResponse (Core.Maybe Types.StateMachineStatus)
dsmrrsStatus = Lens.field @"status"
{-# INLINEABLE dsmrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Selects whether AWS X-Ray tracing is enabled.
--
-- /Note:/ Consider using 'tracingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsTracingConfiguration :: Lens.Lens' DescribeStateMachineResponse (Core.Maybe Types.TracingConfiguration)
dsmrrsTracingConfiguration = Lens.field @"tracingConfiguration"
{-# INLINEABLE dsmrrsTracingConfiguration #-}
{-# DEPRECATED tracingConfiguration "Use generic-lens or generic-optics with 'tracingConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsResponseStatus :: Lens.Lens' DescribeStateMachineResponse Core.Int
dsmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
