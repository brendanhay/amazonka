{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.UpdateStateMachine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing state machine by modifying its @definition@ , @roleArn@ , or @loggingConfiguration@ . Running executions will continue to use the previous @definition@ and @roleArn@ . You must include at least one of @definition@ or @roleArn@ or you will receive a @MissingRequiredParameter@ error.
module Network.AWS.StepFunctions.UpdateStateMachine
    (
    -- * Creating a request
      UpdateStateMachine (..)
    , mkUpdateStateMachine
    -- ** Request lenses
    , usmStateMachineArn
    , usmDefinition
    , usmLoggingConfiguration
    , usmRoleArn
    , usmTracingConfiguration

    -- * Destructuring the response
    , UpdateStateMachineResponse (..)
    , mkUpdateStateMachineResponse
    -- ** Response lenses
    , usmrrsUpdateDate
    , usmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkUpdateStateMachine' smart constructor.
data UpdateStateMachine = UpdateStateMachine'
  { stateMachineArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the state machine.
  , definition :: Core.Maybe Types.Definition
    -- ^ The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
  , loggingConfiguration :: Core.Maybe Types.LoggingConfiguration
    -- ^ The @LoggingConfiguration@ data type is used to set CloudWatch Logs options.
  , roleArn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the IAM role of the state machine.
  , tracingConfiguration :: Core.Maybe Types.TracingConfiguration
    -- ^ Selects whether AWS X-Ray tracing is enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStateMachine' value with any optional fields omitted.
mkUpdateStateMachine
    :: Types.Arn -- ^ 'stateMachineArn'
    -> UpdateStateMachine
mkUpdateStateMachine stateMachineArn
  = UpdateStateMachine'{stateMachineArn, definition = Core.Nothing,
                        loggingConfiguration = Core.Nothing, roleArn = Core.Nothing,
                        tracingConfiguration = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the state machine.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmStateMachineArn :: Lens.Lens' UpdateStateMachine Types.Arn
usmStateMachineArn = Lens.field @"stateMachineArn"
{-# INLINEABLE usmStateMachineArn #-}
{-# DEPRECATED stateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead"  #-}

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmDefinition :: Lens.Lens' UpdateStateMachine (Core.Maybe Types.Definition)
usmDefinition = Lens.field @"definition"
{-# INLINEABLE usmDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

-- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs options.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmLoggingConfiguration :: Lens.Lens' UpdateStateMachine (Core.Maybe Types.LoggingConfiguration)
usmLoggingConfiguration = Lens.field @"loggingConfiguration"
{-# INLINEABLE usmLoggingConfiguration #-}
{-# DEPRECATED loggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role of the state machine.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmRoleArn :: Lens.Lens' UpdateStateMachine (Core.Maybe Types.Arn)
usmRoleArn = Lens.field @"roleArn"
{-# INLINEABLE usmRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Selects whether AWS X-Ray tracing is enabled.
--
-- /Note:/ Consider using 'tracingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmTracingConfiguration :: Lens.Lens' UpdateStateMachine (Core.Maybe Types.TracingConfiguration)
usmTracingConfiguration = Lens.field @"tracingConfiguration"
{-# INLINEABLE usmTracingConfiguration #-}
{-# DEPRECATED tracingConfiguration "Use generic-lens or generic-optics with 'tracingConfiguration' instead"  #-}

instance Core.ToQuery UpdateStateMachine where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateStateMachine where
        toHeaders UpdateStateMachine{..}
          = Core.pure ("X-Amz-Target", "AWSStepFunctions.UpdateStateMachine")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON UpdateStateMachine where
        toJSON UpdateStateMachine{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("stateMachineArn" Core..= stateMachineArn),
                  ("definition" Core..=) Core.<$> definition,
                  ("loggingConfiguration" Core..=) Core.<$> loggingConfiguration,
                  ("roleArn" Core..=) Core.<$> roleArn,
                  ("tracingConfiguration" Core..=) Core.<$> tracingConfiguration])

instance Core.AWSRequest UpdateStateMachine where
        type Rs UpdateStateMachine = UpdateStateMachineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateStateMachineResponse' Core.<$>
                   (x Core..: "updateDate") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateStateMachineResponse' smart constructor.
data UpdateStateMachineResponse = UpdateStateMachineResponse'
  { updateDate :: Core.NominalDiffTime
    -- ^ The date and time the state machine was updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateStateMachineResponse' value with any optional fields omitted.
mkUpdateStateMachineResponse
    :: Core.NominalDiffTime -- ^ 'updateDate'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateStateMachineResponse
mkUpdateStateMachineResponse updateDate responseStatus
  = UpdateStateMachineResponse'{updateDate, responseStatus}

-- | The date and time the state machine was updated.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmrrsUpdateDate :: Lens.Lens' UpdateStateMachineResponse Core.NominalDiffTime
usmrrsUpdateDate = Lens.field @"updateDate"
{-# INLINEABLE usmrrsUpdateDate #-}
{-# DEPRECATED updateDate "Use generic-lens or generic-optics with 'updateDate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmrrsResponseStatus :: Lens.Lens' UpdateStateMachineResponse Core.Int
usmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
