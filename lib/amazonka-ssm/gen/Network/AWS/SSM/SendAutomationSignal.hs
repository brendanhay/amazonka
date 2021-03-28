{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.SendAutomationSignal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a signal to an Automation execution to change the current behavior or status of the execution. 
module Network.AWS.SSM.SendAutomationSignal
    (
    -- * Creating a request
      SendAutomationSignal (..)
    , mkSendAutomationSignal
    -- ** Request lenses
    , sasAutomationExecutionId
    , sasSignalType
    , sasPayload

    -- * Destructuring the response
    , SendAutomationSignalResponse (..)
    , mkSendAutomationSignalResponse
    -- ** Response lenses
    , sasrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkSendAutomationSignal' smart constructor.
data SendAutomationSignal = SendAutomationSignal'
  { automationExecutionId :: Types.AutomationExecutionId
    -- ^ The unique identifier for an existing Automation execution that you want to send the signal to.
  , signalType :: Types.SignalType
    -- ^ The type of signal to send to an Automation execution. 
  , payload :: Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue])
    -- ^ The data sent with the signal. The data schema depends on the type of signal used in the request.
--
-- For @Approve@ and @Reject@ signal types, the payload is an optional comment that you can send with the signal type. For example:
-- @Comment="Looks good"@ 
-- For @StartStep@ and @Resume@ signal types, you must send the name of the Automation step to start or resume as the payload. For example:
-- @StepName="step1"@ 
-- For the @StopStep@ signal type, you must send the step execution ID as the payload. For example:
-- @StepExecutionId="97fff367-fc5a-4299-aed8-0123456789ab"@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendAutomationSignal' value with any optional fields omitted.
mkSendAutomationSignal
    :: Types.AutomationExecutionId -- ^ 'automationExecutionId'
    -> Types.SignalType -- ^ 'signalType'
    -> SendAutomationSignal
mkSendAutomationSignal automationExecutionId signalType
  = SendAutomationSignal'{automationExecutionId, signalType,
                          payload = Core.Nothing}

-- | The unique identifier for an existing Automation execution that you want to send the signal to.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasAutomationExecutionId :: Lens.Lens' SendAutomationSignal Types.AutomationExecutionId
sasAutomationExecutionId = Lens.field @"automationExecutionId"
{-# INLINEABLE sasAutomationExecutionId #-}
{-# DEPRECATED automationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead"  #-}

-- | The type of signal to send to an Automation execution. 
--
-- /Note:/ Consider using 'signalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasSignalType :: Lens.Lens' SendAutomationSignal Types.SignalType
sasSignalType = Lens.field @"signalType"
{-# INLINEABLE sasSignalType #-}
{-# DEPRECATED signalType "Use generic-lens or generic-optics with 'signalType' instead"  #-}

-- | The data sent with the signal. The data schema depends on the type of signal used in the request.
--
-- For @Approve@ and @Reject@ signal types, the payload is an optional comment that you can send with the signal type. For example:
-- @Comment="Looks good"@ 
-- For @StartStep@ and @Resume@ signal types, you must send the name of the Automation step to start or resume as the payload. For example:
-- @StepName="step1"@ 
-- For the @StopStep@ signal type, you must send the step execution ID as the payload. For example:
-- @StepExecutionId="97fff367-fc5a-4299-aed8-0123456789ab"@ 
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasPayload :: Lens.Lens' SendAutomationSignal (Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]))
sasPayload = Lens.field @"payload"
{-# INLINEABLE sasPayload #-}
{-# DEPRECATED payload "Use generic-lens or generic-optics with 'payload' instead"  #-}

instance Core.ToQuery SendAutomationSignal where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SendAutomationSignal where
        toHeaders SendAutomationSignal{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.SendAutomationSignal")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SendAutomationSignal where
        toJSON SendAutomationSignal{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AutomationExecutionId" Core..= automationExecutionId),
                  Core.Just ("SignalType" Core..= signalType),
                  ("Payload" Core..=) Core.<$> payload])

instance Core.AWSRequest SendAutomationSignal where
        type Rs SendAutomationSignal = SendAutomationSignalResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 SendAutomationSignalResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSendAutomationSignalResponse' smart constructor.
newtype SendAutomationSignalResponse = SendAutomationSignalResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SendAutomationSignalResponse' value with any optional fields omitted.
mkSendAutomationSignalResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SendAutomationSignalResponse
mkSendAutomationSignalResponse responseStatus
  = SendAutomationSignalResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasrrsResponseStatus :: Lens.Lens' SendAutomationSignalResponse Core.Int
sasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
