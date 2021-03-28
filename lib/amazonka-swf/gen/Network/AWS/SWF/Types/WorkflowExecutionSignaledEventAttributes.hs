{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionSignaledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowExecutionSignaledEventAttributes
  ( WorkflowExecutionSignaledEventAttributes (..)
  -- * Smart constructor
  , mkWorkflowExecutionSignaledEventAttributes
  -- * Lenses
  , weseaSignalName
  , weseaExternalInitiatedEventId
  , weseaExternalWorkflowExecution
  , weseaInput
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.SignalName as Types
import qualified Network.AWS.SWF.Types.WorkflowExecution as Types

-- | Provides the details of the @WorkflowExecutionSignaled@ event.
--
-- /See:/ 'mkWorkflowExecutionSignaledEventAttributes' smart constructor.
data WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes'
  { signalName :: Types.SignalName
    -- ^ The name of the signal received. The decider can use the signal name and inputs to determine how to the process the signal.
  , externalInitiatedEventId :: Core.Maybe Core.Integer
    -- ^ The ID of the @SignalExternalWorkflowExecutionInitiated@ event corresponding to the @SignalExternalWorkflow@ decision to signal this workflow execution.The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event. This field is set only if the signal was initiated by another workflow execution.
  , externalWorkflowExecution :: Core.Maybe Types.WorkflowExecution
    -- ^ The workflow execution that sent the signal. This is set only of the signal was sent by another workflow execution.
  , input :: Core.Maybe Types.Data
    -- ^ The inputs provided with the signal. The decider can use the signal name and inputs to determine how to process the signal.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionSignaledEventAttributes' value with any optional fields omitted.
mkWorkflowExecutionSignaledEventAttributes
    :: Types.SignalName -- ^ 'signalName'
    -> WorkflowExecutionSignaledEventAttributes
mkWorkflowExecutionSignaledEventAttributes signalName
  = WorkflowExecutionSignaledEventAttributes'{signalName,
                                              externalInitiatedEventId = Core.Nothing,
                                              externalWorkflowExecution = Core.Nothing,
                                              input = Core.Nothing}

-- | The name of the signal received. The decider can use the signal name and inputs to determine how to the process the signal.
--
-- /Note:/ Consider using 'signalName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaSignalName :: Lens.Lens' WorkflowExecutionSignaledEventAttributes Types.SignalName
weseaSignalName = Lens.field @"signalName"
{-# INLINEABLE weseaSignalName #-}
{-# DEPRECATED signalName "Use generic-lens or generic-optics with 'signalName' instead"  #-}

-- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event corresponding to the @SignalExternalWorkflow@ decision to signal this workflow execution.The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event. This field is set only if the signal was initiated by another workflow execution.
--
-- /Note:/ Consider using 'externalInitiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaExternalInitiatedEventId :: Lens.Lens' WorkflowExecutionSignaledEventAttributes (Core.Maybe Core.Integer)
weseaExternalInitiatedEventId = Lens.field @"externalInitiatedEventId"
{-# INLINEABLE weseaExternalInitiatedEventId #-}
{-# DEPRECATED externalInitiatedEventId "Use generic-lens or generic-optics with 'externalInitiatedEventId' instead"  #-}

-- | The workflow execution that sent the signal. This is set only of the signal was sent by another workflow execution.
--
-- /Note:/ Consider using 'externalWorkflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaExternalWorkflowExecution :: Lens.Lens' WorkflowExecutionSignaledEventAttributes (Core.Maybe Types.WorkflowExecution)
weseaExternalWorkflowExecution = Lens.field @"externalWorkflowExecution"
{-# INLINEABLE weseaExternalWorkflowExecution #-}
{-# DEPRECATED externalWorkflowExecution "Use generic-lens or generic-optics with 'externalWorkflowExecution' instead"  #-}

-- | The inputs provided with the signal. The decider can use the signal name and inputs to determine how to process the signal.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaInput :: Lens.Lens' WorkflowExecutionSignaledEventAttributes (Core.Maybe Types.Data)
weseaInput = Lens.field @"input"
{-# INLINEABLE weseaInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

instance Core.FromJSON WorkflowExecutionSignaledEventAttributes
         where
        parseJSON
          = Core.withObject "WorkflowExecutionSignaledEventAttributes" Core.$
              \ x ->
                WorkflowExecutionSignaledEventAttributes' Core.<$>
                  (x Core..: "signalName") Core.<*>
                    x Core..:? "externalInitiatedEventId"
                    Core.<*> x Core..:? "externalWorkflowExecution"
                    Core.<*> x Core..:? "input"
