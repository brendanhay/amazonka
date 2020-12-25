{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskSubmittedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskSubmittedEventDetails
  ( TaskSubmittedEventDetails (..),

    -- * Smart constructor
    mkTaskSubmittedEventDetails,

    -- * Lenses
    tsedfResourceType,
    tsedfResource,
    tsedfOutput,
    tsedfOutputDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails as Types
import qualified Network.AWS.StepFunctions.Types.Resource as Types
import qualified Network.AWS.StepFunctions.Types.ResourceType as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveData as Types

-- | Contains details about a task submitted to a resource .
--
-- /See:/ 'mkTaskSubmittedEventDetails' smart constructor.
data TaskSubmittedEventDetails = TaskSubmittedEventDetails'
  { -- | The action of the resource called by a task state.
    resourceType :: Types.ResourceType,
    -- | The service name of the resource in a task state.
    resource :: Types.Resource,
    -- | The response from a resource when a task has started. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Core.Maybe Types.SensitiveData,
    -- | Contains details about the output of an execution history event.
    outputDetails :: Core.Maybe Types.HistoryEventExecutionDataDetails
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskSubmittedEventDetails' value with any optional fields omitted.
mkTaskSubmittedEventDetails ::
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'resource'
  Types.Resource ->
  TaskSubmittedEventDetails
mkTaskSubmittedEventDetails resourceType resource =
  TaskSubmittedEventDetails'
    { resourceType,
      resource,
      output = Core.Nothing,
      outputDetails = Core.Nothing
    }

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedfResourceType :: Lens.Lens' TaskSubmittedEventDetails Types.ResourceType
tsedfResourceType = Lens.field @"resourceType"
{-# DEPRECATED tsedfResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedfResource :: Lens.Lens' TaskSubmittedEventDetails Types.Resource
tsedfResource = Lens.field @"resource"
{-# DEPRECATED tsedfResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | The response from a resource when a task has started. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedfOutput :: Lens.Lens' TaskSubmittedEventDetails (Core.Maybe Types.SensitiveData)
tsedfOutput = Lens.field @"output"
{-# DEPRECATED tsedfOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | Contains details about the output of an execution history event.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedfOutputDetails :: Lens.Lens' TaskSubmittedEventDetails (Core.Maybe Types.HistoryEventExecutionDataDetails)
tsedfOutputDetails = Lens.field @"outputDetails"
{-# DEPRECATED tsedfOutputDetails "Use generic-lens or generic-optics with 'outputDetails' instead." #-}

instance Core.FromJSON TaskSubmittedEventDetails where
  parseJSON =
    Core.withObject "TaskSubmittedEventDetails" Core.$
      \x ->
        TaskSubmittedEventDetails'
          Core.<$> (x Core..: "resourceType")
          Core.<*> (x Core..: "resource")
          Core.<*> (x Core..:? "output")
          Core.<*> (x Core..:? "outputDetails")
