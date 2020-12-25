{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskStartedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskStartedEventDetails
  ( TaskStartedEventDetails (..),

    -- * Smart constructor
    mkTaskStartedEventDetails,

    -- * Lenses
    tsedsResourceType,
    tsedsResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.Resource as Types
import qualified Network.AWS.StepFunctions.Types.ResourceType as Types

-- | Contains details about the start of a task during an execution.
--
-- /See:/ 'mkTaskStartedEventDetails' smart constructor.
data TaskStartedEventDetails = TaskStartedEventDetails'
  { -- | The action of the resource called by a task state.
    resourceType :: Types.ResourceType,
    -- | The service name of the resource in a task state.
    resource :: Types.Resource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskStartedEventDetails' value with any optional fields omitted.
mkTaskStartedEventDetails ::
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'resource'
  Types.Resource ->
  TaskStartedEventDetails
mkTaskStartedEventDetails resourceType resource =
  TaskStartedEventDetails' {resourceType, resource}

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedsResourceType :: Lens.Lens' TaskStartedEventDetails Types.ResourceType
tsedsResourceType = Lens.field @"resourceType"
{-# DEPRECATED tsedsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedsResource :: Lens.Lens' TaskStartedEventDetails Types.Resource
tsedsResource = Lens.field @"resource"
{-# DEPRECATED tsedsResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Core.FromJSON TaskStartedEventDetails where
  parseJSON =
    Core.withObject "TaskStartedEventDetails" Core.$
      \x ->
        TaskStartedEventDetails'
          Core.<$> (x Core..: "resourceType") Core.<*> (x Core..: "resource")
