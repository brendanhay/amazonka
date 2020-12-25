{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails
  ( TaskTimedOutEventDetails (..),

    -- * Smart constructor
    mkTaskTimedOutEventDetails,

    -- * Lenses
    ttoedResourceType,
    ttoedResource,
    ttoedCause,
    ttoedError,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.Resource as Types
import qualified Network.AWS.StepFunctions.Types.ResourceType as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveCause as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveError as Types

-- | Contains details about a resource timeout that occurred during an execution.
--
-- /See:/ 'mkTaskTimedOutEventDetails' smart constructor.
data TaskTimedOutEventDetails = TaskTimedOutEventDetails'
  { -- | The action of the resource called by a task state.
    resourceType :: Types.ResourceType,
    -- | The service name of the resource in a task state.
    resource :: Types.Resource,
    -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe Types.SensitiveCause,
    -- | The error code of the failure.
    error :: Core.Maybe Types.SensitiveError
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskTimedOutEventDetails' value with any optional fields omitted.
mkTaskTimedOutEventDetails ::
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'resource'
  Types.Resource ->
  TaskTimedOutEventDetails
mkTaskTimedOutEventDetails resourceType resource =
  TaskTimedOutEventDetails'
    { resourceType,
      resource,
      cause = Core.Nothing,
      error = Core.Nothing
    }

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttoedResourceType :: Lens.Lens' TaskTimedOutEventDetails Types.ResourceType
ttoedResourceType = Lens.field @"resourceType"
{-# DEPRECATED ttoedResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttoedResource :: Lens.Lens' TaskTimedOutEventDetails Types.Resource
ttoedResource = Lens.field @"resource"
{-# DEPRECATED ttoedResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttoedCause :: Lens.Lens' TaskTimedOutEventDetails (Core.Maybe Types.SensitiveCause)
ttoedCause = Lens.field @"cause"
{-# DEPRECATED ttoedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttoedError :: Lens.Lens' TaskTimedOutEventDetails (Core.Maybe Types.SensitiveError)
ttoedError = Lens.field @"error"
{-# DEPRECATED ttoedError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Core.FromJSON TaskTimedOutEventDetails where
  parseJSON =
    Core.withObject "TaskTimedOutEventDetails" Core.$
      \x ->
        TaskTimedOutEventDetails'
          Core.<$> (x Core..: "resourceType")
          Core.<*> (x Core..: "resource")
          Core.<*> (x Core..:? "cause")
          Core.<*> (x Core..:? "error")
