{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskFailedEventDetails
  ( TaskFailedEventDetails (..),

    -- * Smart constructor
    mkTaskFailedEventDetails,

    -- * Lenses
    tfedResourceType,
    tfedResource,
    tfedCause,
    tfedError,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.Name as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveCause as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveError as Types

-- | Contains details about a task failure event.
--
-- /See:/ 'mkTaskFailedEventDetails' smart constructor.
data TaskFailedEventDetails = TaskFailedEventDetails'
  { -- | The action of the resource called by a task state.
    resourceType :: Types.Name,
    -- | The service name of the resource in a task state.
    resource :: Types.Name,
    -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe Types.SensitiveCause,
    -- | The error code of the failure.
    error :: Core.Maybe Types.SensitiveError
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskFailedEventDetails' value with any optional fields omitted.
mkTaskFailedEventDetails ::
  -- | 'resourceType'
  Types.Name ->
  -- | 'resource'
  Types.Name ->
  TaskFailedEventDetails
mkTaskFailedEventDetails resourceType resource =
  TaskFailedEventDetails'
    { resourceType,
      resource,
      cause = Core.Nothing,
      error = Core.Nothing
    }

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfedResourceType :: Lens.Lens' TaskFailedEventDetails Types.Name
tfedResourceType = Lens.field @"resourceType"
{-# DEPRECATED tfedResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfedResource :: Lens.Lens' TaskFailedEventDetails Types.Name
tfedResource = Lens.field @"resource"
{-# DEPRECATED tfedResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfedCause :: Lens.Lens' TaskFailedEventDetails (Core.Maybe Types.SensitiveCause)
tfedCause = Lens.field @"cause"
{-# DEPRECATED tfedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfedError :: Lens.Lens' TaskFailedEventDetails (Core.Maybe Types.SensitiveError)
tfedError = Lens.field @"error"
{-# DEPRECATED tfedError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Core.FromJSON TaskFailedEventDetails where
  parseJSON =
    Core.withObject "TaskFailedEventDetails" Core.$
      \x ->
        TaskFailedEventDetails'
          Core.<$> (x Core..: "resourceType")
          Core.<*> (x Core..: "resource")
          Core.<*> (x Core..:? "cause")
          Core.<*> (x Core..:? "error")
