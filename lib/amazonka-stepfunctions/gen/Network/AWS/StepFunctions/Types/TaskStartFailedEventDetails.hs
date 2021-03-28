{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskStartFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.TaskStartFailedEventDetails
  ( TaskStartFailedEventDetails (..)
  -- * Smart constructor
  , mkTaskStartFailedEventDetails
  -- * Lenses
  , tResourceType
  , tResource
  , tCause
  , tError
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.Resource as Types
import qualified Network.AWS.StepFunctions.Types.ResourceType as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveCause as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveError as Types

-- | Contains details about a task that failed to start during an execution.
--
-- /See:/ 'mkTaskStartFailedEventDetails' smart constructor.
data TaskStartFailedEventDetails = TaskStartFailedEventDetails'
  { resourceType :: Types.ResourceType
    -- ^ The action of the resource called by a task state.
  , resource :: Types.Resource
    -- ^ The service name of the resource in a task state.
  , cause :: Core.Maybe Types.SensitiveCause
    -- ^ A more detailed explanation of the cause of the failure.
  , error :: Core.Maybe Types.SensitiveError
    -- ^ The error code of the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskStartFailedEventDetails' value with any optional fields omitted.
mkTaskStartFailedEventDetails
    :: Types.ResourceType -- ^ 'resourceType'
    -> Types.Resource -- ^ 'resource'
    -> TaskStartFailedEventDetails
mkTaskStartFailedEventDetails resourceType resource
  = TaskStartFailedEventDetails'{resourceType, resource,
                                 cause = Core.Nothing, error = Core.Nothing}

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResourceType :: Lens.Lens' TaskStartFailedEventDetails Types.ResourceType
tResourceType = Lens.field @"resourceType"
{-# INLINEABLE tResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResource :: Lens.Lens' TaskStartFailedEventDetails Types.Resource
tResource = Lens.field @"resource"
{-# INLINEABLE tResource #-}
{-# DEPRECATED resource "Use generic-lens or generic-optics with 'resource' instead"  #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCause :: Lens.Lens' TaskStartFailedEventDetails (Core.Maybe Types.SensitiveCause)
tCause = Lens.field @"cause"
{-# INLINEABLE tCause #-}
{-# DEPRECATED cause "Use generic-lens or generic-optics with 'cause' instead"  #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tError :: Lens.Lens' TaskStartFailedEventDetails (Core.Maybe Types.SensitiveError)
tError = Lens.field @"error"
{-# INLINEABLE tError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

instance Core.FromJSON TaskStartFailedEventDetails where
        parseJSON
          = Core.withObject "TaskStartFailedEventDetails" Core.$
              \ x ->
                TaskStartFailedEventDetails' Core.<$>
                  (x Core..: "resourceType") Core.<*> x Core..: "resource" Core.<*>
                    x Core..:? "cause"
                    Core.<*> x Core..:? "error"
