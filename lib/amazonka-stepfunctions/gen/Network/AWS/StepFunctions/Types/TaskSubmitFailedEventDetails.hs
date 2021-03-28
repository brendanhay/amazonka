{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails
  ( TaskSubmitFailedEventDetails (..)
  -- * Smart constructor
  , mkTaskSubmitFailedEventDetails
  -- * Lenses
  , tsfedResourceType
  , tsfedResource
  , tsfedCause
  , tsfedError
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.Cause as Types
import qualified Network.AWS.StepFunctions.Types.Error as Types
import qualified Network.AWS.StepFunctions.Types.Resource as Types
import qualified Network.AWS.StepFunctions.Types.ResourceType as Types

-- | Contains details about a task that failed to submit during an execution.
--
-- /See:/ 'mkTaskSubmitFailedEventDetails' smart constructor.
data TaskSubmitFailedEventDetails = TaskSubmitFailedEventDetails'
  { resourceType :: Types.ResourceType
    -- ^ The action of the resource called by a task state.
  , resource :: Types.Resource
    -- ^ The service name of the resource in a task state.
  , cause :: Core.Maybe Types.Cause
    -- ^ A more detailed explanation of the cause of the failure.
  , error :: Core.Maybe Types.Error
    -- ^ The error code of the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskSubmitFailedEventDetails' value with any optional fields omitted.
mkTaskSubmitFailedEventDetails
    :: Types.ResourceType -- ^ 'resourceType'
    -> Types.Resource -- ^ 'resource'
    -> TaskSubmitFailedEventDetails
mkTaskSubmitFailedEventDetails resourceType resource
  = TaskSubmitFailedEventDetails'{resourceType, resource,
                                  cause = Core.Nothing, error = Core.Nothing}

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfedResourceType :: Lens.Lens' TaskSubmitFailedEventDetails Types.ResourceType
tsfedResourceType = Lens.field @"resourceType"
{-# INLINEABLE tsfedResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfedResource :: Lens.Lens' TaskSubmitFailedEventDetails Types.Resource
tsfedResource = Lens.field @"resource"
{-# INLINEABLE tsfedResource #-}
{-# DEPRECATED resource "Use generic-lens or generic-optics with 'resource' instead"  #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfedCause :: Lens.Lens' TaskSubmitFailedEventDetails (Core.Maybe Types.Cause)
tsfedCause = Lens.field @"cause"
{-# INLINEABLE tsfedCause #-}
{-# DEPRECATED cause "Use generic-lens or generic-optics with 'cause' instead"  #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfedError :: Lens.Lens' TaskSubmitFailedEventDetails (Core.Maybe Types.Error)
tsfedError = Lens.field @"error"
{-# INLINEABLE tsfedError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

instance Core.FromJSON TaskSubmitFailedEventDetails where
        parseJSON
          = Core.withObject "TaskSubmitFailedEventDetails" Core.$
              \ x ->
                TaskSubmitFailedEventDetails' Core.<$>
                  (x Core..: "resourceType") Core.<*> x Core..: "resource" Core.<*>
                    x Core..:? "cause"
                    Core.<*> x Core..:? "error"
