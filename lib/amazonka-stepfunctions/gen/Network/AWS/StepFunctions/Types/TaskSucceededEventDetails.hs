{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskSucceededEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.TaskSucceededEventDetails
  ( TaskSucceededEventDetails (..)
  -- * Smart constructor
  , mkTaskSucceededEventDetails
  -- * Lenses
  , tsedgResourceType
  , tsedgResource
  , tsedgOutput
  , tsedgOutputDetails
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails as Types
import qualified Network.AWS.StepFunctions.Types.Resource as Types
import qualified Network.AWS.StepFunctions.Types.ResourceType as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveData as Types

-- | Contains details about the successful completion of a task state.
--
-- /See:/ 'mkTaskSucceededEventDetails' smart constructor.
data TaskSucceededEventDetails = TaskSucceededEventDetails'
  { resourceType :: Types.ResourceType
    -- ^ The action of the resource called by a task state.
  , resource :: Types.Resource
    -- ^ The service name of the resource in a task state.
  , output :: Core.Maybe Types.SensitiveData
    -- ^ The full JSON response from a resource when a task has succeeded. This response becomes the output of the related task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
  , outputDetails :: Core.Maybe Types.HistoryEventExecutionDataDetails
    -- ^ Contains details about the output of an execution history event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskSucceededEventDetails' value with any optional fields omitted.
mkTaskSucceededEventDetails
    :: Types.ResourceType -- ^ 'resourceType'
    -> Types.Resource -- ^ 'resource'
    -> TaskSucceededEventDetails
mkTaskSucceededEventDetails resourceType resource
  = TaskSucceededEventDetails'{resourceType, resource,
                               output = Core.Nothing, outputDetails = Core.Nothing}

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedgResourceType :: Lens.Lens' TaskSucceededEventDetails Types.ResourceType
tsedgResourceType = Lens.field @"resourceType"
{-# INLINEABLE tsedgResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedgResource :: Lens.Lens' TaskSucceededEventDetails Types.Resource
tsedgResource = Lens.field @"resource"
{-# INLINEABLE tsedgResource #-}
{-# DEPRECATED resource "Use generic-lens or generic-optics with 'resource' instead"  #-}

-- | The full JSON response from a resource when a task has succeeded. This response becomes the output of the related task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedgOutput :: Lens.Lens' TaskSucceededEventDetails (Core.Maybe Types.SensitiveData)
tsedgOutput = Lens.field @"output"
{-# INLINEABLE tsedgOutput #-}
{-# DEPRECATED output "Use generic-lens or generic-optics with 'output' instead"  #-}

-- | Contains details about the output of an execution history event.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedgOutputDetails :: Lens.Lens' TaskSucceededEventDetails (Core.Maybe Types.HistoryEventExecutionDataDetails)
tsedgOutputDetails = Lens.field @"outputDetails"
{-# INLINEABLE tsedgOutputDetails #-}
{-# DEPRECATED outputDetails "Use generic-lens or generic-optics with 'outputDetails' instead"  #-}

instance Core.FromJSON TaskSucceededEventDetails where
        parseJSON
          = Core.withObject "TaskSucceededEventDetails" Core.$
              \ x ->
                TaskSucceededEventDetails' Core.<$>
                  (x Core..: "resourceType") Core.<*> x Core..: "resource" Core.<*>
                    x Core..:? "output"
                    Core.<*> x Core..:? "outputDetails"
