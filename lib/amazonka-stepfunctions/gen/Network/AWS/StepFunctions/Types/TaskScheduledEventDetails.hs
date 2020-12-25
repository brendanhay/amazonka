{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskScheduledEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskScheduledEventDetails
  ( TaskScheduledEventDetails (..),

    -- * Smart constructor
    mkTaskScheduledEventDetails,

    -- * Lenses
    tsedResourceType,
    tsedResource,
    tsedRegion,
    tsedParameters,
    tsedHeartbeatInSeconds,
    tsedTimeoutInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.Name as Types
import qualified Network.AWS.StepFunctions.Types.Parameters as Types

-- | Contains details about a task scheduled during an execution.
--
-- /See:/ 'mkTaskScheduledEventDetails' smart constructor.
data TaskScheduledEventDetails = TaskScheduledEventDetails'
  { -- | The action of the resource called by a task state.
    resourceType :: Types.Name,
    -- | The service name of the resource in a task state.
    resource :: Types.Name,
    -- | The region of the scheduled task
    region :: Types.Name,
    -- | The JSON data passed to the resource referenced in a task state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    parameters :: Types.Parameters,
    -- | The maximum allowed duration between two heartbeats for the task.
    heartbeatInSeconds :: Core.Maybe Core.Integer,
    -- | The maximum allowed duration of the task.
    timeoutInSeconds :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskScheduledEventDetails' value with any optional fields omitted.
mkTaskScheduledEventDetails ::
  -- | 'resourceType'
  Types.Name ->
  -- | 'resource'
  Types.Name ->
  -- | 'region'
  Types.Name ->
  -- | 'parameters'
  Types.Parameters ->
  TaskScheduledEventDetails
mkTaskScheduledEventDetails resourceType resource region parameters =
  TaskScheduledEventDetails'
    { resourceType,
      resource,
      region,
      parameters,
      heartbeatInSeconds = Core.Nothing,
      timeoutInSeconds = Core.Nothing
    }

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedResourceType :: Lens.Lens' TaskScheduledEventDetails Types.Name
tsedResourceType = Lens.field @"resourceType"
{-# DEPRECATED tsedResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedResource :: Lens.Lens' TaskScheduledEventDetails Types.Name
tsedResource = Lens.field @"resource"
{-# DEPRECATED tsedResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | The region of the scheduled task
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedRegion :: Lens.Lens' TaskScheduledEventDetails Types.Name
tsedRegion = Lens.field @"region"
{-# DEPRECATED tsedRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The JSON data passed to the resource referenced in a task state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedParameters :: Lens.Lens' TaskScheduledEventDetails Types.Parameters
tsedParameters = Lens.field @"parameters"
{-# DEPRECATED tsedParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The maximum allowed duration between two heartbeats for the task.
--
-- /Note:/ Consider using 'heartbeatInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedHeartbeatInSeconds :: Lens.Lens' TaskScheduledEventDetails (Core.Maybe Core.Integer)
tsedHeartbeatInSeconds = Lens.field @"heartbeatInSeconds"
{-# DEPRECATED tsedHeartbeatInSeconds "Use generic-lens or generic-optics with 'heartbeatInSeconds' instead." #-}

-- | The maximum allowed duration of the task.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedTimeoutInSeconds :: Lens.Lens' TaskScheduledEventDetails (Core.Maybe Core.Integer)
tsedTimeoutInSeconds = Lens.field @"timeoutInSeconds"
{-# DEPRECATED tsedTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

instance Core.FromJSON TaskScheduledEventDetails where
  parseJSON =
    Core.withObject "TaskScheduledEventDetails" Core.$
      \x ->
        TaskScheduledEventDetails'
          Core.<$> (x Core..: "resourceType")
          Core.<*> (x Core..: "resource")
          Core.<*> (x Core..: "region")
          Core.<*> (x Core..: "parameters")
          Core.<*> (x Core..:? "heartbeatInSeconds")
          Core.<*> (x Core..:? "timeoutInSeconds")
