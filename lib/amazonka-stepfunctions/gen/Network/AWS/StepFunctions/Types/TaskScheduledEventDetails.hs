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
    tsedfHeartbeatInSeconds,
    tsedfResourceType,
    tsedfTimeoutInSeconds,
    tsedfResource,
    tsedfParameters,
    tsedfRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about a task scheduled during an execution.
--
-- /See:/ 'mkTaskScheduledEventDetails' smart constructor.
data TaskScheduledEventDetails = TaskScheduledEventDetails'
  { -- | The maximum allowed duration between two heartbeats for the task.
    heartbeatInSeconds :: Lude.Maybe Lude.Integer,
    -- | The action of the resource called by a task state.
    resourceType :: Lude.Text,
    -- | The maximum allowed duration of the task.
    timeoutInSeconds :: Lude.Maybe Lude.Integer,
    -- | The service name of the resource in a task state.
    resource :: Lude.Text,
    -- | The JSON data passed to the resource referenced in a task state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    parameters :: Lude.Sensitive Lude.Text,
    -- | The region of the scheduled task
    region :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskScheduledEventDetails' with the minimum fields required to make a request.
--
-- * 'heartbeatInSeconds' - The maximum allowed duration between two heartbeats for the task.
-- * 'resourceType' - The action of the resource called by a task state.
-- * 'timeoutInSeconds' - The maximum allowed duration of the task.
-- * 'resource' - The service name of the resource in a task state.
-- * 'parameters' - The JSON data passed to the resource referenced in a task state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'region' - The region of the scheduled task
mkTaskScheduledEventDetails ::
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resource'
  Lude.Text ->
  -- | 'parameters'
  Lude.Sensitive Lude.Text ->
  -- | 'region'
  Lude.Text ->
  TaskScheduledEventDetails
mkTaskScheduledEventDetails
  pResourceType_
  pResource_
  pParameters_
  pRegion_ =
    TaskScheduledEventDetails'
      { heartbeatInSeconds = Lude.Nothing,
        resourceType = pResourceType_,
        timeoutInSeconds = Lude.Nothing,
        resource = pResource_,
        parameters = pParameters_,
        region = pRegion_
      }

-- | The maximum allowed duration between two heartbeats for the task.
--
-- /Note:/ Consider using 'heartbeatInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedfHeartbeatInSeconds :: Lens.Lens' TaskScheduledEventDetails (Lude.Maybe Lude.Integer)
tsedfHeartbeatInSeconds = Lens.lens (heartbeatInSeconds :: TaskScheduledEventDetails -> Lude.Maybe Lude.Integer) (\s a -> s {heartbeatInSeconds = a} :: TaskScheduledEventDetails)
{-# DEPRECATED tsedfHeartbeatInSeconds "Use generic-lens or generic-optics with 'heartbeatInSeconds' instead." #-}

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedfResourceType :: Lens.Lens' TaskScheduledEventDetails Lude.Text
tsedfResourceType = Lens.lens (resourceType :: TaskScheduledEventDetails -> Lude.Text) (\s a -> s {resourceType = a} :: TaskScheduledEventDetails)
{-# DEPRECATED tsedfResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The maximum allowed duration of the task.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedfTimeoutInSeconds :: Lens.Lens' TaskScheduledEventDetails (Lude.Maybe Lude.Integer)
tsedfTimeoutInSeconds = Lens.lens (timeoutInSeconds :: TaskScheduledEventDetails -> Lude.Maybe Lude.Integer) (\s a -> s {timeoutInSeconds = a} :: TaskScheduledEventDetails)
{-# DEPRECATED tsedfTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedfResource :: Lens.Lens' TaskScheduledEventDetails Lude.Text
tsedfResource = Lens.lens (resource :: TaskScheduledEventDetails -> Lude.Text) (\s a -> s {resource = a} :: TaskScheduledEventDetails)
{-# DEPRECATED tsedfResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | The JSON data passed to the resource referenced in a task state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedfParameters :: Lens.Lens' TaskScheduledEventDetails (Lude.Sensitive Lude.Text)
tsedfParameters = Lens.lens (parameters :: TaskScheduledEventDetails -> Lude.Sensitive Lude.Text) (\s a -> s {parameters = a} :: TaskScheduledEventDetails)
{-# DEPRECATED tsedfParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The region of the scheduled task
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedfRegion :: Lens.Lens' TaskScheduledEventDetails Lude.Text
tsedfRegion = Lens.lens (region :: TaskScheduledEventDetails -> Lude.Text) (\s a -> s {region = a} :: TaskScheduledEventDetails)
{-# DEPRECATED tsedfRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromJSON TaskScheduledEventDetails where
  parseJSON =
    Lude.withObject
      "TaskScheduledEventDetails"
      ( \x ->
          TaskScheduledEventDetails'
            Lude.<$> (x Lude..:? "heartbeatInSeconds")
            Lude.<*> (x Lude..: "resourceType")
            Lude.<*> (x Lude..:? "timeoutInSeconds")
            Lude.<*> (x Lude..: "resource")
            Lude.<*> (x Lude..: "parameters")
            Lude.<*> (x Lude..: "region")
      )
