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
    tasHeartbeatInSeconds,
    tasTimeoutInSeconds,
    tasResourceType,
    tasResource,
    tasRegion,
    tasParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about a task scheduled during an execution.
--
-- /See:/ 'mkTaskScheduledEventDetails' smart constructor.
data TaskScheduledEventDetails = TaskScheduledEventDetails'
  { heartbeatInSeconds ::
      Lude.Maybe Lude.Integer,
    timeoutInSeconds ::
      Lude.Maybe Lude.Integer,
    resourceType :: Lude.Text,
    resource :: Lude.Text,
    region :: Lude.Text,
    parameters :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskScheduledEventDetails' with the minimum fields required to make a request.
--
-- * 'heartbeatInSeconds' - The maximum allowed duration between two heartbeats for the task.
-- * 'parameters' - The JSON data passed to the resource referenced in a task state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'region' - The region of the scheduled task
-- * 'resource' - The service name of the resource in a task state.
-- * 'resourceType' - The action of the resource called by a task state.
-- * 'timeoutInSeconds' - The maximum allowed duration of the task.
mkTaskScheduledEventDetails ::
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resource'
  Lude.Text ->
  -- | 'region'
  Lude.Text ->
  -- | 'parameters'
  Lude.Sensitive Lude.Text ->
  TaskScheduledEventDetails
mkTaskScheduledEventDetails
  pResourceType_
  pResource_
  pRegion_
  pParameters_ =
    TaskScheduledEventDetails'
      { heartbeatInSeconds = Lude.Nothing,
        timeoutInSeconds = Lude.Nothing,
        resourceType = pResourceType_,
        resource = pResource_,
        region = pRegion_,
        parameters = pParameters_
      }

-- | The maximum allowed duration between two heartbeats for the task.
--
-- /Note:/ Consider using 'heartbeatInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tasHeartbeatInSeconds :: Lens.Lens' TaskScheduledEventDetails (Lude.Maybe Lude.Integer)
tasHeartbeatInSeconds = Lens.lens (heartbeatInSeconds :: TaskScheduledEventDetails -> Lude.Maybe Lude.Integer) (\s a -> s {heartbeatInSeconds = a} :: TaskScheduledEventDetails)
{-# DEPRECATED tasHeartbeatInSeconds "Use generic-lens or generic-optics with 'heartbeatInSeconds' instead." #-}

-- | The maximum allowed duration of the task.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tasTimeoutInSeconds :: Lens.Lens' TaskScheduledEventDetails (Lude.Maybe Lude.Integer)
tasTimeoutInSeconds = Lens.lens (timeoutInSeconds :: TaskScheduledEventDetails -> Lude.Maybe Lude.Integer) (\s a -> s {timeoutInSeconds = a} :: TaskScheduledEventDetails)
{-# DEPRECATED tasTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tasResourceType :: Lens.Lens' TaskScheduledEventDetails Lude.Text
tasResourceType = Lens.lens (resourceType :: TaskScheduledEventDetails -> Lude.Text) (\s a -> s {resourceType = a} :: TaskScheduledEventDetails)
{-# DEPRECATED tasResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tasResource :: Lens.Lens' TaskScheduledEventDetails Lude.Text
tasResource = Lens.lens (resource :: TaskScheduledEventDetails -> Lude.Text) (\s a -> s {resource = a} :: TaskScheduledEventDetails)
{-# DEPRECATED tasResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | The region of the scheduled task
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tasRegion :: Lens.Lens' TaskScheduledEventDetails Lude.Text
tasRegion = Lens.lens (region :: TaskScheduledEventDetails -> Lude.Text) (\s a -> s {region = a} :: TaskScheduledEventDetails)
{-# DEPRECATED tasRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The JSON data passed to the resource referenced in a task state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tasParameters :: Lens.Lens' TaskScheduledEventDetails (Lude.Sensitive Lude.Text)
tasParameters = Lens.lens (parameters :: TaskScheduledEventDetails -> Lude.Sensitive Lude.Text) (\s a -> s {parameters = a} :: TaskScheduledEventDetails)
{-# DEPRECATED tasParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Lude.FromJSON TaskScheduledEventDetails where
  parseJSON =
    Lude.withObject
      "TaskScheduledEventDetails"
      ( \x ->
          TaskScheduledEventDetails'
            Lude.<$> (x Lude..:? "heartbeatInSeconds")
            Lude.<*> (x Lude..:? "timeoutInSeconds")
            Lude.<*> (x Lude..: "resourceType")
            Lude.<*> (x Lude..: "resource")
            Lude.<*> (x Lude..: "region")
            Lude.<*> (x Lude..: "parameters")
      )
