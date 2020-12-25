{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Multiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Multiplex
  ( Multiplex (..),

    -- * Smart constructor
    mkMultiplex,

    -- * Lenses
    mArn,
    mAvailabilityZones,
    mDestinations,
    mId,
    mMultiplexSettings,
    mName,
    mPipelinesRunningCount,
    mProgramCount,
    mState,
    mTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.MultiplexOutputDestination as Types
import qualified Network.AWS.MediaLive.Types.MultiplexSettings as Types
import qualified Network.AWS.MediaLive.Types.MultiplexState as Types
import qualified Network.AWS.Prelude as Core

-- | The multiplex object.
--
-- /See:/ 'mkMultiplex' smart constructor.
data Multiplex = Multiplex'
  { -- | The unique arn of the multiplex.
    arn :: Core.Maybe Core.Text,
    -- | A list of availability zones for the multiplex.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | A list of the multiplex output destinations.
    destinations :: Core.Maybe [Types.MultiplexOutputDestination],
    -- | The unique id of the multiplex.
    id :: Core.Maybe Core.Text,
    -- | Configuration for a multiplex event.
    multiplexSettings :: Core.Maybe Types.MultiplexSettings,
    -- | The name of the multiplex.
    name :: Core.Maybe Core.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Core.Maybe Core.Int,
    -- | The number of programs in the multiplex.
    programCount :: Core.Maybe Core.Int,
    -- | The current state of the multiplex.
    state :: Core.Maybe Types.MultiplexState,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Multiplex' value with any optional fields omitted.
mkMultiplex ::
  Multiplex
mkMultiplex =
  Multiplex'
    { arn = Core.Nothing,
      availabilityZones = Core.Nothing,
      destinations = Core.Nothing,
      id = Core.Nothing,
      multiplexSettings = Core.Nothing,
      name = Core.Nothing,
      pipelinesRunningCount = Core.Nothing,
      programCount = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing
    }

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mArn :: Lens.Lens' Multiplex (Core.Maybe Core.Text)
mArn = Lens.field @"arn"
{-# DEPRECATED mArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAvailabilityZones :: Lens.Lens' Multiplex (Core.Maybe [Core.Text])
mAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED mAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDestinations :: Lens.Lens' Multiplex (Core.Maybe [Types.MultiplexOutputDestination])
mDestinations = Lens.field @"destinations"
{-# DEPRECATED mDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mId :: Lens.Lens' Multiplex (Core.Maybe Core.Text)
mId = Lens.field @"id"
{-# DEPRECATED mId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMultiplexSettings :: Lens.Lens' Multiplex (Core.Maybe Types.MultiplexSettings)
mMultiplexSettings = Lens.field @"multiplexSettings"
{-# DEPRECATED mMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mName :: Lens.Lens' Multiplex (Core.Maybe Core.Text)
mName = Lens.field @"name"
{-# DEPRECATED mName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPipelinesRunningCount :: Lens.Lens' Multiplex (Core.Maybe Core.Int)
mPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# DEPRECATED mPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mProgramCount :: Lens.Lens' Multiplex (Core.Maybe Core.Int)
mProgramCount = Lens.field @"programCount"
{-# DEPRECATED mProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | The current state of the multiplex.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mState :: Lens.Lens' Multiplex (Core.Maybe Types.MultiplexState)
mState = Lens.field @"state"
{-# DEPRECATED mState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTags :: Lens.Lens' Multiplex (Core.Maybe (Core.HashMap Core.Text Core.Text))
mTags = Lens.field @"tags"
{-# DEPRECATED mTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON Multiplex where
  parseJSON =
    Core.withObject "Multiplex" Core.$
      \x ->
        Multiplex'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "availabilityZones")
          Core.<*> (x Core..:? "destinations")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "multiplexSettings")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "pipelinesRunningCount")
          Core.<*> (x Core..:? "programCount")
          Core.<*> (x Core..:? "state")
          Core.<*> (x Core..:? "tags")
