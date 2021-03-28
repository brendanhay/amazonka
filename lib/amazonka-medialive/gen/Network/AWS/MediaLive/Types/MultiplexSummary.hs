{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MultiplexSummary
  ( MultiplexSummary (..)
  -- * Smart constructor
  , mkMultiplexSummary
  -- * Lenses
  , msArn
  , msAvailabilityZones
  , msId
  , msMultiplexSettings
  , msName
  , msPipelinesRunningCount
  , msProgramCount
  , msState
  , msTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.MultiplexSettingsSummary as Types
import qualified Network.AWS.MediaLive.Types.MultiplexState as Types
import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for MultiplexSummary
--
-- /See:/ 'mkMultiplexSummary' smart constructor.
data MultiplexSummary = MultiplexSummary'
  { arn :: Core.Maybe Core.Text
    -- ^ The unique arn of the multiplex.
  , availabilityZones :: Core.Maybe [Core.Text]
    -- ^ A list of availability zones for the multiplex.
  , id :: Core.Maybe Core.Text
    -- ^ The unique id of the multiplex.
  , multiplexSettings :: Core.Maybe Types.MultiplexSettingsSummary
    -- ^ Configuration for a multiplex event.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the multiplex.
  , pipelinesRunningCount :: Core.Maybe Core.Int
    -- ^ The number of currently healthy pipelines.
  , programCount :: Core.Maybe Core.Int
    -- ^ The number of programs in the multiplex.
  , state :: Core.Maybe Types.MultiplexState
    -- ^ The current state of the multiplex.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A collection of key-value pairs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexSummary' value with any optional fields omitted.
mkMultiplexSummary
    :: MultiplexSummary
mkMultiplexSummary
  = MultiplexSummary'{arn = Core.Nothing,
                      availabilityZones = Core.Nothing, id = Core.Nothing,
                      multiplexSettings = Core.Nothing, name = Core.Nothing,
                      pipelinesRunningCount = Core.Nothing, programCount = Core.Nothing,
                      state = Core.Nothing, tags = Core.Nothing}

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msArn :: Lens.Lens' MultiplexSummary (Core.Maybe Core.Text)
msArn = Lens.field @"arn"
{-# INLINEABLE msArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAvailabilityZones :: Lens.Lens' MultiplexSummary (Core.Maybe [Core.Text])
msAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE msAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msId :: Lens.Lens' MultiplexSummary (Core.Maybe Core.Text)
msId = Lens.field @"id"
{-# INLINEABLE msId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMultiplexSettings :: Lens.Lens' MultiplexSummary (Core.Maybe Types.MultiplexSettingsSummary)
msMultiplexSettings = Lens.field @"multiplexSettings"
{-# INLINEABLE msMultiplexSettings #-}
{-# DEPRECATED multiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead"  #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msName :: Lens.Lens' MultiplexSummary (Core.Maybe Core.Text)
msName = Lens.field @"name"
{-# INLINEABLE msName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPipelinesRunningCount :: Lens.Lens' MultiplexSummary (Core.Maybe Core.Int)
msPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# INLINEABLE msPipelinesRunningCount #-}
{-# DEPRECATED pipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead"  #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msProgramCount :: Lens.Lens' MultiplexSummary (Core.Maybe Core.Int)
msProgramCount = Lens.field @"programCount"
{-# INLINEABLE msProgramCount #-}
{-# DEPRECATED programCount "Use generic-lens or generic-optics with 'programCount' instead"  #-}

-- | The current state of the multiplex.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msState :: Lens.Lens' MultiplexSummary (Core.Maybe Types.MultiplexState)
msState = Lens.field @"state"
{-# INLINEABLE msState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTags :: Lens.Lens' MultiplexSummary (Core.Maybe (Core.HashMap Core.Text Core.Text))
msTags = Lens.field @"tags"
{-# INLINEABLE msTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON MultiplexSummary where
        parseJSON
          = Core.withObject "MultiplexSummary" Core.$
              \ x ->
                MultiplexSummary' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "availabilityZones" Core.<*>
                    x Core..:? "id"
                    Core.<*> x Core..:? "multiplexSettings"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "pipelinesRunningCount"
                    Core.<*> x Core..:? "programCount"
                    Core.<*> x Core..:? "state"
                    Core.<*> x Core..:? "tags"
