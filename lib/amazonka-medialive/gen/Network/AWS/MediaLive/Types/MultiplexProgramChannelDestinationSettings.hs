{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramChannelDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramChannelDestinationSettings
  ( MultiplexProgramChannelDestinationSettings (..),

    -- * Smart constructor
    mkMultiplexProgramChannelDestinationSettings,

    -- * Lenses
    mpcdsMultiplexId,
    mpcdsProgramName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Multiplex Program Input Destination Settings for outputting a Channel to a Multiplex
--
-- /See:/ 'mkMultiplexProgramChannelDestinationSettings' smart constructor.
data MultiplexProgramChannelDestinationSettings = MultiplexProgramChannelDestinationSettings'
  { -- | The ID of the Multiplex that the encoder is providing output to. You do not need to specify the individual inputs to the Multiplex; MediaLive will handle the connection of the two MediaLive pipelines to the two Multiplex instances.
    --
    -- The Multiplex must be in the same region as the Channel.
    multiplexId :: Core.Maybe Core.Text,
    -- | The program name of the Multiplex program that the encoder is providing output to.
    programName :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexProgramChannelDestinationSettings' value with any optional fields omitted.
mkMultiplexProgramChannelDestinationSettings ::
  MultiplexProgramChannelDestinationSettings
mkMultiplexProgramChannelDestinationSettings =
  MultiplexProgramChannelDestinationSettings'
    { multiplexId =
        Core.Nothing,
      programName = Core.Nothing
    }

-- | The ID of the Multiplex that the encoder is providing output to. You do not need to specify the individual inputs to the Multiplex; MediaLive will handle the connection of the two MediaLive pipelines to the two Multiplex instances.
--
-- The Multiplex must be in the same region as the Channel.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdsMultiplexId :: Lens.Lens' MultiplexProgramChannelDestinationSettings (Core.Maybe Core.Text)
mpcdsMultiplexId = Lens.field @"multiplexId"
{-# DEPRECATED mpcdsMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

-- | The program name of the Multiplex program that the encoder is providing output to.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdsProgramName :: Lens.Lens' MultiplexProgramChannelDestinationSettings (Core.Maybe Core.Text)
mpcdsProgramName = Lens.field @"programName"
{-# DEPRECATED mpcdsProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

instance Core.FromJSON MultiplexProgramChannelDestinationSettings where
  toJSON MultiplexProgramChannelDestinationSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("multiplexId" Core..=) Core.<$> multiplexId,
            ("programName" Core..=) Core.<$> programName
          ]
      )

instance Core.FromJSON MultiplexProgramChannelDestinationSettings where
  parseJSON =
    Core.withObject "MultiplexProgramChannelDestinationSettings" Core.$
      \x ->
        MultiplexProgramChannelDestinationSettings'
          Core.<$> (x Core..:? "multiplexId") Core.<*> (x Core..:? "programName")
