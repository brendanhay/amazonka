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
import qualified Network.AWS.Prelude as Lude

-- | Multiplex Program Input Destination Settings for outputting a Channel to a Multiplex
--
-- /See:/ 'mkMultiplexProgramChannelDestinationSettings' smart constructor.
data MultiplexProgramChannelDestinationSettings = MultiplexProgramChannelDestinationSettings'
  { multiplexId ::
      Lude.Maybe
        Lude.Text,
    programName ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexProgramChannelDestinationSettings' with the minimum fields required to make a request.
--
-- * 'multiplexId' - The ID of the Multiplex that the encoder is providing output to. You do not need to specify the individual inputs to the Multiplex; MediaLive will handle the connection of the two MediaLive pipelines to the two Multiplex instances.
--
-- The Multiplex must be in the same region as the Channel.
-- * 'programName' - The program name of the Multiplex program that the encoder is providing output to.
mkMultiplexProgramChannelDestinationSettings ::
  MultiplexProgramChannelDestinationSettings
mkMultiplexProgramChannelDestinationSettings =
  MultiplexProgramChannelDestinationSettings'
    { multiplexId =
        Lude.Nothing,
      programName = Lude.Nothing
    }

-- | The ID of the Multiplex that the encoder is providing output to. You do not need to specify the individual inputs to the Multiplex; MediaLive will handle the connection of the two MediaLive pipelines to the two Multiplex instances.
--
-- The Multiplex must be in the same region as the Channel.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdsMultiplexId :: Lens.Lens' MultiplexProgramChannelDestinationSettings (Lude.Maybe Lude.Text)
mpcdsMultiplexId = Lens.lens (multiplexId :: MultiplexProgramChannelDestinationSettings -> Lude.Maybe Lude.Text) (\s a -> s {multiplexId = a} :: MultiplexProgramChannelDestinationSettings)
{-# DEPRECATED mpcdsMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

-- | The program name of the Multiplex program that the encoder is providing output to.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpcdsProgramName :: Lens.Lens' MultiplexProgramChannelDestinationSettings (Lude.Maybe Lude.Text)
mpcdsProgramName = Lens.lens (programName :: MultiplexProgramChannelDestinationSettings -> Lude.Maybe Lude.Text) (\s a -> s {programName = a} :: MultiplexProgramChannelDestinationSettings)
{-# DEPRECATED mpcdsProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

instance Lude.FromJSON MultiplexProgramChannelDestinationSettings where
  parseJSON =
    Lude.withObject
      "MultiplexProgramChannelDestinationSettings"
      ( \x ->
          MultiplexProgramChannelDestinationSettings'
            Lude.<$> (x Lude..:? "multiplexId") Lude.<*> (x Lude..:? "programName")
      )

instance Lude.ToJSON MultiplexProgramChannelDestinationSettings where
  toJSON MultiplexProgramChannelDestinationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("multiplexId" Lude..=) Lude.<$> multiplexId,
            ("programName" Lude..=) Lude.<$> programName
          ]
      )
