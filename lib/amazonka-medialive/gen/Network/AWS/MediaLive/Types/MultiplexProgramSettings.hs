{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramSettings
  ( MultiplexProgramSettings (..),

    -- * Smart constructor
    mkMultiplexProgramSettings,

    -- * Lenses
    mpsPreferredChannelPipeline,
    mpsVideoSettings,
    mpsServiceDescriptor,
    mpsProgramNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MultiplexProgramServiceDescriptor
import Network.AWS.MediaLive.Types.MultiplexVideoSettings
import Network.AWS.MediaLive.Types.PreferredChannelPipeline
import qualified Network.AWS.Prelude as Lude

-- | Multiplex Program settings configuration.
--
-- /See:/ 'mkMultiplexProgramSettings' smart constructor.
data MultiplexProgramSettings = MultiplexProgramSettings'
  { preferredChannelPipeline ::
      Lude.Maybe PreferredChannelPipeline,
    videoSettings ::
      Lude.Maybe MultiplexVideoSettings,
    serviceDescriptor ::
      Lude.Maybe
        MultiplexProgramServiceDescriptor,
    programNumber :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexProgramSettings' with the minimum fields required to make a request.
--
-- * 'preferredChannelPipeline' - Indicates which pipeline is preferred by the multiplex for program ingest.
-- * 'programNumber' - Unique program number.
-- * 'serviceDescriptor' - Transport stream service descriptor configuration for the Multiplex program.
-- * 'videoSettings' - Program video settings configuration.
mkMultiplexProgramSettings ::
  -- | 'programNumber'
  Lude.Natural ->
  MultiplexProgramSettings
mkMultiplexProgramSettings pProgramNumber_ =
  MultiplexProgramSettings'
    { preferredChannelPipeline =
        Lude.Nothing,
      videoSettings = Lude.Nothing,
      serviceDescriptor = Lude.Nothing,
      programNumber = pProgramNumber_
    }

-- | Indicates which pipeline is preferred by the multiplex for program ingest.
--
-- /Note:/ Consider using 'preferredChannelPipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsPreferredChannelPipeline :: Lens.Lens' MultiplexProgramSettings (Lude.Maybe PreferredChannelPipeline)
mpsPreferredChannelPipeline = Lens.lens (preferredChannelPipeline :: MultiplexProgramSettings -> Lude.Maybe PreferredChannelPipeline) (\s a -> s {preferredChannelPipeline = a} :: MultiplexProgramSettings)
{-# DEPRECATED mpsPreferredChannelPipeline "Use generic-lens or generic-optics with 'preferredChannelPipeline' instead." #-}

-- | Program video settings configuration.
--
-- /Note:/ Consider using 'videoSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsVideoSettings :: Lens.Lens' MultiplexProgramSettings (Lude.Maybe MultiplexVideoSettings)
mpsVideoSettings = Lens.lens (videoSettings :: MultiplexProgramSettings -> Lude.Maybe MultiplexVideoSettings) (\s a -> s {videoSettings = a} :: MultiplexProgramSettings)
{-# DEPRECATED mpsVideoSettings "Use generic-lens or generic-optics with 'videoSettings' instead." #-}

-- | Transport stream service descriptor configuration for the Multiplex program.
--
-- /Note:/ Consider using 'serviceDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsServiceDescriptor :: Lens.Lens' MultiplexProgramSettings (Lude.Maybe MultiplexProgramServiceDescriptor)
mpsServiceDescriptor = Lens.lens (serviceDescriptor :: MultiplexProgramSettings -> Lude.Maybe MultiplexProgramServiceDescriptor) (\s a -> s {serviceDescriptor = a} :: MultiplexProgramSettings)
{-# DEPRECATED mpsServiceDescriptor "Use generic-lens or generic-optics with 'serviceDescriptor' instead." #-}

-- | Unique program number.
--
-- /Note:/ Consider using 'programNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsProgramNumber :: Lens.Lens' MultiplexProgramSettings Lude.Natural
mpsProgramNumber = Lens.lens (programNumber :: MultiplexProgramSettings -> Lude.Natural) (\s a -> s {programNumber = a} :: MultiplexProgramSettings)
{-# DEPRECATED mpsProgramNumber "Use generic-lens or generic-optics with 'programNumber' instead." #-}

instance Lude.FromJSON MultiplexProgramSettings where
  parseJSON =
    Lude.withObject
      "MultiplexProgramSettings"
      ( \x ->
          MultiplexProgramSettings'
            Lude.<$> (x Lude..:? "preferredChannelPipeline")
            Lude.<*> (x Lude..:? "videoSettings")
            Lude.<*> (x Lude..:? "serviceDescriptor")
            Lude.<*> (x Lude..: "programNumber")
      )

instance Lude.ToJSON MultiplexProgramSettings where
  toJSON MultiplexProgramSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("preferredChannelPipeline" Lude..=)
              Lude.<$> preferredChannelPipeline,
            ("videoSettings" Lude..=) Lude.<$> videoSettings,
            ("serviceDescriptor" Lude..=) Lude.<$> serviceDescriptor,
            Lude.Just ("programNumber" Lude..= programNumber)
          ]
      )
