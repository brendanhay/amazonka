-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputDestination
  ( OutputDestination (..),

    -- * Smart constructor
    mkOutputDestination,

    -- * Lenses
    odSettings,
    odMediaPackageSettings,
    odId,
    odMultiplexSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings
import Network.AWS.MediaLive.Types.MultiplexProgramChannelDestinationSettings
import Network.AWS.MediaLive.Types.OutputDestinationSettings
import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for OutputDestination
--
-- /See:/ 'mkOutputDestination' smart constructor.
data OutputDestination = OutputDestination'
  { settings ::
      Lude.Maybe [OutputDestinationSettings],
    mediaPackageSettings ::
      Lude.Maybe [MediaPackageOutputDestinationSettings],
    id :: Lude.Maybe Lude.Text,
    multiplexSettings ::
      Lude.Maybe MultiplexProgramChannelDestinationSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputDestination' with the minimum fields required to make a request.
--
-- * 'id' - User-specified id. This is used in an output group or an output.
-- * 'mediaPackageSettings' - Destination settings for a MediaPackage output; one destination for both encoders.
-- * 'multiplexSettings' - Destination settings for a Multiplex output; one destination for both encoders.
-- * 'settings' - Destination settings for a standard output; one destination for each redundant encoder.
mkOutputDestination ::
  OutputDestination
mkOutputDestination =
  OutputDestination'
    { settings = Lude.Nothing,
      mediaPackageSettings = Lude.Nothing,
      id = Lude.Nothing,
      multiplexSettings = Lude.Nothing
    }

-- | Destination settings for a standard output; one destination for each redundant encoder.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odSettings :: Lens.Lens' OutputDestination (Lude.Maybe [OutputDestinationSettings])
odSettings = Lens.lens (settings :: OutputDestination -> Lude.Maybe [OutputDestinationSettings]) (\s a -> s {settings = a} :: OutputDestination)
{-# DEPRECATED odSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | Destination settings for a MediaPackage output; one destination for both encoders.
--
-- /Note:/ Consider using 'mediaPackageSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odMediaPackageSettings :: Lens.Lens' OutputDestination (Lude.Maybe [MediaPackageOutputDestinationSettings])
odMediaPackageSettings = Lens.lens (mediaPackageSettings :: OutputDestination -> Lude.Maybe [MediaPackageOutputDestinationSettings]) (\s a -> s {mediaPackageSettings = a} :: OutputDestination)
{-# DEPRECATED odMediaPackageSettings "Use generic-lens or generic-optics with 'mediaPackageSettings' instead." #-}

-- | User-specified id. This is used in an output group or an output.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odId :: Lens.Lens' OutputDestination (Lude.Maybe Lude.Text)
odId = Lens.lens (id :: OutputDestination -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: OutputDestination)
{-# DEPRECATED odId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Destination settings for a Multiplex output; one destination for both encoders.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odMultiplexSettings :: Lens.Lens' OutputDestination (Lude.Maybe MultiplexProgramChannelDestinationSettings)
odMultiplexSettings = Lens.lens (multiplexSettings :: OutputDestination -> Lude.Maybe MultiplexProgramChannelDestinationSettings) (\s a -> s {multiplexSettings = a} :: OutputDestination)
{-# DEPRECATED odMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

instance Lude.FromJSON OutputDestination where
  parseJSON =
    Lude.withObject
      "OutputDestination"
      ( \x ->
          OutputDestination'
            Lude.<$> (x Lude..:? "settings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "mediaPackageSettings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "multiplexSettings")
      )

instance Lude.ToJSON OutputDestination where
  toJSON OutputDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("settings" Lude..=) Lude.<$> settings,
            ("mediaPackageSettings" Lude..=) Lude.<$> mediaPackageSettings,
            ("id" Lude..=) Lude.<$> id,
            ("multiplexSettings" Lude..=) Lude.<$> multiplexSettings
          ]
      )
