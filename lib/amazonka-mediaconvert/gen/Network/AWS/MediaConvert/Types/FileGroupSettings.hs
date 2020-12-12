{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.FileGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.FileGroupSettings
  ( FileGroupSettings (..),

    -- * Smart constructor
    mkFileGroupSettings,

    -- * Lenses
    fgsDestination,
    fgsDestinationSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DestinationSettings
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to FILE_GROUP_SETTINGS.
--
-- /See:/ 'mkFileGroupSettings' smart constructor.
data FileGroupSettings = FileGroupSettings'
  { destination ::
      Lude.Maybe Lude.Text,
    destinationSettings :: Lude.Maybe DestinationSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileGroupSettings' with the minimum fields required to make a request.
--
-- * 'destination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
-- * 'destinationSettings' - Settings associated with the destination. Will vary based on the type of destination
mkFileGroupSettings ::
  FileGroupSettings
mkFileGroupSettings =
  FileGroupSettings'
    { destination = Lude.Nothing,
      destinationSettings = Lude.Nothing
    }

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgsDestination :: Lens.Lens' FileGroupSettings (Lude.Maybe Lude.Text)
fgsDestination = Lens.lens (destination :: FileGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {destination = a} :: FileGroupSettings)
{-# DEPRECATED fgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgsDestinationSettings :: Lens.Lens' FileGroupSettings (Lude.Maybe DestinationSettings)
fgsDestinationSettings = Lens.lens (destinationSettings :: FileGroupSettings -> Lude.Maybe DestinationSettings) (\s a -> s {destinationSettings = a} :: FileGroupSettings)
{-# DEPRECATED fgsDestinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead." #-}

instance Lude.FromJSON FileGroupSettings where
  parseJSON =
    Lude.withObject
      "FileGroupSettings"
      ( \x ->
          FileGroupSettings'
            Lude.<$> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "destinationSettings")
      )

instance Lude.ToJSON FileGroupSettings where
  toJSON FileGroupSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("destination" Lude..=) Lude.<$> destination,
            ("destinationSettings" Lude..=) Lude.<$> destinationSettings
          ]
      )
