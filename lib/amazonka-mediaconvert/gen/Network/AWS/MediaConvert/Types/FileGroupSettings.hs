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
import qualified Network.AWS.MediaConvert.Types.DestinationSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to FILE_GROUP_SETTINGS.
--
-- /See:/ 'mkFileGroupSettings' smart constructor.
data FileGroupSettings = FileGroupSettings'
  { -- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
    destination :: Core.Maybe Core.Text,
    -- | Settings associated with the destination. Will vary based on the type of destination
    destinationSettings :: Core.Maybe Types.DestinationSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FileGroupSettings' value with any optional fields omitted.
mkFileGroupSettings ::
  FileGroupSettings
mkFileGroupSettings =
  FileGroupSettings'
    { destination = Core.Nothing,
      destinationSettings = Core.Nothing
    }

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgsDestination :: Lens.Lens' FileGroupSettings (Core.Maybe Core.Text)
fgsDestination = Lens.field @"destination"
{-# DEPRECATED fgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgsDestinationSettings :: Lens.Lens' FileGroupSettings (Core.Maybe Types.DestinationSettings)
fgsDestinationSettings = Lens.field @"destinationSettings"
{-# DEPRECATED fgsDestinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead." #-}

instance Core.FromJSON FileGroupSettings where
  toJSON FileGroupSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("destination" Core..=) Core.<$> destination,
            ("destinationSettings" Core..=) Core.<$> destinationSettings
          ]
      )

instance Core.FromJSON FileGroupSettings where
  parseJSON =
    Core.withObject "FileGroupSettings" Core.$
      \x ->
        FileGroupSettings'
          Core.<$> (x Core..:? "destination")
          Core.<*> (x Core..:? "destinationSettings")
