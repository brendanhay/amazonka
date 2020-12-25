{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ArchiveContainerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ArchiveContainerSettings
  ( ArchiveContainerSettings (..),

    -- * Smart constructor
    mkArchiveContainerSettings,

    -- * Lenses
    acsM2tsSettings,
    acsRawSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.M2tsSettings as Types
import qualified Network.AWS.MediaLive.Types.RawSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Archive Container Settings
--
-- /See:/ 'mkArchiveContainerSettings' smart constructor.
data ArchiveContainerSettings = ArchiveContainerSettings'
  { m2tsSettings :: Core.Maybe Types.M2tsSettings,
    rawSettings :: Core.Maybe Types.RawSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ArchiveContainerSettings' value with any optional fields omitted.
mkArchiveContainerSettings ::
  ArchiveContainerSettings
mkArchiveContainerSettings =
  ArchiveContainerSettings'
    { m2tsSettings = Core.Nothing,
      rawSettings = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'm2tsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsM2tsSettings :: Lens.Lens' ArchiveContainerSettings (Core.Maybe Types.M2tsSettings)
acsM2tsSettings = Lens.field @"m2tsSettings"
{-# DEPRECATED acsM2tsSettings "Use generic-lens or generic-optics with 'm2tsSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rawSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsRawSettings :: Lens.Lens' ArchiveContainerSettings (Core.Maybe Types.RawSettings)
acsRawSettings = Lens.field @"rawSettings"
{-# DEPRECATED acsRawSettings "Use generic-lens or generic-optics with 'rawSettings' instead." #-}

instance Core.FromJSON ArchiveContainerSettings where
  toJSON ArchiveContainerSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("m2tsSettings" Core..=) Core.<$> m2tsSettings,
            ("rawSettings" Core..=) Core.<$> rawSettings
          ]
      )

instance Core.FromJSON ArchiveContainerSettings where
  parseJSON =
    Core.withObject "ArchiveContainerSettings" Core.$
      \x ->
        ArchiveContainerSettings'
          Core.<$> (x Core..:? "m2tsSettings") Core.<*> (x Core..:? "rawSettings")
