{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2FilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2FilterSettings
  ( Mpeg2FilterSettings (..),

    -- * Smart constructor
    mkMpeg2FilterSettings,

    -- * Lenses
    mfsTemporalFilterSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.TemporalFilterSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Mpeg2 Filter Settings
--
-- /See:/ 'mkMpeg2FilterSettings' smart constructor.
newtype Mpeg2FilterSettings = Mpeg2FilterSettings'
  { temporalFilterSettings :: Core.Maybe Types.TemporalFilterSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Mpeg2FilterSettings' value with any optional fields omitted.
mkMpeg2FilterSettings ::
  Mpeg2FilterSettings
mkMpeg2FilterSettings =
  Mpeg2FilterSettings' {temporalFilterSettings = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'temporalFilterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfsTemporalFilterSettings :: Lens.Lens' Mpeg2FilterSettings (Core.Maybe Types.TemporalFilterSettings)
mfsTemporalFilterSettings = Lens.field @"temporalFilterSettings"
{-# DEPRECATED mfsTemporalFilterSettings "Use generic-lens or generic-optics with 'temporalFilterSettings' instead." #-}

instance Core.FromJSON Mpeg2FilterSettings where
  toJSON Mpeg2FilterSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("temporalFilterSettings" Core..=)
              Core.<$> temporalFilterSettings
          ]
      )

instance Core.FromJSON Mpeg2FilterSettings where
  parseJSON =
    Core.withObject "Mpeg2FilterSettings" Core.$
      \x ->
        Mpeg2FilterSettings' Core.<$> (x Core..:? "temporalFilterSettings")
