{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264FilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264FilterSettings
  ( H264FilterSettings (..),

    -- * Smart constructor
    mkH264FilterSettings,

    -- * Lenses
    hfsTemporalFilterSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.TemporalFilterSettings as Types
import qualified Network.AWS.Prelude as Core

-- | H264 Filter Settings
--
-- /See:/ 'mkH264FilterSettings' smart constructor.
newtype H264FilterSettings = H264FilterSettings'
  { temporalFilterSettings :: Core.Maybe Types.TemporalFilterSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'H264FilterSettings' value with any optional fields omitted.
mkH264FilterSettings ::
  H264FilterSettings
mkH264FilterSettings =
  H264FilterSettings' {temporalFilterSettings = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'temporalFilterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hfsTemporalFilterSettings :: Lens.Lens' H264FilterSettings (Core.Maybe Types.TemporalFilterSettings)
hfsTemporalFilterSettings = Lens.field @"temporalFilterSettings"
{-# DEPRECATED hfsTemporalFilterSettings "Use generic-lens or generic-optics with 'temporalFilterSettings' instead." #-}

instance Core.FromJSON H264FilterSettings where
  toJSON H264FilterSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("temporalFilterSettings" Core..=)
              Core.<$> temporalFilterSettings
          ]
      )

instance Core.FromJSON H264FilterSettings where
  parseJSON =
    Core.withObject "H264FilterSettings" Core.$
      \x ->
        H264FilterSettings' Core.<$> (x Core..:? "temporalFilterSettings")
