{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265FilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265FilterSettings
  ( H265FilterSettings (..),

    -- * Smart constructor
    mkH265FilterSettings,

    -- * Lenses
    hTemporalFilterSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.TemporalFilterSettings as Types
import qualified Network.AWS.Prelude as Core

-- | H265 Filter Settings
--
-- /See:/ 'mkH265FilterSettings' smart constructor.
newtype H265FilterSettings = H265FilterSettings'
  { temporalFilterSettings :: Core.Maybe Types.TemporalFilterSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'H265FilterSettings' value with any optional fields omitted.
mkH265FilterSettings ::
  H265FilterSettings
mkH265FilterSettings =
  H265FilterSettings' {temporalFilterSettings = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'temporalFilterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTemporalFilterSettings :: Lens.Lens' H265FilterSettings (Core.Maybe Types.TemporalFilterSettings)
hTemporalFilterSettings = Lens.field @"temporalFilterSettings"
{-# DEPRECATED hTemporalFilterSettings "Use generic-lens or generic-optics with 'temporalFilterSettings' instead." #-}

instance Core.FromJSON H265FilterSettings where
  toJSON H265FilterSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("temporalFilterSettings" Core..=)
              Core.<$> temporalFilterSettings
          ]
      )

instance Core.FromJSON H265FilterSettings where
  parseJSON =
    Core.withObject "H265FilterSettings" Core.$
      \x ->
        H265FilterSettings' Core.<$> (x Core..:? "temporalFilterSettings")
