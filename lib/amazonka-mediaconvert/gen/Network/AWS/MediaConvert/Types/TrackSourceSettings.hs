{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TrackSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TrackSourceSettings
  ( TrackSourceSettings (..),

    -- * Smart constructor
    mkTrackSourceSettings,

    -- * Lenses
    tssTrackNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings specific to caption sources that are specified by track number. Currently, this is only IMSC captions in an IMF package. If your caption source is IMSC 1.1 in a separate xml file, use FileSourceSettings instead of TrackSourceSettings.
--
-- /See:/ 'mkTrackSourceSettings' smart constructor.
newtype TrackSourceSettings = TrackSourceSettings'
  { -- | Use this setting to select a single captions track from a source. Track numbers correspond to the order in the captions source file. For IMF sources, track numbering is based on the order that the captions appear in the CPL. For example, use 1 to select the captions asset that is listed first in the CPL. To include more than one captions track in your job outputs, create multiple input captions selectors. Specify one track per selector.
    trackNumber :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TrackSourceSettings' value with any optional fields omitted.
mkTrackSourceSettings ::
  TrackSourceSettings
mkTrackSourceSettings =
  TrackSourceSettings' {trackNumber = Core.Nothing}

-- | Use this setting to select a single captions track from a source. Track numbers correspond to the order in the captions source file. For IMF sources, track numbering is based on the order that the captions appear in the CPL. For example, use 1 to select the captions asset that is listed first in the CPL. To include more than one captions track in your job outputs, create multiple input captions selectors. Specify one track per selector.
--
-- /Note:/ Consider using 'trackNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tssTrackNumber :: Lens.Lens' TrackSourceSettings (Core.Maybe Core.Natural)
tssTrackNumber = Lens.field @"trackNumber"
{-# DEPRECATED tssTrackNumber "Use generic-lens or generic-optics with 'trackNumber' instead." #-}

instance Core.FromJSON TrackSourceSettings where
  toJSON TrackSourceSettings {..} =
    Core.object
      (Core.catMaybes [("trackNumber" Core..=) Core.<$> trackNumber])

instance Core.FromJSON TrackSourceSettings where
  parseJSON =
    Core.withObject "TrackSourceSettings" Core.$
      \x -> TrackSourceSettings' Core.<$> (x Core..:? "trackNumber")
