{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte20SourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte20SourceSettings
  ( Scte20SourceSettings (..),

    -- * Smart constructor
    mkScte20SourceSettings,

    -- * Lenses
    sssConvert608To708,
    sssSource608ChannelNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Scte20Convert608To708 as Types
import qualified Network.AWS.Prelude as Core

-- | Scte20 Source Settings
--
-- /See:/ 'mkScte20SourceSettings' smart constructor.
data Scte20SourceSettings = Scte20SourceSettings'
  { -- | If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
    convert608To708 :: Core.Maybe Types.Scte20Convert608To708,
    -- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
    source608ChannelNumber :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Scte20SourceSettings' value with any optional fields omitted.
mkScte20SourceSettings ::
  Scte20SourceSettings
mkScte20SourceSettings =
  Scte20SourceSettings'
    { convert608To708 = Core.Nothing,
      source608ChannelNumber = Core.Nothing
    }

-- | If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
--
-- /Note:/ Consider using 'convert608To708' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssConvert608To708 :: Lens.Lens' Scte20SourceSettings (Core.Maybe Types.Scte20Convert608To708)
sssConvert608To708 = Lens.field @"convert608To708"
{-# DEPRECATED sssConvert608To708 "Use generic-lens or generic-optics with 'convert608To708' instead." #-}

-- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
--
-- /Note:/ Consider using 'source608ChannelNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSource608ChannelNumber :: Lens.Lens' Scte20SourceSettings (Core.Maybe Core.Natural)
sssSource608ChannelNumber = Lens.field @"source608ChannelNumber"
{-# DEPRECATED sssSource608ChannelNumber "Use generic-lens or generic-optics with 'source608ChannelNumber' instead." #-}

instance Core.FromJSON Scte20SourceSettings where
  toJSON Scte20SourceSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("convert608To708" Core..=) Core.<$> convert608To708,
            ("source608ChannelNumber" Core..=)
              Core.<$> source608ChannelNumber
          ]
      )

instance Core.FromJSON Scte20SourceSettings where
  parseJSON =
    Core.withObject "Scte20SourceSettings" Core.$
      \x ->
        Scte20SourceSettings'
          Core.<$> (x Core..:? "convert608To708")
          Core.<*> (x Core..:? "source608ChannelNumber")
