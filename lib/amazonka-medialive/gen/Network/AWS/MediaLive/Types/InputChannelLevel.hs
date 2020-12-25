{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputChannelLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputChannelLevel
  ( InputChannelLevel (..),

    -- * Smart constructor
    mkInputChannelLevel,

    -- * Lenses
    iclInputChannel,
    iclGain,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Input Channel Level
--
-- /See:/ 'mkInputChannelLevel' smart constructor.
data InputChannelLevel = InputChannelLevel'
  { -- | The index of the input channel used as a source.
    inputChannel :: Core.Natural,
    -- | Remixing value. Units are in dB and acceptable values are within the range from -60 (mute) and 6 dB.
    gain :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputChannelLevel' value with any optional fields omitted.
mkInputChannelLevel ::
  -- | 'inputChannel'
  Core.Natural ->
  -- | 'gain'
  Core.Int ->
  InputChannelLevel
mkInputChannelLevel inputChannel gain =
  InputChannelLevel' {inputChannel, gain}

-- | The index of the input channel used as a source.
--
-- /Note:/ Consider using 'inputChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iclInputChannel :: Lens.Lens' InputChannelLevel Core.Natural
iclInputChannel = Lens.field @"inputChannel"
{-# DEPRECATED iclInputChannel "Use generic-lens or generic-optics with 'inputChannel' instead." #-}

-- | Remixing value. Units are in dB and acceptable values are within the range from -60 (mute) and 6 dB.
--
-- /Note:/ Consider using 'gain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iclGain :: Lens.Lens' InputChannelLevel Core.Int
iclGain = Lens.field @"gain"
{-# DEPRECATED iclGain "Use generic-lens or generic-optics with 'gain' instead." #-}

instance Core.FromJSON InputChannelLevel where
  toJSON InputChannelLevel {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("inputChannel" Core..= inputChannel),
            Core.Just ("gain" Core..= gain)
          ]
      )

instance Core.FromJSON InputChannelLevel where
  parseJSON =
    Core.withObject "InputChannelLevel" Core.$
      \x ->
        InputChannelLevel'
          Core.<$> (x Core..: "inputChannel") Core.<*> (x Core..: "gain")
