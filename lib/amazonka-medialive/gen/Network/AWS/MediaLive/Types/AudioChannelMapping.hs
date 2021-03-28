{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioChannelMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioChannelMapping
  ( AudioChannelMapping (..)
  -- * Smart constructor
  , mkAudioChannelMapping
  -- * Lenses
  , acmOutputChannel
  , acmInputChannelLevels
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputChannelLevel as Types
import qualified Network.AWS.Prelude as Core

-- | Audio Channel Mapping
--
-- /See:/ 'mkAudioChannelMapping' smart constructor.
data AudioChannelMapping = AudioChannelMapping'
  { outputChannel :: Core.Natural
    -- ^ The index of the output channel being produced.
  , inputChannelLevels :: [Types.InputChannelLevel]
    -- ^ Indices and gain values for each input channel that should be remixed into this output channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioChannelMapping' value with any optional fields omitted.
mkAudioChannelMapping
    :: Core.Natural -- ^ 'outputChannel'
    -> AudioChannelMapping
mkAudioChannelMapping outputChannel
  = AudioChannelMapping'{outputChannel,
                         inputChannelLevels = Core.mempty}

-- | The index of the output channel being produced.
--
-- /Note:/ Consider using 'outputChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acmOutputChannel :: Lens.Lens' AudioChannelMapping Core.Natural
acmOutputChannel = Lens.field @"outputChannel"
{-# INLINEABLE acmOutputChannel #-}
{-# DEPRECATED outputChannel "Use generic-lens or generic-optics with 'outputChannel' instead"  #-}

-- | Indices and gain values for each input channel that should be remixed into this output channel.
--
-- /Note:/ Consider using 'inputChannelLevels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acmInputChannelLevels :: Lens.Lens' AudioChannelMapping [Types.InputChannelLevel]
acmInputChannelLevels = Lens.field @"inputChannelLevels"
{-# INLINEABLE acmInputChannelLevels #-}
{-# DEPRECATED inputChannelLevels "Use generic-lens or generic-optics with 'inputChannelLevels' instead"  #-}

instance Core.FromJSON AudioChannelMapping where
        toJSON AudioChannelMapping{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("outputChannel" Core..= outputChannel),
                  Core.Just ("inputChannelLevels" Core..= inputChannelLevels)])

instance Core.FromJSON AudioChannelMapping where
        parseJSON
          = Core.withObject "AudioChannelMapping" Core.$
              \ x ->
                AudioChannelMapping' Core.<$>
                  (x Core..: "outputChannel") Core.<*>
                    x Core..:? "inputChannelLevels" Core..!= Core.mempty
