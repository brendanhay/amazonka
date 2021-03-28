{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ChannelMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ChannelMapping
  ( ChannelMapping (..)
  -- * Smart constructor
  , mkChannelMapping
  -- * Lenses
  , cmOutputChannels
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.OutputChannelMapping as Types
import qualified Network.AWS.Prelude as Core

-- | Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).
--
-- /See:/ 'mkChannelMapping' smart constructor.
newtype ChannelMapping = ChannelMapping'
  { outputChannels :: Core.Maybe [Types.OutputChannelMapping]
    -- ^ List of output channels
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ChannelMapping' value with any optional fields omitted.
mkChannelMapping
    :: ChannelMapping
mkChannelMapping = ChannelMapping'{outputChannels = Core.Nothing}

-- | List of output channels
--
-- /Note:/ Consider using 'outputChannels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmOutputChannels :: Lens.Lens' ChannelMapping (Core.Maybe [Types.OutputChannelMapping])
cmOutputChannels = Lens.field @"outputChannels"
{-# INLINEABLE cmOutputChannels #-}
{-# DEPRECATED outputChannels "Use generic-lens or generic-optics with 'outputChannels' instead"  #-}

instance Core.FromJSON ChannelMapping where
        toJSON ChannelMapping{..}
          = Core.object
              (Core.catMaybes
                 [("outputChannels" Core..=) Core.<$> outputChannels])

instance Core.FromJSON ChannelMapping where
        parseJSON
          = Core.withObject "ChannelMapping" Core.$
              \ x -> ChannelMapping' Core.<$> (x Core..:? "outputChannels")
