-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ChannelMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ChannelMapping
  ( ChannelMapping (..),

    -- * Smart constructor
    mkChannelMapping,

    -- * Lenses
    cmOutputChannels,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.OutputChannelMapping
import qualified Network.AWS.Prelude as Lude

-- | Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).
--
-- /See:/ 'mkChannelMapping' smart constructor.
newtype ChannelMapping = ChannelMapping'
  { outputChannels ::
      Lude.Maybe [OutputChannelMapping]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChannelMapping' with the minimum fields required to make a request.
--
-- * 'outputChannels' - List of output channels
mkChannelMapping ::
  ChannelMapping
mkChannelMapping = ChannelMapping' {outputChannels = Lude.Nothing}

-- | List of output channels
--
-- /Note:/ Consider using 'outputChannels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmOutputChannels :: Lens.Lens' ChannelMapping (Lude.Maybe [OutputChannelMapping])
cmOutputChannels = Lens.lens (outputChannels :: ChannelMapping -> Lude.Maybe [OutputChannelMapping]) (\s a -> s {outputChannels = a} :: ChannelMapping)
{-# DEPRECATED cmOutputChannels "Use generic-lens or generic-optics with 'outputChannels' instead." #-}

instance Lude.FromJSON ChannelMapping where
  parseJSON =
    Lude.withObject
      "ChannelMapping"
      ( \x ->
          ChannelMapping'
            Lude.<$> (x Lude..:? "outputChannels" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ChannelMapping where
  toJSON ChannelMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [("outputChannels" Lude..=) Lude.<$> outputChannels]
      )
