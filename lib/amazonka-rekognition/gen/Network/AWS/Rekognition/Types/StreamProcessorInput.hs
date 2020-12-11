-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StreamProcessorInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessorInput
  ( StreamProcessorInput (..),

    -- * Smart constructor
    mkStreamProcessorInput,

    -- * Lenses
    spiKinesisVideoStream,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.KinesisVideoStream

-- | Information about the source streaming video.
--
-- /See:/ 'mkStreamProcessorInput' smart constructor.
newtype StreamProcessorInput = StreamProcessorInput'
  { kinesisVideoStream ::
      Lude.Maybe KinesisVideoStream
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamProcessorInput' with the minimum fields required to make a request.
--
-- * 'kinesisVideoStream' - The Kinesis video stream input stream for the source streaming video.
mkStreamProcessorInput ::
  StreamProcessorInput
mkStreamProcessorInput =
  StreamProcessorInput' {kinesisVideoStream = Lude.Nothing}

-- | The Kinesis video stream input stream for the source streaming video.
--
-- /Note:/ Consider using 'kinesisVideoStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spiKinesisVideoStream :: Lens.Lens' StreamProcessorInput (Lude.Maybe KinesisVideoStream)
spiKinesisVideoStream = Lens.lens (kinesisVideoStream :: StreamProcessorInput -> Lude.Maybe KinesisVideoStream) (\s a -> s {kinesisVideoStream = a} :: StreamProcessorInput)
{-# DEPRECATED spiKinesisVideoStream "Use generic-lens or generic-optics with 'kinesisVideoStream' instead." #-}

instance Lude.FromJSON StreamProcessorInput where
  parseJSON =
    Lude.withObject
      "StreamProcessorInput"
      ( \x ->
          StreamProcessorInput' Lude.<$> (x Lude..:? "KinesisVideoStream")
      )

instance Lude.ToJSON StreamProcessorInput where
  toJSON StreamProcessorInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [("KinesisVideoStream" Lude..=) Lude.<$> kinesisVideoStream]
      )
