{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.KinesisVideoStream as Types

-- | Information about the source streaming video.
--
-- /See:/ 'mkStreamProcessorInput' smart constructor.
newtype StreamProcessorInput = StreamProcessorInput'
  { -- | The Kinesis video stream input stream for the source streaming video.
    kinesisVideoStream :: Core.Maybe Types.KinesisVideoStream
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StreamProcessorInput' value with any optional fields omitted.
mkStreamProcessorInput ::
  StreamProcessorInput
mkStreamProcessorInput =
  StreamProcessorInput' {kinesisVideoStream = Core.Nothing}

-- | The Kinesis video stream input stream for the source streaming video.
--
-- /Note:/ Consider using 'kinesisVideoStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spiKinesisVideoStream :: Lens.Lens' StreamProcessorInput (Core.Maybe Types.KinesisVideoStream)
spiKinesisVideoStream = Lens.field @"kinesisVideoStream"
{-# DEPRECATED spiKinesisVideoStream "Use generic-lens or generic-optics with 'kinesisVideoStream' instead." #-}

instance Core.FromJSON StreamProcessorInput where
  toJSON StreamProcessorInput {..} =
    Core.object
      ( Core.catMaybes
          [("KinesisVideoStream" Core..=) Core.<$> kinesisVideoStream]
      )

instance Core.FromJSON StreamProcessorInput where
  parseJSON =
    Core.withObject "StreamProcessorInput" Core.$
      \x ->
        StreamProcessorInput' Core.<$> (x Core..:? "KinesisVideoStream")
