{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StreamProcessorOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessorOutput
  ( StreamProcessorOutput (..),

    -- * Smart constructor
    mkStreamProcessorOutput,

    -- * Lenses
    spoKinesisDataStream,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.KinesisDataStream as Types

-- | Information about the Amazon Kinesis Data Streams stream to which a Amazon Rekognition Video stream processor streams the results of a video analysis. For more information, see CreateStreamProcessor in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkStreamProcessorOutput' smart constructor.
newtype StreamProcessorOutput = StreamProcessorOutput'
  { -- | The Amazon Kinesis Data Streams stream to which the Amazon Rekognition stream processor streams the analysis results.
    kinesisDataStream :: Core.Maybe Types.KinesisDataStream
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StreamProcessorOutput' value with any optional fields omitted.
mkStreamProcessorOutput ::
  StreamProcessorOutput
mkStreamProcessorOutput =
  StreamProcessorOutput' {kinesisDataStream = Core.Nothing}

-- | The Amazon Kinesis Data Streams stream to which the Amazon Rekognition stream processor streams the analysis results.
--
-- /Note:/ Consider using 'kinesisDataStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spoKinesisDataStream :: Lens.Lens' StreamProcessorOutput (Core.Maybe Types.KinesisDataStream)
spoKinesisDataStream = Lens.field @"kinesisDataStream"
{-# DEPRECATED spoKinesisDataStream "Use generic-lens or generic-optics with 'kinesisDataStream' instead." #-}

instance Core.FromJSON StreamProcessorOutput where
  toJSON StreamProcessorOutput {..} =
    Core.object
      ( Core.catMaybes
          [("KinesisDataStream" Core..=) Core.<$> kinesisDataStream]
      )

instance Core.FromJSON StreamProcessorOutput where
  parseJSON =
    Core.withObject "StreamProcessorOutput" Core.$
      \x ->
        StreamProcessorOutput' Core.<$> (x Core..:? "KinesisDataStream")
