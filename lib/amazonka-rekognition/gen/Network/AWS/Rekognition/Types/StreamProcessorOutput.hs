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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.KinesisDataStream

-- | Information about the Amazon Kinesis Data Streams stream to which a Amazon Rekognition Video stream processor streams the results of a video analysis. For more information, see CreateStreamProcessor in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkStreamProcessorOutput' smart constructor.
newtype StreamProcessorOutput = StreamProcessorOutput'
  { kinesisDataStream ::
      Lude.Maybe KinesisDataStream
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamProcessorOutput' with the minimum fields required to make a request.
--
-- * 'kinesisDataStream' - The Amazon Kinesis Data Streams stream to which the Amazon Rekognition stream processor streams the analysis results.
mkStreamProcessorOutput ::
  StreamProcessorOutput
mkStreamProcessorOutput =
  StreamProcessorOutput' {kinesisDataStream = Lude.Nothing}

-- | The Amazon Kinesis Data Streams stream to which the Amazon Rekognition stream processor streams the analysis results.
--
-- /Note:/ Consider using 'kinesisDataStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spoKinesisDataStream :: Lens.Lens' StreamProcessorOutput (Lude.Maybe KinesisDataStream)
spoKinesisDataStream = Lens.lens (kinesisDataStream :: StreamProcessorOutput -> Lude.Maybe KinesisDataStream) (\s a -> s {kinesisDataStream = a} :: StreamProcessorOutput)
{-# DEPRECATED spoKinesisDataStream "Use generic-lens or generic-optics with 'kinesisDataStream' instead." #-}

instance Lude.FromJSON StreamProcessorOutput where
  parseJSON =
    Lude.withObject
      "StreamProcessorOutput"
      ( \x ->
          StreamProcessorOutput' Lude.<$> (x Lude..:? "KinesisDataStream")
      )

instance Lude.ToJSON StreamProcessorOutput where
  toJSON StreamProcessorOutput' {..} =
    Lude.object
      ( Lude.catMaybes
          [("KinesisDataStream" Lude..=) Lude.<$> kinesisDataStream]
      )
