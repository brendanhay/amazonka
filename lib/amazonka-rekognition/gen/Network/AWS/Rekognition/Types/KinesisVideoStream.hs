{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.KinesisVideoStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.KinesisVideoStream
  ( KinesisVideoStream (..),

    -- * Smart constructor
    mkKinesisVideoStream,

    -- * Lenses
    kvsARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Kinesis video stream stream that provides the source streaming video for a Amazon Rekognition Video stream processor. For more information, see CreateStreamProcessor in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkKinesisVideoStream' smart constructor.
newtype KinesisVideoStream = KinesisVideoStream'
  { arn ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisVideoStream' with the minimum fields required to make a request.
--
-- * 'arn' - ARN of the Kinesis video stream stream that streams the source video.
mkKinesisVideoStream ::
  KinesisVideoStream
mkKinesisVideoStream = KinesisVideoStream' {arn = Lude.Nothing}

-- | ARN of the Kinesis video stream stream that streams the source video.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvsARN :: Lens.Lens' KinesisVideoStream (Lude.Maybe Lude.Text)
kvsARN = Lens.lens (arn :: KinesisVideoStream -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: KinesisVideoStream)
{-# DEPRECATED kvsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromJSON KinesisVideoStream where
  parseJSON =
    Lude.withObject
      "KinesisVideoStream"
      (\x -> KinesisVideoStream' Lude.<$> (x Lude..:? "Arn"))

instance Lude.ToJSON KinesisVideoStream where
  toJSON KinesisVideoStream' {..} =
    Lude.object (Lude.catMaybes [("Arn" Lude..=) Lude.<$> arn])
