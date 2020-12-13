{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.KinesisDataStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.KinesisDataStream
  ( KinesisDataStream (..),

    -- * Smart constructor
    mkKinesisDataStream,

    -- * Lenses
    kdsARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Kinesis data stream Amazon Rekognition to which the analysis results of a Amazon Rekognition stream processor are streamed. For more information, see CreateStreamProcessor in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkKinesisDataStream' smart constructor.
newtype KinesisDataStream = KinesisDataStream'
  { -- | ARN of the output Amazon Kinesis Data Streams stream.
    arn :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisDataStream' with the minimum fields required to make a request.
--
-- * 'arn' - ARN of the output Amazon Kinesis Data Streams stream.
mkKinesisDataStream ::
  KinesisDataStream
mkKinesisDataStream = KinesisDataStream' {arn = Lude.Nothing}

-- | ARN of the output Amazon Kinesis Data Streams stream.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kdsARN :: Lens.Lens' KinesisDataStream (Lude.Maybe Lude.Text)
kdsARN = Lens.lens (arn :: KinesisDataStream -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: KinesisDataStream)
{-# DEPRECATED kdsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromJSON KinesisDataStream where
  parseJSON =
    Lude.withObject
      "KinesisDataStream"
      (\x -> KinesisDataStream' Lude.<$> (x Lude..:? "Arn"))

instance Lude.ToJSON KinesisDataStream where
  toJSON KinesisDataStream' {..} =
    Lude.object (Lude.catMaybes [("Arn" Lude..=) Lude.<$> arn])
