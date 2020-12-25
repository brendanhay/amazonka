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
    kvsArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.KinesisVideoArn as Types

-- | Kinesis video stream stream that provides the source streaming video for a Amazon Rekognition Video stream processor. For more information, see CreateStreamProcessor in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkKinesisVideoStream' smart constructor.
newtype KinesisVideoStream = KinesisVideoStream'
  { -- | ARN of the Kinesis video stream stream that streams the source video.
    arn :: Core.Maybe Types.KinesisVideoArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisVideoStream' value with any optional fields omitted.
mkKinesisVideoStream ::
  KinesisVideoStream
mkKinesisVideoStream = KinesisVideoStream' {arn = Core.Nothing}

-- | ARN of the Kinesis video stream stream that streams the source video.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvsArn :: Lens.Lens' KinesisVideoStream (Core.Maybe Types.KinesisVideoArn)
kvsArn = Lens.field @"arn"
{-# DEPRECATED kvsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON KinesisVideoStream where
  toJSON KinesisVideoStream {..} =
    Core.object (Core.catMaybes [("Arn" Core..=) Core.<$> arn])

instance Core.FromJSON KinesisVideoStream where
  parseJSON =
    Core.withObject "KinesisVideoStream" Core.$
      \x -> KinesisVideoStream' Core.<$> (x Core..:? "Arn")
