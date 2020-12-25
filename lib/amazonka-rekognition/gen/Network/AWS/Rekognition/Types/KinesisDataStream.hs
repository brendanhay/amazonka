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
    kdsArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.Arn as Types

-- | The Kinesis data stream Amazon Rekognition to which the analysis results of a Amazon Rekognition stream processor are streamed. For more information, see CreateStreamProcessor in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkKinesisDataStream' smart constructor.
newtype KinesisDataStream = KinesisDataStream'
  { -- | ARN of the output Amazon Kinesis Data Streams stream.
    arn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisDataStream' value with any optional fields omitted.
mkKinesisDataStream ::
  KinesisDataStream
mkKinesisDataStream = KinesisDataStream' {arn = Core.Nothing}

-- | ARN of the output Amazon Kinesis Data Streams stream.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kdsArn :: Lens.Lens' KinesisDataStream (Core.Maybe Types.Arn)
kdsArn = Lens.field @"arn"
{-# DEPRECATED kdsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON KinesisDataStream where
  toJSON KinesisDataStream {..} =
    Core.object (Core.catMaybes [("Arn" Core..=) Core.<$> arn])

instance Core.FromJSON KinesisDataStream where
  parseJSON =
    Core.withObject "KinesisDataStream" Core.$
      \x -> KinesisDataStream' Core.<$> (x Core..:? "Arn")
