{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EndPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.EndPoint
  ( EndPoint (..),

    -- * Smart constructor
    mkEndPoint,

    -- * Lenses
    epKinesisStreamConfig,
    epStreamType,
  )
where

import Network.AWS.CloudFront.Types.KinesisStreamConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data in a real-time log configuration.
--
-- /See:/ 'mkEndPoint' smart constructor.
data EndPoint = EndPoint'
  { kinesisStreamConfig ::
      Lude.Maybe KinesisStreamConfig,
    streamType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndPoint' with the minimum fields required to make a request.
--
-- * 'kinesisStreamConfig' - Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
-- * 'streamType' - The type of data stream where you are sending real-time log data. The only valid value is @Kinesis@ .
mkEndPoint ::
  -- | 'streamType'
  Lude.Text ->
  EndPoint
mkEndPoint pStreamType_ =
  EndPoint'
    { kinesisStreamConfig = Lude.Nothing,
      streamType = pStreamType_
    }

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
--
-- /Note:/ Consider using 'kinesisStreamConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epKinesisStreamConfig :: Lens.Lens' EndPoint (Lude.Maybe KinesisStreamConfig)
epKinesisStreamConfig = Lens.lens (kinesisStreamConfig :: EndPoint -> Lude.Maybe KinesisStreamConfig) (\s a -> s {kinesisStreamConfig = a} :: EndPoint)
{-# DEPRECATED epKinesisStreamConfig "Use generic-lens or generic-optics with 'kinesisStreamConfig' instead." #-}

-- | The type of data stream where you are sending real-time log data. The only valid value is @Kinesis@ .
--
-- /Note:/ Consider using 'streamType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epStreamType :: Lens.Lens' EndPoint Lude.Text
epStreamType = Lens.lens (streamType :: EndPoint -> Lude.Text) (\s a -> s {streamType = a} :: EndPoint)
{-# DEPRECATED epStreamType "Use generic-lens or generic-optics with 'streamType' instead." #-}

instance Lude.FromXML EndPoint where
  parseXML x =
    EndPoint'
      Lude.<$> (x Lude..@? "KinesisStreamConfig")
      Lude.<*> (x Lude..@ "StreamType")

instance Lude.ToXML EndPoint where
  toXML EndPoint' {..} =
    Lude.mconcat
      [ "KinesisStreamConfig" Lude.@= kinesisStreamConfig,
        "StreamType" Lude.@= streamType
      ]
