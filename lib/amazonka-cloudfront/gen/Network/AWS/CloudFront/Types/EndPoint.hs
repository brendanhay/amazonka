{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EndPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.EndPoint
  ( EndPoint (..)
  -- * Smart constructor
  , mkEndPoint
  -- * Lenses
  , epStreamType
  , epKinesisStreamConfig
  ) where

import qualified Network.AWS.CloudFront.Types.KinesisStreamConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data in a real-time log configuration.
--
-- /See:/ 'mkEndPoint' smart constructor.
data EndPoint = EndPoint'
  { streamType :: Core.Text
    -- ^ The type of data stream where you are sending real-time log data. The only valid value is @Kinesis@ .
  , kinesisStreamConfig :: Core.Maybe Types.KinesisStreamConfig
    -- ^ Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndPoint' value with any optional fields omitted.
mkEndPoint
    :: Core.Text -- ^ 'streamType'
    -> EndPoint
mkEndPoint streamType
  = EndPoint'{streamType, kinesisStreamConfig = Core.Nothing}

-- | The type of data stream where you are sending real-time log data. The only valid value is @Kinesis@ .
--
-- /Note:/ Consider using 'streamType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epStreamType :: Lens.Lens' EndPoint Core.Text
epStreamType = Lens.field @"streamType"
{-# INLINEABLE epStreamType #-}
{-# DEPRECATED streamType "Use generic-lens or generic-optics with 'streamType' instead"  #-}

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
--
-- /Note:/ Consider using 'kinesisStreamConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epKinesisStreamConfig :: Lens.Lens' EndPoint (Core.Maybe Types.KinesisStreamConfig)
epKinesisStreamConfig = Lens.field @"kinesisStreamConfig"
{-# INLINEABLE epKinesisStreamConfig #-}
{-# DEPRECATED kinesisStreamConfig "Use generic-lens or generic-optics with 'kinesisStreamConfig' instead"  #-}

instance Core.ToXML EndPoint where
        toXML EndPoint{..}
          = Core.toXMLElement "StreamType" streamType Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "KinesisStreamConfig")
                kinesisStreamConfig

instance Core.FromXML EndPoint where
        parseXML x
          = EndPoint' Core.<$>
              (x Core..@ "StreamType") Core.<*> x Core..@? "KinesisStreamConfig"
