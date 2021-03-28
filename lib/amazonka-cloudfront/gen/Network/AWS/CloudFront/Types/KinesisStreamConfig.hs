{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KinesisStreamConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.KinesisStreamConfig
  ( KinesisStreamConfig (..)
  -- * Smart constructor
  , mkKinesisStreamConfig
  -- * Lenses
  , kscRoleARN
  , kscStreamARN
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
--
-- /See:/ 'mkKinesisStreamConfig' smart constructor.
data KinesisStreamConfig = KinesisStreamConfig'
  { roleARN :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that CloudFront can use to send real-time log data to your Kinesis data stream.
--
-- For more information the IAM role, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-iam-role Real-time log configuration IAM role> in the /Amazon CloudFront Developer Guide/ .
  , streamARN :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Kinesis data stream where you are sending real-time log data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisStreamConfig' value with any optional fields omitted.
mkKinesisStreamConfig
    :: Core.Text -- ^ 'roleARN'
    -> Core.Text -- ^ 'streamARN'
    -> KinesisStreamConfig
mkKinesisStreamConfig roleARN streamARN
  = KinesisStreamConfig'{roleARN, streamARN}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that CloudFront can use to send real-time log data to your Kinesis data stream.
--
-- For more information the IAM role, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-iam-role Real-time log configuration IAM role> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kscRoleARN :: Lens.Lens' KinesisStreamConfig Core.Text
kscRoleARN = Lens.field @"roleARN"
{-# INLINEABLE kscRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Kinesis data stream where you are sending real-time log data.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kscStreamARN :: Lens.Lens' KinesisStreamConfig Core.Text
kscStreamARN = Lens.field @"streamARN"
{-# INLINEABLE kscStreamARN #-}
{-# DEPRECATED streamARN "Use generic-lens or generic-optics with 'streamARN' instead"  #-}

instance Core.ToXML KinesisStreamConfig where
        toXML KinesisStreamConfig{..}
          = Core.toXMLElement "RoleARN" roleARN Core.<>
              Core.toXMLElement "StreamARN" streamARN

instance Core.FromXML KinesisStreamConfig where
        parseXML x
          = KinesisStreamConfig' Core.<$>
              (x Core..@ "RoleARN") Core.<*> x Core..@ "StreamARN"
