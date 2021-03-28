{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.KinesisFirehoseDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.KinesisFirehoseDestination
  ( KinesisFirehoseDestination (..)
  -- * Smart constructor
  , mkKinesisFirehoseDestination
  -- * Lenses
  , kfdIAMRoleARN
  , kfdDeliveryStreamARN
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.DeliveryStreamARN as Types
import qualified Network.AWS.SES.Types.IAMRoleARN as Types

-- | Contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.
--
-- Event destinations, such as Amazon Kinesis Firehose, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkKinesisFirehoseDestination' smart constructor.
data KinesisFirehoseDestination = KinesisFirehoseDestination'
  { iAMRoleARN :: Types.IAMRoleARN
    -- ^ The ARN of the IAM role under which Amazon SES publishes email sending events to the Amazon Kinesis Firehose stream.
  , deliveryStreamARN :: Types.DeliveryStreamARN
    -- ^ The ARN of the Amazon Kinesis Firehose stream that email sending events should be published to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisFirehoseDestination' value with any optional fields omitted.
mkKinesisFirehoseDestination
    :: Types.IAMRoleARN -- ^ 'iAMRoleARN'
    -> Types.DeliveryStreamARN -- ^ 'deliveryStreamARN'
    -> KinesisFirehoseDestination
mkKinesisFirehoseDestination iAMRoleARN deliveryStreamARN
  = KinesisFirehoseDestination'{iAMRoleARN, deliveryStreamARN}

-- | The ARN of the IAM role under which Amazon SES publishes email sending events to the Amazon Kinesis Firehose stream.
--
-- /Note:/ Consider using 'iAMRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfdIAMRoleARN :: Lens.Lens' KinesisFirehoseDestination Types.IAMRoleARN
kfdIAMRoleARN = Lens.field @"iAMRoleARN"
{-# INLINEABLE kfdIAMRoleARN #-}
{-# DEPRECATED iAMRoleARN "Use generic-lens or generic-optics with 'iAMRoleARN' instead"  #-}

-- | The ARN of the Amazon Kinesis Firehose stream that email sending events should be published to.
--
-- /Note:/ Consider using 'deliveryStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfdDeliveryStreamARN :: Lens.Lens' KinesisFirehoseDestination Types.DeliveryStreamARN
kfdDeliveryStreamARN = Lens.field @"deliveryStreamARN"
{-# INLINEABLE kfdDeliveryStreamARN #-}
{-# DEPRECATED deliveryStreamARN "Use generic-lens or generic-optics with 'deliveryStreamARN' instead"  #-}

instance Core.ToQuery KinesisFirehoseDestination where
        toQuery KinesisFirehoseDestination{..}
          = Core.toQueryPair "IAMRoleARN" iAMRoleARN Core.<>
              Core.toQueryPair "DeliveryStreamARN" deliveryStreamARN

instance Core.FromXML KinesisFirehoseDestination where
        parseXML x
          = KinesisFirehoseDestination' Core.<$>
              (x Core..@ "IAMRoleARN") Core.<*> x Core..@ "DeliveryStreamARN"
