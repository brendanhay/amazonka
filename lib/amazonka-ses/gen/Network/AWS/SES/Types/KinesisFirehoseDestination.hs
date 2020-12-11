-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.KinesisFirehoseDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.KinesisFirehoseDestination
  ( KinesisFirehoseDestination (..),

    -- * Smart constructor
    mkKinesisFirehoseDestination,

    -- * Lenses
    kfdIAMRoleARN,
    kfdDeliveryStreamARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.
--
-- Event destinations, such as Amazon Kinesis Firehose, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkKinesisFirehoseDestination' smart constructor.
data KinesisFirehoseDestination = KinesisFirehoseDestination'
  { iamRoleARN ::
      Lude.Text,
    deliveryStreamARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisFirehoseDestination' with the minimum fields required to make a request.
--
-- * 'deliveryStreamARN' - The ARN of the Amazon Kinesis Firehose stream that email sending events should be published to.
-- * 'iamRoleARN' - The ARN of the IAM role under which Amazon SES publishes email sending events to the Amazon Kinesis Firehose stream.
mkKinesisFirehoseDestination ::
  -- | 'iamRoleARN'
  Lude.Text ->
  -- | 'deliveryStreamARN'
  Lude.Text ->
  KinesisFirehoseDestination
mkKinesisFirehoseDestination pIAMRoleARN_ pDeliveryStreamARN_ =
  KinesisFirehoseDestination'
    { iamRoleARN = pIAMRoleARN_,
      deliveryStreamARN = pDeliveryStreamARN_
    }

-- | The ARN of the IAM role under which Amazon SES publishes email sending events to the Amazon Kinesis Firehose stream.
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfdIAMRoleARN :: Lens.Lens' KinesisFirehoseDestination Lude.Text
kfdIAMRoleARN = Lens.lens (iamRoleARN :: KinesisFirehoseDestination -> Lude.Text) (\s a -> s {iamRoleARN = a} :: KinesisFirehoseDestination)
{-# DEPRECATED kfdIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The ARN of the Amazon Kinesis Firehose stream that email sending events should be published to.
--
-- /Note:/ Consider using 'deliveryStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfdDeliveryStreamARN :: Lens.Lens' KinesisFirehoseDestination Lude.Text
kfdDeliveryStreamARN = Lens.lens (deliveryStreamARN :: KinesisFirehoseDestination -> Lude.Text) (\s a -> s {deliveryStreamARN = a} :: KinesisFirehoseDestination)
{-# DEPRECATED kfdDeliveryStreamARN "Use generic-lens or generic-optics with 'deliveryStreamARN' instead." #-}

instance Lude.FromXML KinesisFirehoseDestination where
  parseXML x =
    KinesisFirehoseDestination'
      Lude.<$> (x Lude..@ "IAMRoleARN") Lude.<*> (x Lude..@ "DeliveryStreamARN")

instance Lude.ToQuery KinesisFirehoseDestination where
  toQuery KinesisFirehoseDestination' {..} =
    Lude.mconcat
      [ "IAMRoleARN" Lude.=: iamRoleARN,
        "DeliveryStreamARN" Lude.=: deliveryStreamARN
      ]
