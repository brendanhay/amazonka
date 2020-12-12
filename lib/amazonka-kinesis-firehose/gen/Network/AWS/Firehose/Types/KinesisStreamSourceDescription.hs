{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.KinesisStreamSourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KinesisStreamSourceDescription
  ( KinesisStreamSourceDescription (..),

    -- * Smart constructor
    mkKinesisStreamSourceDescription,

    -- * Lenses
    kssdDeliveryStartTimestamp,
    kssdKinesisStreamARN,
    kssdRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about a Kinesis data stream used as the source for a Kinesis Data Firehose delivery stream.
--
-- /See:/ 'mkKinesisStreamSourceDescription' smart constructor.
data KinesisStreamSourceDescription = KinesisStreamSourceDescription'
  { deliveryStartTimestamp ::
      Lude.Maybe Lude.Timestamp,
    kinesisStreamARN ::
      Lude.Maybe Lude.Text,
    roleARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisStreamSourceDescription' with the minimum fields required to make a request.
--
-- * 'deliveryStartTimestamp' - Kinesis Data Firehose starts retrieving records from the Kinesis data stream starting with this timestamp.
-- * 'kinesisStreamARN' - The Amazon Resource Name (ARN) of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
-- * 'roleARN' - The ARN of the role used by the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
mkKinesisStreamSourceDescription ::
  KinesisStreamSourceDescription
mkKinesisStreamSourceDescription =
  KinesisStreamSourceDescription'
    { deliveryStartTimestamp =
        Lude.Nothing,
      kinesisStreamARN = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Kinesis Data Firehose starts retrieving records from the Kinesis data stream starting with this timestamp.
--
-- /Note:/ Consider using 'deliveryStartTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kssdDeliveryStartTimestamp :: Lens.Lens' KinesisStreamSourceDescription (Lude.Maybe Lude.Timestamp)
kssdDeliveryStartTimestamp = Lens.lens (deliveryStartTimestamp :: KinesisStreamSourceDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {deliveryStartTimestamp = a} :: KinesisStreamSourceDescription)
{-# DEPRECATED kssdDeliveryStartTimestamp "Use generic-lens or generic-optics with 'deliveryStartTimestamp' instead." #-}

-- | The Amazon Resource Name (ARN) of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
--
-- /Note:/ Consider using 'kinesisStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kssdKinesisStreamARN :: Lens.Lens' KinesisStreamSourceDescription (Lude.Maybe Lude.Text)
kssdKinesisStreamARN = Lens.lens (kinesisStreamARN :: KinesisStreamSourceDescription -> Lude.Maybe Lude.Text) (\s a -> s {kinesisStreamARN = a} :: KinesisStreamSourceDescription)
{-# DEPRECATED kssdKinesisStreamARN "Use generic-lens or generic-optics with 'kinesisStreamARN' instead." #-}

-- | The ARN of the role used by the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kssdRoleARN :: Lens.Lens' KinesisStreamSourceDescription (Lude.Maybe Lude.Text)
kssdRoleARN = Lens.lens (roleARN :: KinesisStreamSourceDescription -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: KinesisStreamSourceDescription)
{-# DEPRECATED kssdRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON KinesisStreamSourceDescription where
  parseJSON =
    Lude.withObject
      "KinesisStreamSourceDescription"
      ( \x ->
          KinesisStreamSourceDescription'
            Lude.<$> (x Lude..:? "DeliveryStartTimestamp")
            Lude.<*> (x Lude..:? "KinesisStreamARN")
            Lude.<*> (x Lude..:? "RoleARN")
      )
