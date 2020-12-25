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

import qualified Network.AWS.Firehose.Types.KinesisStreamARN as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about a Kinesis data stream used as the source for a Kinesis Data Firehose delivery stream.
--
-- /See:/ 'mkKinesisStreamSourceDescription' smart constructor.
data KinesisStreamSourceDescription = KinesisStreamSourceDescription'
  { -- | Kinesis Data Firehose starts retrieving records from the Kinesis data stream starting with this timestamp.
    deliveryStartTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
    kinesisStreamARN :: Core.Maybe Types.KinesisStreamARN,
    -- | The ARN of the role used by the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
    roleARN :: Core.Maybe Types.RoleARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'KinesisStreamSourceDescription' value with any optional fields omitted.
mkKinesisStreamSourceDescription ::
  KinesisStreamSourceDescription
mkKinesisStreamSourceDescription =
  KinesisStreamSourceDescription'
    { deliveryStartTimestamp =
        Core.Nothing,
      kinesisStreamARN = Core.Nothing,
      roleARN = Core.Nothing
    }

-- | Kinesis Data Firehose starts retrieving records from the Kinesis data stream starting with this timestamp.
--
-- /Note:/ Consider using 'deliveryStartTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kssdDeliveryStartTimestamp :: Lens.Lens' KinesisStreamSourceDescription (Core.Maybe Core.NominalDiffTime)
kssdDeliveryStartTimestamp = Lens.field @"deliveryStartTimestamp"
{-# DEPRECATED kssdDeliveryStartTimestamp "Use generic-lens or generic-optics with 'deliveryStartTimestamp' instead." #-}

-- | The Amazon Resource Name (ARN) of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
--
-- /Note:/ Consider using 'kinesisStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kssdKinesisStreamARN :: Lens.Lens' KinesisStreamSourceDescription (Core.Maybe Types.KinesisStreamARN)
kssdKinesisStreamARN = Lens.field @"kinesisStreamARN"
{-# DEPRECATED kssdKinesisStreamARN "Use generic-lens or generic-optics with 'kinesisStreamARN' instead." #-}

-- | The ARN of the role used by the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kssdRoleARN :: Lens.Lens' KinesisStreamSourceDescription (Core.Maybe Types.RoleARN)
kssdRoleARN = Lens.field @"roleARN"
{-# DEPRECATED kssdRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Core.FromJSON KinesisStreamSourceDescription where
  parseJSON =
    Core.withObject "KinesisStreamSourceDescription" Core.$
      \x ->
        KinesisStreamSourceDescription'
          Core.<$> (x Core..:? "DeliveryStartTimestamp")
          Core.<*> (x Core..:? "KinesisStreamARN")
          Core.<*> (x Core..:? "RoleARN")
