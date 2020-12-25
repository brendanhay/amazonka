{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.DeliveryChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.DeliveryChannel
  ( DeliveryChannel (..),

    -- * Smart constructor
    mkDeliveryChannel,

    -- * Lenses
    dcConfigSnapshotDeliveryProperties,
    dcName,
    dcS3BucketName,
    dcS3KeyPrefix,
    dcSnsTopicARN,
  )
where

import qualified Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties as Types
import qualified Network.AWS.Config.Types.Name as Types
import qualified Network.AWS.Config.Types.S3BucketName as Types
import qualified Network.AWS.Config.Types.S3KeyPrefix as Types
import qualified Network.AWS.Config.Types.SnsTopicARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The channel through which AWS Config delivers notifications and updated configuration states.
--
-- /See:/ 'mkDeliveryChannel' smart constructor.
data DeliveryChannel = DeliveryChannel'
  { -- | The options for how often AWS Config delivers configuration snapshots to the Amazon S3 bucket.
    configSnapshotDeliveryProperties :: Core.Maybe Types.ConfigSnapshotDeliveryProperties,
    -- | The name of the delivery channel. By default, AWS Config assigns the name "default" when creating the delivery channel. To change the delivery channel name, you must use the DeleteDeliveryChannel action to delete your current delivery channel, and then you must use the PutDeliveryChannel command to create a delivery channel that has the desired name.
    name :: Core.Maybe Types.Name,
    -- | The name of the Amazon S3 bucket to which AWS Config delivers configuration snapshots and configuration history files.
    --
    -- If you specify a bucket that belongs to another AWS account, that bucket must have policies that grant access permissions to AWS Config. For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-policy.html Permissions for the Amazon S3 Bucket> in the AWS Config Developer Guide.
    s3BucketName :: Core.Maybe Types.S3BucketName,
    -- | The prefix for the specified Amazon S3 bucket.
    s3KeyPrefix :: Core.Maybe Types.S3KeyPrefix,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which AWS Config sends notifications about configuration changes.
    --
    -- If you choose a topic from another account, the topic must have policies that grant access permissions to AWS Config. For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/sns-topic-policy.html Permissions for the Amazon SNS Topic> in the AWS Config Developer Guide.
    snsTopicARN :: Core.Maybe Types.SnsTopicARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeliveryChannel' value with any optional fields omitted.
mkDeliveryChannel ::
  DeliveryChannel
mkDeliveryChannel =
  DeliveryChannel'
    { configSnapshotDeliveryProperties = Core.Nothing,
      name = Core.Nothing,
      s3BucketName = Core.Nothing,
      s3KeyPrefix = Core.Nothing,
      snsTopicARN = Core.Nothing
    }

-- | The options for how often AWS Config delivers configuration snapshots to the Amazon S3 bucket.
--
-- /Note:/ Consider using 'configSnapshotDeliveryProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConfigSnapshotDeliveryProperties :: Lens.Lens' DeliveryChannel (Core.Maybe Types.ConfigSnapshotDeliveryProperties)
dcConfigSnapshotDeliveryProperties = Lens.field @"configSnapshotDeliveryProperties"
{-# DEPRECATED dcConfigSnapshotDeliveryProperties "Use generic-lens or generic-optics with 'configSnapshotDeliveryProperties' instead." #-}

-- | The name of the delivery channel. By default, AWS Config assigns the name "default" when creating the delivery channel. To change the delivery channel name, you must use the DeleteDeliveryChannel action to delete your current delivery channel, and then you must use the PutDeliveryChannel command to create a delivery channel that has the desired name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcName :: Lens.Lens' DeliveryChannel (Core.Maybe Types.Name)
dcName = Lens.field @"name"
{-# DEPRECATED dcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the Amazon S3 bucket to which AWS Config delivers configuration snapshots and configuration history files.
--
-- If you specify a bucket that belongs to another AWS account, that bucket must have policies that grant access permissions to AWS Config. For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-policy.html Permissions for the Amazon S3 Bucket> in the AWS Config Developer Guide.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcS3BucketName :: Lens.Lens' DeliveryChannel (Core.Maybe Types.S3BucketName)
dcS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED dcS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The prefix for the specified Amazon S3 bucket.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcS3KeyPrefix :: Lens.Lens' DeliveryChannel (Core.Maybe Types.S3KeyPrefix)
dcS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# DEPRECATED dcS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which AWS Config sends notifications about configuration changes.
--
-- If you choose a topic from another account, the topic must have policies that grant access permissions to AWS Config. For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/sns-topic-policy.html Permissions for the Amazon SNS Topic> in the AWS Config Developer Guide.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcSnsTopicARN :: Lens.Lens' DeliveryChannel (Core.Maybe Types.SnsTopicARN)
dcSnsTopicARN = Lens.field @"snsTopicARN"
{-# DEPRECATED dcSnsTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

instance Core.FromJSON DeliveryChannel where
  toJSON DeliveryChannel {..} =
    Core.object
      ( Core.catMaybes
          [ ("configSnapshotDeliveryProperties" Core..=)
              Core.<$> configSnapshotDeliveryProperties,
            ("name" Core..=) Core.<$> name,
            ("s3BucketName" Core..=) Core.<$> s3BucketName,
            ("s3KeyPrefix" Core..=) Core.<$> s3KeyPrefix,
            ("snsTopicARN" Core..=) Core.<$> snsTopicARN
          ]
      )

instance Core.FromJSON DeliveryChannel where
  parseJSON =
    Core.withObject "DeliveryChannel" Core.$
      \x ->
        DeliveryChannel'
          Core.<$> (x Core..:? "configSnapshotDeliveryProperties")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "s3BucketName")
          Core.<*> (x Core..:? "s3KeyPrefix")
          Core.<*> (x Core..:? "snsTopicARN")
