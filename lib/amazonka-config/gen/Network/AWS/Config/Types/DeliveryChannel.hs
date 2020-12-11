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
    dcS3KeyPrefix,
    dcSnsTopicARN,
    dcName,
    dcConfigSnapshotDeliveryProperties,
    dcS3BucketName,
  )
where

import Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The channel through which AWS Config delivers notifications and updated configuration states.
--
-- /See:/ 'mkDeliveryChannel' smart constructor.
data DeliveryChannel = DeliveryChannel'
  { s3KeyPrefix ::
      Lude.Maybe Lude.Text,
    snsTopicARN :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    configSnapshotDeliveryProperties ::
      Lude.Maybe ConfigSnapshotDeliveryProperties,
    s3BucketName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeliveryChannel' with the minimum fields required to make a request.
--
-- * 'configSnapshotDeliveryProperties' - The options for how often AWS Config delivers configuration snapshots to the Amazon S3 bucket.
-- * 'name' - The name of the delivery channel. By default, AWS Config assigns the name "default" when creating the delivery channel. To change the delivery channel name, you must use the DeleteDeliveryChannel action to delete your current delivery channel, and then you must use the PutDeliveryChannel command to create a delivery channel that has the desired name.
-- * 's3BucketName' - The name of the Amazon S3 bucket to which AWS Config delivers configuration snapshots and configuration history files.
--
-- If you specify a bucket that belongs to another AWS account, that bucket must have policies that grant access permissions to AWS Config. For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-policy.html Permissions for the Amazon S3 Bucket> in the AWS Config Developer Guide.
-- * 's3KeyPrefix' - The prefix for the specified Amazon S3 bucket.
-- * 'snsTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which AWS Config sends notifications about configuration changes.
--
-- If you choose a topic from another account, the topic must have policies that grant access permissions to AWS Config. For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/sns-topic-policy.html Permissions for the Amazon SNS Topic> in the AWS Config Developer Guide.
mkDeliveryChannel ::
  DeliveryChannel
mkDeliveryChannel =
  DeliveryChannel'
    { s3KeyPrefix = Lude.Nothing,
      snsTopicARN = Lude.Nothing,
      name = Lude.Nothing,
      configSnapshotDeliveryProperties = Lude.Nothing,
      s3BucketName = Lude.Nothing
    }

-- | The prefix for the specified Amazon S3 bucket.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcS3KeyPrefix :: Lens.Lens' DeliveryChannel (Lude.Maybe Lude.Text)
dcS3KeyPrefix = Lens.lens (s3KeyPrefix :: DeliveryChannel -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: DeliveryChannel)
{-# DEPRECATED dcS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which AWS Config sends notifications about configuration changes.
--
-- If you choose a topic from another account, the topic must have policies that grant access permissions to AWS Config. For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/sns-topic-policy.html Permissions for the Amazon SNS Topic> in the AWS Config Developer Guide.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcSnsTopicARN :: Lens.Lens' DeliveryChannel (Lude.Maybe Lude.Text)
dcSnsTopicARN = Lens.lens (snsTopicARN :: DeliveryChannel -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: DeliveryChannel)
{-# DEPRECATED dcSnsTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | The name of the delivery channel. By default, AWS Config assigns the name "default" when creating the delivery channel. To change the delivery channel name, you must use the DeleteDeliveryChannel action to delete your current delivery channel, and then you must use the PutDeliveryChannel command to create a delivery channel that has the desired name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcName :: Lens.Lens' DeliveryChannel (Lude.Maybe Lude.Text)
dcName = Lens.lens (name :: DeliveryChannel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeliveryChannel)
{-# DEPRECATED dcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The options for how often AWS Config delivers configuration snapshots to the Amazon S3 bucket.
--
-- /Note:/ Consider using 'configSnapshotDeliveryProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConfigSnapshotDeliveryProperties :: Lens.Lens' DeliveryChannel (Lude.Maybe ConfigSnapshotDeliveryProperties)
dcConfigSnapshotDeliveryProperties = Lens.lens (configSnapshotDeliveryProperties :: DeliveryChannel -> Lude.Maybe ConfigSnapshotDeliveryProperties) (\s a -> s {configSnapshotDeliveryProperties = a} :: DeliveryChannel)
{-# DEPRECATED dcConfigSnapshotDeliveryProperties "Use generic-lens or generic-optics with 'configSnapshotDeliveryProperties' instead." #-}

-- | The name of the Amazon S3 bucket to which AWS Config delivers configuration snapshots and configuration history files.
--
-- If you specify a bucket that belongs to another AWS account, that bucket must have policies that grant access permissions to AWS Config. For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-policy.html Permissions for the Amazon S3 Bucket> in the AWS Config Developer Guide.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcS3BucketName :: Lens.Lens' DeliveryChannel (Lude.Maybe Lude.Text)
dcS3BucketName = Lens.lens (s3BucketName :: DeliveryChannel -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: DeliveryChannel)
{-# DEPRECATED dcS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

instance Lude.FromJSON DeliveryChannel where
  parseJSON =
    Lude.withObject
      "DeliveryChannel"
      ( \x ->
          DeliveryChannel'
            Lude.<$> (x Lude..:? "s3KeyPrefix")
            Lude.<*> (x Lude..:? "snsTopicARN")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "configSnapshotDeliveryProperties")
            Lude.<*> (x Lude..:? "s3BucketName")
      )

instance Lude.ToJSON DeliveryChannel where
  toJSON DeliveryChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("s3KeyPrefix" Lude..=) Lude.<$> s3KeyPrefix,
            ("snsTopicARN" Lude..=) Lude.<$> snsTopicARN,
            ("name" Lude..=) Lude.<$> name,
            ("configSnapshotDeliveryProperties" Lude..=)
              Lude.<$> configSnapshotDeliveryProperties,
            ("s3BucketName" Lude..=) Lude.<$> s3BucketName
          ]
      )
