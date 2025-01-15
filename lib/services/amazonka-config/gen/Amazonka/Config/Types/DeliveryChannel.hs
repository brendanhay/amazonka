{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Config.Types.DeliveryChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.DeliveryChannel where

import Amazonka.Config.Types.ConfigSnapshotDeliveryProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The channel through which Config delivers notifications and updated
-- configuration states.
--
-- /See:/ 'newDeliveryChannel' smart constructor.
data DeliveryChannel = DeliveryChannel'
  { -- | The options for how often Config delivers configuration snapshots to the
    -- Amazon S3 bucket.
    configSnapshotDeliveryProperties :: Prelude.Maybe ConfigSnapshotDeliveryProperties,
    -- | The name of the delivery channel. By default, Config assigns the name
    -- \"default\" when creating the delivery channel. To change the delivery
    -- channel name, you must use the DeleteDeliveryChannel action to delete
    -- your current delivery channel, and then you must use the
    -- PutDeliveryChannel command to create a delivery channel that has the
    -- desired name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket to which Config delivers configuration
    -- snapshots and configuration history files.
    --
    -- If you specify a bucket that belongs to another Amazon Web Services
    -- account, that bucket must have policies that grant access permissions to
    -- Config. For more information, see
    -- <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-policy.html Permissions for the Amazon S3 Bucket>
    -- in the /Config Developer Guide/.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The prefix for the specified Amazon S3 bucket.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Key Management Service (KMS ) KMS
    -- key (KMS key) used to encrypt objects delivered by Config. Must belong
    -- to the same Region as the destination S3 bucket.
    s3KmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which Config
    -- sends notifications about configuration changes.
    --
    -- If you choose a topic from another account, the topic must have policies
    -- that grant access permissions to Config. For more information, see
    -- <https://docs.aws.amazon.com/config/latest/developerguide/sns-topic-policy.html Permissions for the Amazon SNS Topic>
    -- in the /Config Developer Guide/.
    snsTopicARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeliveryChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configSnapshotDeliveryProperties', 'deliveryChannel_configSnapshotDeliveryProperties' - The options for how often Config delivers configuration snapshots to the
-- Amazon S3 bucket.
--
-- 'name', 'deliveryChannel_name' - The name of the delivery channel. By default, Config assigns the name
-- \"default\" when creating the delivery channel. To change the delivery
-- channel name, you must use the DeleteDeliveryChannel action to delete
-- your current delivery channel, and then you must use the
-- PutDeliveryChannel command to create a delivery channel that has the
-- desired name.
--
-- 's3BucketName', 'deliveryChannel_s3BucketName' - The name of the Amazon S3 bucket to which Config delivers configuration
-- snapshots and configuration history files.
--
-- If you specify a bucket that belongs to another Amazon Web Services
-- account, that bucket must have policies that grant access permissions to
-- Config. For more information, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-policy.html Permissions for the Amazon S3 Bucket>
-- in the /Config Developer Guide/.
--
-- 's3KeyPrefix', 'deliveryChannel_s3KeyPrefix' - The prefix for the specified Amazon S3 bucket.
--
-- 's3KmsKeyArn', 'deliveryChannel_s3KmsKeyArn' - The Amazon Resource Name (ARN) of the Key Management Service (KMS ) KMS
-- key (KMS key) used to encrypt objects delivered by Config. Must belong
-- to the same Region as the destination S3 bucket.
--
-- 'snsTopicARN', 'deliveryChannel_snsTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which Config
-- sends notifications about configuration changes.
--
-- If you choose a topic from another account, the topic must have policies
-- that grant access permissions to Config. For more information, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/sns-topic-policy.html Permissions for the Amazon SNS Topic>
-- in the /Config Developer Guide/.
newDeliveryChannel ::
  DeliveryChannel
newDeliveryChannel =
  DeliveryChannel'
    { configSnapshotDeliveryProperties =
        Prelude.Nothing,
      name = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      s3KmsKeyArn = Prelude.Nothing,
      snsTopicARN = Prelude.Nothing
    }

-- | The options for how often Config delivers configuration snapshots to the
-- Amazon S3 bucket.
deliveryChannel_configSnapshotDeliveryProperties :: Lens.Lens' DeliveryChannel (Prelude.Maybe ConfigSnapshotDeliveryProperties)
deliveryChannel_configSnapshotDeliveryProperties = Lens.lens (\DeliveryChannel' {configSnapshotDeliveryProperties} -> configSnapshotDeliveryProperties) (\s@DeliveryChannel' {} a -> s {configSnapshotDeliveryProperties = a} :: DeliveryChannel)

-- | The name of the delivery channel. By default, Config assigns the name
-- \"default\" when creating the delivery channel. To change the delivery
-- channel name, you must use the DeleteDeliveryChannel action to delete
-- your current delivery channel, and then you must use the
-- PutDeliveryChannel command to create a delivery channel that has the
-- desired name.
deliveryChannel_name :: Lens.Lens' DeliveryChannel (Prelude.Maybe Prelude.Text)
deliveryChannel_name = Lens.lens (\DeliveryChannel' {name} -> name) (\s@DeliveryChannel' {} a -> s {name = a} :: DeliveryChannel)

-- | The name of the Amazon S3 bucket to which Config delivers configuration
-- snapshots and configuration history files.
--
-- If you specify a bucket that belongs to another Amazon Web Services
-- account, that bucket must have policies that grant access permissions to
-- Config. For more information, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-policy.html Permissions for the Amazon S3 Bucket>
-- in the /Config Developer Guide/.
deliveryChannel_s3BucketName :: Lens.Lens' DeliveryChannel (Prelude.Maybe Prelude.Text)
deliveryChannel_s3BucketName = Lens.lens (\DeliveryChannel' {s3BucketName} -> s3BucketName) (\s@DeliveryChannel' {} a -> s {s3BucketName = a} :: DeliveryChannel)

-- | The prefix for the specified Amazon S3 bucket.
deliveryChannel_s3KeyPrefix :: Lens.Lens' DeliveryChannel (Prelude.Maybe Prelude.Text)
deliveryChannel_s3KeyPrefix = Lens.lens (\DeliveryChannel' {s3KeyPrefix} -> s3KeyPrefix) (\s@DeliveryChannel' {} a -> s {s3KeyPrefix = a} :: DeliveryChannel)

-- | The Amazon Resource Name (ARN) of the Key Management Service (KMS ) KMS
-- key (KMS key) used to encrypt objects delivered by Config. Must belong
-- to the same Region as the destination S3 bucket.
deliveryChannel_s3KmsKeyArn :: Lens.Lens' DeliveryChannel (Prelude.Maybe Prelude.Text)
deliveryChannel_s3KmsKeyArn = Lens.lens (\DeliveryChannel' {s3KmsKeyArn} -> s3KmsKeyArn) (\s@DeliveryChannel' {} a -> s {s3KmsKeyArn = a} :: DeliveryChannel)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which Config
-- sends notifications about configuration changes.
--
-- If you choose a topic from another account, the topic must have policies
-- that grant access permissions to Config. For more information, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/sns-topic-policy.html Permissions for the Amazon SNS Topic>
-- in the /Config Developer Guide/.
deliveryChannel_snsTopicARN :: Lens.Lens' DeliveryChannel (Prelude.Maybe Prelude.Text)
deliveryChannel_snsTopicARN = Lens.lens (\DeliveryChannel' {snsTopicARN} -> snsTopicARN) (\s@DeliveryChannel' {} a -> s {snsTopicARN = a} :: DeliveryChannel)

instance Data.FromJSON DeliveryChannel where
  parseJSON =
    Data.withObject
      "DeliveryChannel"
      ( \x ->
          DeliveryChannel'
            Prelude.<$> (x Data..:? "configSnapshotDeliveryProperties")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "s3BucketName")
            Prelude.<*> (x Data..:? "s3KeyPrefix")
            Prelude.<*> (x Data..:? "s3KmsKeyArn")
            Prelude.<*> (x Data..:? "snsTopicARN")
      )

instance Prelude.Hashable DeliveryChannel where
  hashWithSalt _salt DeliveryChannel' {..} =
    _salt
      `Prelude.hashWithSalt` configSnapshotDeliveryProperties
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` s3KmsKeyArn
      `Prelude.hashWithSalt` snsTopicARN

instance Prelude.NFData DeliveryChannel where
  rnf DeliveryChannel' {..} =
    Prelude.rnf configSnapshotDeliveryProperties `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf s3BucketName `Prelude.seq`
          Prelude.rnf s3KeyPrefix `Prelude.seq`
            Prelude.rnf s3KmsKeyArn `Prelude.seq`
              Prelude.rnf snsTopicARN

instance Data.ToJSON DeliveryChannel where
  toJSON DeliveryChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("configSnapshotDeliveryProperties" Data..=)
              Prelude.<$> configSnapshotDeliveryProperties,
            ("name" Data..=) Prelude.<$> name,
            ("s3BucketName" Data..=) Prelude.<$> s3BucketName,
            ("s3KeyPrefix" Data..=) Prelude.<$> s3KeyPrefix,
            ("s3KmsKeyArn" Data..=) Prelude.<$> s3KmsKeyArn,
            ("snsTopicARN" Data..=) Prelude.<$> snsTopicARN
          ]
      )
