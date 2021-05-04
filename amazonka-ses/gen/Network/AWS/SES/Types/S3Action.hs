{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SES.Types.S3Action
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.S3Action where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | When included in a receipt rule, this action saves the received message
-- to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally,
-- publishes a notification to Amazon Simple Notification Service (Amazon
-- SNS).
--
-- To enable Amazon SES to write emails to your Amazon S3 bucket, use an
-- AWS KMS key to encrypt your emails, or publish to an Amazon SNS topic of
-- another account, Amazon SES must have permission to access those
-- resources. For information about giving permissions, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
--
-- When you save your emails to an Amazon S3 bucket, the maximum email size
-- (including headers) is 30 MB. Emails larger than that will bounce.
--
-- For information about specifying Amazon S3 actions in receipt rules, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-s3.html Amazon SES Developer Guide>.
--
-- /See:/ 'newS3Action' smart constructor.
data S3Action = S3Action'
  { -- | The key prefix of the Amazon S3 bucket. The key prefix is similar to a
    -- directory name that enables you to store similar data under the same
    -- directory in a bucket.
    objectKeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The customer master key that Amazon SES should use to encrypt your
    -- emails before saving them to the Amazon S3 bucket. You can use the
    -- default master key or a custom master key you created in AWS KMS as
    -- follows:
    --
    -- -   To use the default master key, provide an ARN in the form of
    --     @arn:aws:kms:REGION:ACCOUNT-ID-WITHOUT-HYPHENS:alias\/aws\/ses@. For
    --     example, if your AWS account ID is 123456789012 and you want to use
    --     the default master key in the US West (Oregon) region, the ARN of
    --     the default master key would be
    --     @arn:aws:kms:us-west-2:123456789012:alias\/aws\/ses@. If you use the
    --     default master key, you don\'t need to perform any extra steps to
    --     give Amazon SES permission to use the key.
    --
    -- -   To use a custom master key you created in AWS KMS, provide the ARN
    --     of the master key and ensure that you add a statement to your key\'s
    --     policy to give Amazon SES permission to use it. For more information
    --     about giving permissions, see the
    --     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
    --
    -- For more information about key policies, see the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS KMS Developer Guide>.
    -- If you do not specify a master key, Amazon SES will not encrypt your
    -- emails.
    --
    -- Your mail is encrypted by Amazon SES using the Amazon S3 encryption
    -- client before the mail is submitted to Amazon S3 for storage. It is not
    -- encrypted using Amazon S3 server-side encryption. This means that you
    -- must use the Amazon S3 encryption client to decrypt the email after
    -- retrieving it from Amazon S3, as the service has no access to use your
    -- AWS KMS keys for decryption. This encryption client is currently
    -- available with the
    -- <http://aws.amazon.com/sdk-for-java/ AWS SDK for Java> and
    -- <http://aws.amazon.com/sdk-for-ruby/ AWS SDK for Ruby> only. For more
    -- information about client-side encryption using AWS KMS master keys, see
    -- the
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 Developer Guide>.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon SNS topic to notify when the message is saved to
    -- the Amazon S3 bucket. An example of an Amazon SNS topic ARN is
    -- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
    -- Amazon SNS topics, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket that incoming email will be saved to.
    bucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectKeyPrefix', 's3Action_objectKeyPrefix' - The key prefix of the Amazon S3 bucket. The key prefix is similar to a
-- directory name that enables you to store similar data under the same
-- directory in a bucket.
--
-- 'kmsKeyArn', 's3Action_kmsKeyArn' - The customer master key that Amazon SES should use to encrypt your
-- emails before saving them to the Amazon S3 bucket. You can use the
-- default master key or a custom master key you created in AWS KMS as
-- follows:
--
-- -   To use the default master key, provide an ARN in the form of
--     @arn:aws:kms:REGION:ACCOUNT-ID-WITHOUT-HYPHENS:alias\/aws\/ses@. For
--     example, if your AWS account ID is 123456789012 and you want to use
--     the default master key in the US West (Oregon) region, the ARN of
--     the default master key would be
--     @arn:aws:kms:us-west-2:123456789012:alias\/aws\/ses@. If you use the
--     default master key, you don\'t need to perform any extra steps to
--     give Amazon SES permission to use the key.
--
-- -   To use a custom master key you created in AWS KMS, provide the ARN
--     of the master key and ensure that you add a statement to your key\'s
--     policy to give Amazon SES permission to use it. For more information
--     about giving permissions, see the
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
--
-- For more information about key policies, see the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS KMS Developer Guide>.
-- If you do not specify a master key, Amazon SES will not encrypt your
-- emails.
--
-- Your mail is encrypted by Amazon SES using the Amazon S3 encryption
-- client before the mail is submitted to Amazon S3 for storage. It is not
-- encrypted using Amazon S3 server-side encryption. This means that you
-- must use the Amazon S3 encryption client to decrypt the email after
-- retrieving it from Amazon S3, as the service has no access to use your
-- AWS KMS keys for decryption. This encryption client is currently
-- available with the
-- <http://aws.amazon.com/sdk-for-java/ AWS SDK for Java> and
-- <http://aws.amazon.com/sdk-for-ruby/ AWS SDK for Ruby> only. For more
-- information about client-side encryption using AWS KMS master keys, see
-- the
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 Developer Guide>.
--
-- 'topicArn', 's3Action_topicArn' - The ARN of the Amazon SNS topic to notify when the message is saved to
-- the Amazon S3 bucket. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
--
-- 'bucketName', 's3Action_bucketName' - The name of the Amazon S3 bucket that incoming email will be saved to.
newS3Action ::
  -- | 'bucketName'
  Prelude.Text ->
  S3Action
newS3Action pBucketName_ =
  S3Action'
    { objectKeyPrefix = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      bucketName = pBucketName_
    }

-- | The key prefix of the Amazon S3 bucket. The key prefix is similar to a
-- directory name that enables you to store similar data under the same
-- directory in a bucket.
s3Action_objectKeyPrefix :: Lens.Lens' S3Action (Prelude.Maybe Prelude.Text)
s3Action_objectKeyPrefix = Lens.lens (\S3Action' {objectKeyPrefix} -> objectKeyPrefix) (\s@S3Action' {} a -> s {objectKeyPrefix = a} :: S3Action)

-- | The customer master key that Amazon SES should use to encrypt your
-- emails before saving them to the Amazon S3 bucket. You can use the
-- default master key or a custom master key you created in AWS KMS as
-- follows:
--
-- -   To use the default master key, provide an ARN in the form of
--     @arn:aws:kms:REGION:ACCOUNT-ID-WITHOUT-HYPHENS:alias\/aws\/ses@. For
--     example, if your AWS account ID is 123456789012 and you want to use
--     the default master key in the US West (Oregon) region, the ARN of
--     the default master key would be
--     @arn:aws:kms:us-west-2:123456789012:alias\/aws\/ses@. If you use the
--     default master key, you don\'t need to perform any extra steps to
--     give Amazon SES permission to use the key.
--
-- -   To use a custom master key you created in AWS KMS, provide the ARN
--     of the master key and ensure that you add a statement to your key\'s
--     policy to give Amazon SES permission to use it. For more information
--     about giving permissions, see the
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
--
-- For more information about key policies, see the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS KMS Developer Guide>.
-- If you do not specify a master key, Amazon SES will not encrypt your
-- emails.
--
-- Your mail is encrypted by Amazon SES using the Amazon S3 encryption
-- client before the mail is submitted to Amazon S3 for storage. It is not
-- encrypted using Amazon S3 server-side encryption. This means that you
-- must use the Amazon S3 encryption client to decrypt the email after
-- retrieving it from Amazon S3, as the service has no access to use your
-- AWS KMS keys for decryption. This encryption client is currently
-- available with the
-- <http://aws.amazon.com/sdk-for-java/ AWS SDK for Java> and
-- <http://aws.amazon.com/sdk-for-ruby/ AWS SDK for Ruby> only. For more
-- information about client-side encryption using AWS KMS master keys, see
-- the
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 Developer Guide>.
s3Action_kmsKeyArn :: Lens.Lens' S3Action (Prelude.Maybe Prelude.Text)
s3Action_kmsKeyArn = Lens.lens (\S3Action' {kmsKeyArn} -> kmsKeyArn) (\s@S3Action' {} a -> s {kmsKeyArn = a} :: S3Action)

-- | The ARN of the Amazon SNS topic to notify when the message is saved to
-- the Amazon S3 bucket. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
s3Action_topicArn :: Lens.Lens' S3Action (Prelude.Maybe Prelude.Text)
s3Action_topicArn = Lens.lens (\S3Action' {topicArn} -> topicArn) (\s@S3Action' {} a -> s {topicArn = a} :: S3Action)

-- | The name of the Amazon S3 bucket that incoming email will be saved to.
s3Action_bucketName :: Lens.Lens' S3Action Prelude.Text
s3Action_bucketName = Lens.lens (\S3Action' {bucketName} -> bucketName) (\s@S3Action' {} a -> s {bucketName = a} :: S3Action)

instance Prelude.FromXML S3Action where
  parseXML x =
    S3Action'
      Prelude.<$> (x Prelude..@? "ObjectKeyPrefix")
      Prelude.<*> (x Prelude..@? "KmsKeyArn")
      Prelude.<*> (x Prelude..@? "TopicArn")
      Prelude.<*> (x Prelude..@ "BucketName")

instance Prelude.Hashable S3Action

instance Prelude.NFData S3Action

instance Prelude.ToQuery S3Action where
  toQuery S3Action' {..} =
    Prelude.mconcat
      [ "ObjectKeyPrefix" Prelude.=: objectKeyPrefix,
        "KmsKeyArn" Prelude.=: kmsKeyArn,
        "TopicArn" Prelude.=: topicArn,
        "BucketName" Prelude.=: bucketName
      ]
