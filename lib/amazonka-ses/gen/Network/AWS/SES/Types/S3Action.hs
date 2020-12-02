{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.S3Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.S3Action where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When included in a receipt rule, this action saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
--
-- To enable Amazon SES to write emails to your Amazon S3 bucket, use an AWS KMS key to encrypt your emails, or publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
--
-- For information about specifying Amazon S3 actions in receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-s3.html Amazon SES Developer Guide> .
--
--
-- /See:/ 's3Action' smart constructor.
data S3Action = S3Action'
  { _s3KMSKeyARN :: !(Maybe Text),
    _s3TopicARN :: !(Maybe Text),
    _s3ObjectKeyPrefix :: !(Maybe Text),
    _s3BucketName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 's3KMSKeyARN' - The customer master key that Amazon SES should use to encrypt your emails before saving them to the Amazon S3 bucket. You can use the default master key or a custom master key you created in AWS KMS as follows:     * To use the default master key, provide an ARN in the form of @arn:aws:kms:REGION:ACCOUNT-ID-WITHOUT-HYPHENS:alias/aws/ses@ . For example, if your AWS account ID is 123456789012 and you want to use the default master key in the US West (Oregon) region, the ARN of the default master key would be @arn:aws:kms:us-west-2:123456789012:alias/aws/ses@ . If you use the default master key, you don't need to perform any extra steps to give Amazon SES permission to use the key.     * To use a custom master key you created in AWS KMS, provide the ARN of the master key and ensure that you add a statement to your key's policy to give Amazon SES permission to use it. For more information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> . For more information about key policies, see the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS KMS Developer Guide> . If you do not specify a master key, Amazon SES will not encrypt your emails. /Important:/ Your mail is encrypted by Amazon SES using the Amazon S3 encryption client before the mail is submitted to Amazon S3 for storage. It is not encrypted using Amazon S3 server-side encryption. This means that you must use the Amazon S3 encryption client to decrypt the email after retrieving it from Amazon S3, as the service has no access to use your AWS KMS keys for decryption. This encryption client is currently available with the <http://aws.amazon.com/sdk-for-java/ AWS SDK for Java> and <http://aws.amazon.com/sdk-for-ruby/ AWS SDK for Ruby> only. For more information about client-side encryption using AWS KMS master keys, see the <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 Developer Guide> .
--
-- * 's3TopicARN' - The ARN of the Amazon SNS topic to notify when the message is saved to the Amazon S3 bucket. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- * 's3ObjectKeyPrefix' - The key prefix of the Amazon S3 bucket. The key prefix is similar to a directory name that enables you to store similar data under the same directory in a bucket.
--
-- * 's3BucketName' - The name of the Amazon S3 bucket that incoming email will be saved to.
s3Action ::
  -- | 's3BucketName'
  Text ->
  S3Action
s3Action pBucketName_ =
  S3Action'
    { _s3KMSKeyARN = Nothing,
      _s3TopicARN = Nothing,
      _s3ObjectKeyPrefix = Nothing,
      _s3BucketName = pBucketName_
    }

-- | The customer master key that Amazon SES should use to encrypt your emails before saving them to the Amazon S3 bucket. You can use the default master key or a custom master key you created in AWS KMS as follows:     * To use the default master key, provide an ARN in the form of @arn:aws:kms:REGION:ACCOUNT-ID-WITHOUT-HYPHENS:alias/aws/ses@ . For example, if your AWS account ID is 123456789012 and you want to use the default master key in the US West (Oregon) region, the ARN of the default master key would be @arn:aws:kms:us-west-2:123456789012:alias/aws/ses@ . If you use the default master key, you don't need to perform any extra steps to give Amazon SES permission to use the key.     * To use a custom master key you created in AWS KMS, provide the ARN of the master key and ensure that you add a statement to your key's policy to give Amazon SES permission to use it. For more information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> . For more information about key policies, see the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS KMS Developer Guide> . If you do not specify a master key, Amazon SES will not encrypt your emails. /Important:/ Your mail is encrypted by Amazon SES using the Amazon S3 encryption client before the mail is submitted to Amazon S3 for storage. It is not encrypted using Amazon S3 server-side encryption. This means that you must use the Amazon S3 encryption client to decrypt the email after retrieving it from Amazon S3, as the service has no access to use your AWS KMS keys for decryption. This encryption client is currently available with the <http://aws.amazon.com/sdk-for-java/ AWS SDK for Java> and <http://aws.amazon.com/sdk-for-ruby/ AWS SDK for Ruby> only. For more information about client-side encryption using AWS KMS master keys, see the <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 Developer Guide> .
s3KMSKeyARN :: Lens' S3Action (Maybe Text)
s3KMSKeyARN = lens _s3KMSKeyARN (\s a -> s {_s3KMSKeyARN = a})

-- | The ARN of the Amazon SNS topic to notify when the message is saved to the Amazon S3 bucket. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
s3TopicARN :: Lens' S3Action (Maybe Text)
s3TopicARN = lens _s3TopicARN (\s a -> s {_s3TopicARN = a})

-- | The key prefix of the Amazon S3 bucket. The key prefix is similar to a directory name that enables you to store similar data under the same directory in a bucket.
s3ObjectKeyPrefix :: Lens' S3Action (Maybe Text)
s3ObjectKeyPrefix = lens _s3ObjectKeyPrefix (\s a -> s {_s3ObjectKeyPrefix = a})

-- | The name of the Amazon S3 bucket that incoming email will be saved to.
s3BucketName :: Lens' S3Action Text
s3BucketName = lens _s3BucketName (\s a -> s {_s3BucketName = a})

instance FromXML S3Action where
  parseXML x =
    S3Action'
      <$> (x .@? "KmsKeyArn")
      <*> (x .@? "TopicArn")
      <*> (x .@? "ObjectKeyPrefix")
      <*> (x .@ "BucketName")

instance Hashable S3Action

instance NFData S3Action

instance ToQuery S3Action where
  toQuery S3Action' {..} =
    mconcat
      [ "KmsKeyArn" =: _s3KMSKeyARN,
        "TopicArn" =: _s3TopicARN,
        "ObjectKeyPrefix" =: _s3ObjectKeyPrefix,
        "BucketName" =: _s3BucketName
      ]
