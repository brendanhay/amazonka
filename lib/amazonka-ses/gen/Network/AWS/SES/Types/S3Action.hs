{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.S3Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.S3Action
  ( S3Action (..)
  -- * Smart constructor
  , mkS3Action
  -- * Lenses
  , saBucketName
  , saKmsKeyArn
  , saObjectKeyPrefix
  , saTopicArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.AmazonResourceName as Types
import qualified Network.AWS.SES.Types.BucketName as Types
import qualified Network.AWS.SES.Types.S3KeyPrefix as Types

-- | When included in a receipt rule, this action saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
-- To enable Amazon SES to write emails to your Amazon S3 bucket, use an AWS KMS key to encrypt your emails, or publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
-- For information about specifying Amazon S3 actions in receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-s3.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkS3Action' smart constructor.
data S3Action = S3Action'
  { bucketName :: Types.BucketName
    -- ^ The name of the Amazon S3 bucket that incoming email will be saved to.
  , kmsKeyArn :: Core.Maybe Types.AmazonResourceName
    -- ^ The customer master key that Amazon SES should use to encrypt your emails before saving them to the Amazon S3 bucket. You can use the default master key or a custom master key you created in AWS KMS as follows:
--
--
--     * To use the default master key, provide an ARN in the form of @arn:aws:kms:REGION:ACCOUNT-ID-WITHOUT-HYPHENS:alias/aws/ses@ . For example, if your AWS account ID is 123456789012 and you want to use the default master key in the US West (Oregon) region, the ARN of the default master key would be @arn:aws:kms:us-west-2:123456789012:alias/aws/ses@ . If you use the default master key, you don't need to perform any extra steps to give Amazon SES permission to use the key.
--
--
--     * To use a custom master key you created in AWS KMS, provide the ARN of the master key and ensure that you add a statement to your key's policy to give Amazon SES permission to use it. For more information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
--
--
-- For more information about key policies, see the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS KMS Developer Guide> . If you do not specify a master key, Amazon SES will not encrypt your emails.
-- /Important:/ Your mail is encrypted by Amazon SES using the Amazon S3 encryption client before the mail is submitted to Amazon S3 for storage. It is not encrypted using Amazon S3 server-side encryption. This means that you must use the Amazon S3 encryption client to decrypt the email after retrieving it from Amazon S3, as the service has no access to use your AWS KMS keys for decryption. This encryption client is currently available with the <http://aws.amazon.com/sdk-for-java/ AWS SDK for Java> and <http://aws.amazon.com/sdk-for-ruby/ AWS SDK for Ruby> only. For more information about client-side encryption using AWS KMS master keys, see the <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 Developer Guide> .
  , objectKeyPrefix :: Core.Maybe Types.S3KeyPrefix
    -- ^ The key prefix of the Amazon S3 bucket. The key prefix is similar to a directory name that enables you to store similar data under the same directory in a bucket.
  , topicArn :: Core.Maybe Types.AmazonResourceName
    -- ^ The ARN of the Amazon SNS topic to notify when the message is saved to the Amazon S3 bucket. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Action' value with any optional fields omitted.
mkS3Action
    :: Types.BucketName -- ^ 'bucketName'
    -> S3Action
mkS3Action bucketName
  = S3Action'{bucketName, kmsKeyArn = Core.Nothing,
              objectKeyPrefix = Core.Nothing, topicArn = Core.Nothing}

-- | The name of the Amazon S3 bucket that incoming email will be saved to.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saBucketName :: Lens.Lens' S3Action Types.BucketName
saBucketName = Lens.field @"bucketName"
{-# INLINEABLE saBucketName #-}
{-# DEPRECATED bucketName "Use generic-lens or generic-optics with 'bucketName' instead"  #-}

-- | The customer master key that Amazon SES should use to encrypt your emails before saving them to the Amazon S3 bucket. You can use the default master key or a custom master key you created in AWS KMS as follows:
--
--
--     * To use the default master key, provide an ARN in the form of @arn:aws:kms:REGION:ACCOUNT-ID-WITHOUT-HYPHENS:alias/aws/ses@ . For example, if your AWS account ID is 123456789012 and you want to use the default master key in the US West (Oregon) region, the ARN of the default master key would be @arn:aws:kms:us-west-2:123456789012:alias/aws/ses@ . If you use the default master key, you don't need to perform any extra steps to give Amazon SES permission to use the key.
--
--
--     * To use a custom master key you created in AWS KMS, provide the ARN of the master key and ensure that you add a statement to your key's policy to give Amazon SES permission to use it. For more information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
--
--
-- For more information about key policies, see the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS KMS Developer Guide> . If you do not specify a master key, Amazon SES will not encrypt your emails.
-- /Important:/ Your mail is encrypted by Amazon SES using the Amazon S3 encryption client before the mail is submitted to Amazon S3 for storage. It is not encrypted using Amazon S3 server-side encryption. This means that you must use the Amazon S3 encryption client to decrypt the email after retrieving it from Amazon S3, as the service has no access to use your AWS KMS keys for decryption. This encryption client is currently available with the <http://aws.amazon.com/sdk-for-java/ AWS SDK for Java> and <http://aws.amazon.com/sdk-for-ruby/ AWS SDK for Ruby> only. For more information about client-side encryption using AWS KMS master keys, see the <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 Developer Guide> .
--
-- /Note:/ Consider using 'kmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saKmsKeyArn :: Lens.Lens' S3Action (Core.Maybe Types.AmazonResourceName)
saKmsKeyArn = Lens.field @"kmsKeyArn"
{-# INLINEABLE saKmsKeyArn #-}
{-# DEPRECATED kmsKeyArn "Use generic-lens or generic-optics with 'kmsKeyArn' instead"  #-}

-- | The key prefix of the Amazon S3 bucket. The key prefix is similar to a directory name that enables you to store similar data under the same directory in a bucket.
--
-- /Note:/ Consider using 'objectKeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saObjectKeyPrefix :: Lens.Lens' S3Action (Core.Maybe Types.S3KeyPrefix)
saObjectKeyPrefix = Lens.field @"objectKeyPrefix"
{-# INLINEABLE saObjectKeyPrefix #-}
{-# DEPRECATED objectKeyPrefix "Use generic-lens or generic-optics with 'objectKeyPrefix' instead"  #-}

-- | The ARN of the Amazon SNS topic to notify when the message is saved to the Amazon S3 bucket. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saTopicArn :: Lens.Lens' S3Action (Core.Maybe Types.AmazonResourceName)
saTopicArn = Lens.field @"topicArn"
{-# INLINEABLE saTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

instance Core.ToQuery S3Action where
        toQuery S3Action{..}
          = Core.toQueryPair "BucketName" bucketName Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyArn") kmsKeyArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ObjectKeyPrefix")
                objectKeyPrefix
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TopicArn") topicArn

instance Core.FromXML S3Action where
        parseXML x
          = S3Action' Core.<$>
              (x Core..@ "BucketName") Core.<*> x Core..@? "KmsKeyArn" Core.<*>
                x Core..@? "ObjectKeyPrefix"
                Core.<*> x Core..@? "TopicArn"
