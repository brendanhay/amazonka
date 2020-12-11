-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Pipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Pipeline
  ( Pipeline (..),

    -- * Smart constructor
    mkPipeline,

    -- * Lenses
    pipStatus,
    pipARN,
    pipInputBucket,
    pipContentConfig,
    pipOutputBucket,
    pipRole,
    pipName,
    pipAWSKMSKeyARN,
    pipId,
    pipNotifications,
    pipThumbnailConfig,
  )
where

import Network.AWS.ElasticTranscoder.Types.Notifications
import Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The pipeline (queue) that is used to manage jobs.
--
-- /See:/ 'mkPipeline' smart constructor.
data Pipeline = Pipeline'
  { status :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    inputBucket :: Lude.Maybe Lude.Text,
    contentConfig :: Lude.Maybe PipelineOutputConfig,
    outputBucket :: Lude.Maybe Lude.Text,
    role' :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    awsKMSKeyARN :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    notifications :: Lude.Maybe Notifications,
    thumbnailConfig :: Lude.Maybe PipelineOutputConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Pipeline' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) for the pipeline.
-- * 'awsKMSKeyARN' - The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
-- * 'contentConfig' - Information about the Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .
--
--
--     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists.
--
--
--     * __Permissions__ : A list of the users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access that you want them to have.
--
--     * GranteeType: The type of value that appears in the @Grantee@ object:
--
--     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution.
--
--
--     * @Email@ : The registered email address of an AWS account.
--
--
--     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
--
--
--
--
--     * @Grantee@ : The AWS user or group that you want to have access to transcoded files and playlists.
--
--
--     * @Access@ : The permission that you want to give to the AWS user that is listed in @Grantee@ . Valid values include:
--
--     * @READ@ : The grantee can read the objects and metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @READ_ACP@ : The grantee can read the object ACL for objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @WRITE_ACP@ : The grantee can write the ACL for the objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @FULL_CONTROL@ : The grantee has @READ@ , @READ_ACP@ , and @WRITE_ACP@ permissions for the objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--
--
--
--
--     * __StorageClass__ : The Amazon S3 storage class, Standard or ReducedRedundancy, that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
--
--
-- * 'id' - The identifier for the pipeline. You use this value to identify the pipeline in which you want to perform a variety of operations, such as creating a job or a preset.
-- * 'inputBucket' - The Amazon S3 bucket from which Elastic Transcoder gets media files for transcoding and the graphics files, if any, that you want to use for watermarks.
-- * 'name' - The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
-- * 'notifications' - The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status.
--
-- /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.
--
--     * __Progressing__ (optional): The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.
--
--
--     * __Complete__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.
--
--
--     * __Warning__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.
--
--
--     * __Error__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
--
--
-- * 'outputBucket' - The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files, thumbnails, and playlists. Either you specify this value, or you specify both @ContentConfig@ and @ThumbnailConfig@ .
-- * 'role'' - The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder uses to transcode jobs for this pipeline.
-- * 'status' - The current status of the pipeline:
--
--
--     * @Active@ : The pipeline is processing jobs.
--
--
--     * @Paused@ : The pipeline is not currently processing jobs.
--
--
-- * 'thumbnailConfig' - Information about the Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .
--
--
--     * @Bucket@ : The Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files.
--
--
--     * @Permissions@ : A list of the users and/or predefined Amazon S3 groups you want to have access to thumbnail files, and the type of access that you want them to have.
--
--     * GranteeType: The type of value that appears in the Grantee object:
--
--     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution.
-- /Important:/ A canonical user ID is not the same as an AWS account number.
--
--
--     * @Email@ : The registered email address of an AWS account.
--
--
--     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
--
--
--
--
--     * @Grantee@ : The AWS user or group that you want to have access to thumbnail files.
--
--
--     * Access: The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:
--
--     * @READ@ : The grantee can read the thumbnails and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @FULL_CONTROL@ : The grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--
--
--
--
--     * @StorageClass@ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the thumbnails that it stores in your Amazon S3 bucket.
mkPipeline ::
  Pipeline
mkPipeline =
  Pipeline'
    { status = Lude.Nothing,
      arn = Lude.Nothing,
      inputBucket = Lude.Nothing,
      contentConfig = Lude.Nothing,
      outputBucket = Lude.Nothing,
      role' = Lude.Nothing,
      name = Lude.Nothing,
      awsKMSKeyARN = Lude.Nothing,
      id = Lude.Nothing,
      notifications = Lude.Nothing,
      thumbnailConfig = Lude.Nothing
    }

-- | The current status of the pipeline:
--
--
--     * @Active@ : The pipeline is processing jobs.
--
--
--     * @Paused@ : The pipeline is not currently processing jobs.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipStatus :: Lens.Lens' Pipeline (Lude.Maybe Lude.Text)
pipStatus = Lens.lens (status :: Pipeline -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Pipeline)
{-# DEPRECATED pipStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) for the pipeline.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipARN :: Lens.Lens' Pipeline (Lude.Maybe Lude.Text)
pipARN = Lens.lens (arn :: Pipeline -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Pipeline)
{-# DEPRECATED pipARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The Amazon S3 bucket from which Elastic Transcoder gets media files for transcoding and the graphics files, if any, that you want to use for watermarks.
--
-- /Note:/ Consider using 'inputBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipInputBucket :: Lens.Lens' Pipeline (Lude.Maybe Lude.Text)
pipInputBucket = Lens.lens (inputBucket :: Pipeline -> Lude.Maybe Lude.Text) (\s a -> s {inputBucket = a} :: Pipeline)
{-# DEPRECATED pipInputBucket "Use generic-lens or generic-optics with 'inputBucket' instead." #-}

-- | Information about the Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .
--
--
--     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists.
--
--
--     * __Permissions__ : A list of the users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access that you want them to have.
--
--     * GranteeType: The type of value that appears in the @Grantee@ object:
--
--     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution.
--
--
--     * @Email@ : The registered email address of an AWS account.
--
--
--     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
--
--
--
--
--     * @Grantee@ : The AWS user or group that you want to have access to transcoded files and playlists.
--
--
--     * @Access@ : The permission that you want to give to the AWS user that is listed in @Grantee@ . Valid values include:
--
--     * @READ@ : The grantee can read the objects and metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @READ_ACP@ : The grantee can read the object ACL for objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @WRITE_ACP@ : The grantee can write the ACL for the objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @FULL_CONTROL@ : The grantee has @READ@ , @READ_ACP@ , and @WRITE_ACP@ permissions for the objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--
--
--
--
--     * __StorageClass__ : The Amazon S3 storage class, Standard or ReducedRedundancy, that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
--
--
--
-- /Note:/ Consider using 'contentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipContentConfig :: Lens.Lens' Pipeline (Lude.Maybe PipelineOutputConfig)
pipContentConfig = Lens.lens (contentConfig :: Pipeline -> Lude.Maybe PipelineOutputConfig) (\s a -> s {contentConfig = a} :: Pipeline)
{-# DEPRECATED pipContentConfig "Use generic-lens or generic-optics with 'contentConfig' instead." #-}

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files, thumbnails, and playlists. Either you specify this value, or you specify both @ContentConfig@ and @ThumbnailConfig@ .
--
-- /Note:/ Consider using 'outputBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipOutputBucket :: Lens.Lens' Pipeline (Lude.Maybe Lude.Text)
pipOutputBucket = Lens.lens (outputBucket :: Pipeline -> Lude.Maybe Lude.Text) (\s a -> s {outputBucket = a} :: Pipeline)
{-# DEPRECATED pipOutputBucket "Use generic-lens or generic-optics with 'outputBucket' instead." #-}

-- | The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder uses to transcode jobs for this pipeline.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipRole :: Lens.Lens' Pipeline (Lude.Maybe Lude.Text)
pipRole = Lens.lens (role' :: Pipeline -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: Pipeline)
{-# DEPRECATED pipRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipName :: Lens.Lens' Pipeline (Lude.Maybe Lude.Text)
pipName = Lens.lens (name :: Pipeline -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Pipeline)
{-# DEPRECATED pipName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
--
-- /Note:/ Consider using 'awsKMSKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipAWSKMSKeyARN :: Lens.Lens' Pipeline (Lude.Maybe Lude.Text)
pipAWSKMSKeyARN = Lens.lens (awsKMSKeyARN :: Pipeline -> Lude.Maybe Lude.Text) (\s a -> s {awsKMSKeyARN = a} :: Pipeline)
{-# DEPRECATED pipAWSKMSKeyARN "Use generic-lens or generic-optics with 'awsKMSKeyARN' instead." #-}

-- | The identifier for the pipeline. You use this value to identify the pipeline in which you want to perform a variety of operations, such as creating a job or a preset.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipId :: Lens.Lens' Pipeline (Lude.Maybe Lude.Text)
pipId = Lens.lens (id :: Pipeline -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Pipeline)
{-# DEPRECATED pipId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status.
--
-- /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.
--
--     * __Progressing__ (optional): The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.
--
--
--     * __Complete__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.
--
--
--     * __Warning__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.
--
--
--     * __Error__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
--
--
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipNotifications :: Lens.Lens' Pipeline (Lude.Maybe Notifications)
pipNotifications = Lens.lens (notifications :: Pipeline -> Lude.Maybe Notifications) (\s a -> s {notifications = a} :: Pipeline)
{-# DEPRECATED pipNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

-- | Information about the Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .
--
--
--     * @Bucket@ : The Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files.
--
--
--     * @Permissions@ : A list of the users and/or predefined Amazon S3 groups you want to have access to thumbnail files, and the type of access that you want them to have.
--
--     * GranteeType: The type of value that appears in the Grantee object:
--
--     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution.
-- /Important:/ A canonical user ID is not the same as an AWS account number.
--
--
--     * @Email@ : The registered email address of an AWS account.
--
--
--     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
--
--
--
--
--     * @Grantee@ : The AWS user or group that you want to have access to thumbnail files.
--
--
--     * Access: The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:
--
--     * @READ@ : The grantee can read the thumbnails and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @FULL_CONTROL@ : The grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--
--
--
--
--     * @StorageClass@ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the thumbnails that it stores in your Amazon S3 bucket.
--
--
--
-- /Note:/ Consider using 'thumbnailConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipThumbnailConfig :: Lens.Lens' Pipeline (Lude.Maybe PipelineOutputConfig)
pipThumbnailConfig = Lens.lens (thumbnailConfig :: Pipeline -> Lude.Maybe PipelineOutputConfig) (\s a -> s {thumbnailConfig = a} :: Pipeline)
{-# DEPRECATED pipThumbnailConfig "Use generic-lens or generic-optics with 'thumbnailConfig' instead." #-}

instance Lude.FromJSON Pipeline where
  parseJSON =
    Lude.withObject
      "Pipeline"
      ( \x ->
          Pipeline'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "InputBucket")
            Lude.<*> (x Lude..:? "ContentConfig")
            Lude.<*> (x Lude..:? "OutputBucket")
            Lude.<*> (x Lude..:? "Role")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "AwsKmsKeyArn")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Notifications")
            Lude.<*> (x Lude..:? "ThumbnailConfig")
      )
