{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Pipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.Pipeline
  ( Pipeline (..)
  -- * Smart constructor
  , mkPipeline
  -- * Lenses
  , pfArn
  , pfAwsKmsKeyArn
  , pfContentConfig
  , pfId
  , pfInputBucket
  , pfName
  , pfNotifications
  , pfOutputBucket
  , pfRole
  , pfStatus
  , pfThumbnailConfig
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.BucketName as Types
import qualified Network.AWS.ElasticTranscoder.Types.Id as Types
import qualified Network.AWS.ElasticTranscoder.Types.KeyArn as Types
import qualified Network.AWS.ElasticTranscoder.Types.Name as Types
import qualified Network.AWS.ElasticTranscoder.Types.Notifications as Types
import qualified Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig as Types
import qualified Network.AWS.ElasticTranscoder.Types.PipelineStatus as Types
import qualified Network.AWS.ElasticTranscoder.Types.Role as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The pipeline (queue) that is used to manage jobs.
--
-- /See:/ 'mkPipeline' smart constructor.
data Pipeline = Pipeline'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the pipeline.
  , awsKmsKeyArn :: Core.Maybe Types.KeyArn
    -- ^ The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
  , contentConfig :: Core.Maybe Types.PipelineOutputConfig
    -- ^ Information about the Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .
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
  , id :: Core.Maybe Types.Id
    -- ^ The identifier for the pipeline. You use this value to identify the pipeline in which you want to perform a variety of operations, such as creating a job or a preset.
  , inputBucket :: Core.Maybe Types.BucketName
    -- ^ The Amazon S3 bucket from which Elastic Transcoder gets media files for transcoding and the graphics files, if any, that you want to use for watermarks.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
  , notifications :: Core.Maybe Types.Notifications
    -- ^ The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status.
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
  , outputBucket :: Core.Maybe Types.BucketName
    -- ^ The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files, thumbnails, and playlists. Either you specify this value, or you specify both @ContentConfig@ and @ThumbnailConfig@ .
  , role' :: Core.Maybe Types.Role
    -- ^ The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder uses to transcode jobs for this pipeline.
  , status :: Core.Maybe Types.PipelineStatus
    -- ^ The current status of the pipeline:
--
--
--     * @Active@ : The pipeline is processing jobs.
--
--
--     * @Paused@ : The pipeline is not currently processing jobs.
--
--
  , thumbnailConfig :: Core.Maybe Types.PipelineOutputConfig
    -- ^ Information about the Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Pipeline' value with any optional fields omitted.
mkPipeline
    :: Pipeline
mkPipeline
  = Pipeline'{arn = Core.Nothing, awsKmsKeyArn = Core.Nothing,
              contentConfig = Core.Nothing, id = Core.Nothing,
              inputBucket = Core.Nothing, name = Core.Nothing,
              notifications = Core.Nothing, outputBucket = Core.Nothing,
              role' = Core.Nothing, status = Core.Nothing,
              thumbnailConfig = Core.Nothing}

-- | The Amazon Resource Name (ARN) for the pipeline.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfArn :: Lens.Lens' Pipeline (Core.Maybe Core.Text)
pfArn = Lens.field @"arn"
{-# INLINEABLE pfArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
--
-- /Note:/ Consider using 'awsKmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfAwsKmsKeyArn :: Lens.Lens' Pipeline (Core.Maybe Types.KeyArn)
pfAwsKmsKeyArn = Lens.field @"awsKmsKeyArn"
{-# INLINEABLE pfAwsKmsKeyArn #-}
{-# DEPRECATED awsKmsKeyArn "Use generic-lens or generic-optics with 'awsKmsKeyArn' instead"  #-}

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
pfContentConfig :: Lens.Lens' Pipeline (Core.Maybe Types.PipelineOutputConfig)
pfContentConfig = Lens.field @"contentConfig"
{-# INLINEABLE pfContentConfig #-}
{-# DEPRECATED contentConfig "Use generic-lens or generic-optics with 'contentConfig' instead"  #-}

-- | The identifier for the pipeline. You use this value to identify the pipeline in which you want to perform a variety of operations, such as creating a job or a preset.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfId :: Lens.Lens' Pipeline (Core.Maybe Types.Id)
pfId = Lens.field @"id"
{-# INLINEABLE pfId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The Amazon S3 bucket from which Elastic Transcoder gets media files for transcoding and the graphics files, if any, that you want to use for watermarks.
--
-- /Note:/ Consider using 'inputBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfInputBucket :: Lens.Lens' Pipeline (Core.Maybe Types.BucketName)
pfInputBucket = Lens.field @"inputBucket"
{-# INLINEABLE pfInputBucket #-}
{-# DEPRECATED inputBucket "Use generic-lens or generic-optics with 'inputBucket' instead"  #-}

-- | The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfName :: Lens.Lens' Pipeline (Core.Maybe Types.Name)
pfName = Lens.field @"name"
{-# INLINEABLE pfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

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
pfNotifications :: Lens.Lens' Pipeline (Core.Maybe Types.Notifications)
pfNotifications = Lens.field @"notifications"
{-# INLINEABLE pfNotifications #-}
{-# DEPRECATED notifications "Use generic-lens or generic-optics with 'notifications' instead"  #-}

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files, thumbnails, and playlists. Either you specify this value, or you specify both @ContentConfig@ and @ThumbnailConfig@ .
--
-- /Note:/ Consider using 'outputBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfOutputBucket :: Lens.Lens' Pipeline (Core.Maybe Types.BucketName)
pfOutputBucket = Lens.field @"outputBucket"
{-# INLINEABLE pfOutputBucket #-}
{-# DEPRECATED outputBucket "Use generic-lens or generic-optics with 'outputBucket' instead"  #-}

-- | The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder uses to transcode jobs for this pipeline.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfRole :: Lens.Lens' Pipeline (Core.Maybe Types.Role)
pfRole = Lens.field @"role'"
{-# INLINEABLE pfRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

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
pfStatus :: Lens.Lens' Pipeline (Core.Maybe Types.PipelineStatus)
pfStatus = Lens.field @"status"
{-# INLINEABLE pfStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

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
pfThumbnailConfig :: Lens.Lens' Pipeline (Core.Maybe Types.PipelineOutputConfig)
pfThumbnailConfig = Lens.field @"thumbnailConfig"
{-# INLINEABLE pfThumbnailConfig #-}
{-# DEPRECATED thumbnailConfig "Use generic-lens or generic-optics with 'thumbnailConfig' instead"  #-}

instance Core.FromJSON Pipeline where
        parseJSON
          = Core.withObject "Pipeline" Core.$
              \ x ->
                Pipeline' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "AwsKmsKeyArn" Core.<*>
                    x Core..:? "ContentConfig"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "InputBucket"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Notifications"
                    Core.<*> x Core..:? "OutputBucket"
                    Core.<*> x Core..:? "Role"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "ThumbnailConfig"
