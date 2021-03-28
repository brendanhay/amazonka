{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.CreatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CreatePipeline operation creates a pipeline with settings that you specify.
module Network.AWS.ElasticTranscoder.CreatePipeline
    (
    -- * Creating a request
      CreatePipeline (..)
    , mkCreatePipeline
    -- ** Request lenses
    , cName
    , cInputBucket
    , cRole
    , cAwsKmsKeyArn
    , cContentConfig
    , cNotifications
    , cOutputBucket
    , cThumbnailConfig

    -- * Destructuring the response
    , CreatePipelineResponse (..)
    , mkCreatePipelineResponse
    -- ** Response lenses
    , crsPipeline
    , crsWarnings
    , crsResponseStatus
    ) where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @CreatePipelineRequest@ structure.
--
-- /See:/ 'mkCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { name :: Types.Name
    -- ^ The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters.
  , inputBucket :: Types.BucketName
    -- ^ The Amazon S3 bucket in which you saved the media files that you want to transcode.
  , role' :: Types.Role
    -- ^ The IAM Amazon Resource Name (ARN) for the role that you want Elastic Transcoder to use to create the pipeline.
  , awsKmsKeyArn :: Core.Maybe Types.KeyArn
    -- ^ The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
  , contentConfig :: Core.Maybe Types.PipelineOutputConfig
    -- ^ The optional @ContentConfig@ object specifies information about the Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists: which bucket to use, which users you want to have access to the files, the type of access you want users to have, and the storage class that you want to assign to the files.
--
-- If you specify values for @ContentConfig@ , you must also specify values for @ThumbnailConfig@ .
-- If you specify values for @ContentConfig@ and @ThumbnailConfig@ , omit the @OutputBucket@ object.
--
--     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists.
--
--
--     * __Permissions__ (Optional): The Permissions object specifies which users you want to have access to transcoded files and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups.
--
--
--     * __Grantee Type__ : Specify the type of value that appears in the @Grantee@ object: 
--
--     * __Canonical__ : The value in the @Grantee@ object is either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. For more information about canonical user IDs, see Access Control List (ACL) Overview in the Amazon Simple Storage Service Developer Guide. For more information about using CloudFront origin access identities to require that users use CloudFront URLs instead of Amazon S3 URLs, see Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content.
-- /Important:/ A canonical user ID is not the same as an AWS account number.
--
--
--     * __Email__ : The value in the @Grantee@ object is the registered email address of an AWS account.
--
--
--     * __Group__ : The value in the @Grantee@ object is one of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
--
--
--
--
--     * __Grantee__ : The AWS user or group that you want to have access to transcoded files and playlists. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group 
--
--
--     * __Access__ : The permission that you want to give to the AWS user that you specified in @Grantee@ . Permissions are granted on the files that Elastic Transcoder adds to the bucket, including playlists and video files. Valid values include: 
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
--     * __StorageClass__ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
--
--
  , notifications :: Core.Maybe Types.Notifications
    -- ^ The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status.
--
-- /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.
--
--     * __Progressing__ : The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic. For more information, see Create a Topic in the Amazon Simple Notification Service Developer Guide.
--
--
--     * __Complete__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Warning__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition while processing a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Error__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition while processing a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic.
--
--
  , outputBucket :: Core.Maybe Types.BucketName
    -- ^ The Amazon S3 bucket in which you want Elastic Transcoder to save the transcoded files. (Use this, or use ContentConfig:Bucket plus ThumbnailConfig:Bucket.)
--
-- Specify this value when all of the following are true:
--
--     * You want to save transcoded files, thumbnails (if any), and playlists (if any) together in one bucket.
--
--
--     * You do not want to specify the users or groups who have access to the transcoded files, thumbnails, and playlists.
--
--
--     * You do not want to specify the permissions that Elastic Transcoder grants to the files. 
-- /Important:/ When Elastic Transcoder saves files in @OutputBucket@ , it grants full control over the files only to the AWS account that owns the role that is specified by @Role@ .
--
--
--     * You want to associate the transcoded files and thumbnails with the Amazon S3 Standard storage class.
--
--
-- If you want to save transcoded files and playlists in one bucket and thumbnails in another bucket, specify which users can access the transcoded files or the permissions the users have, or change the Amazon S3 storage class, omit @OutputBucket@ and specify values for @ContentConfig@ and @ThumbnailConfig@ instead.
  , thumbnailConfig :: Core.Maybe Types.PipelineOutputConfig
    -- ^ The @ThumbnailConfig@ object specifies several values, including the Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files, which users you want to have access to the files, the type of access you want users to have, and the storage class that you want to assign to the files.
--
-- If you specify values for @ContentConfig@ , you must also specify values for @ThumbnailConfig@ even if you don't want to create thumbnails.
-- If you specify values for @ContentConfig@ and @ThumbnailConfig@ , omit the @OutputBucket@ object.
--
--     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files.
--
--
--     * __Permissions__ (Optional): The @Permissions@ object specifies which users and/or predefined Amazon S3 groups you want to have access to thumbnail files, and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups.
--
--
--     * __GranteeType__ : Specify the type of value that appears in the Grantee object: 
--
--     * __Canonical__ : The value in the @Grantee@ object is either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution.
-- /Important:/ A canonical user ID is not the same as an AWS account number.
--
--
--     * __Email__ : The value in the @Grantee@ object is the registered email address of an AWS account. 
--
--
--     * __Group__ : The value in the @Grantee@ object is one of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
--
--
--
--
--     * __Grantee__ : The AWS user or group that you want to have access to thumbnail files. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group. 
--
--
--     * __Access__ : The permission that you want to give to the AWS user that you specified in @Grantee@ . Permissions are granted on the thumbnail files that Elastic Transcoder adds to the bucket. Valid values include: 
--
--     * @READ@ : The grantee can read the thumbnails and metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @FULL_CONTROL@ : The grantee has @READ@ , @READ_ACP@ , and @WRITE_ACP@ permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--
--
--     * __StorageClass__ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the thumbnails that it stores in your Amazon S3 bucket.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePipeline' value with any optional fields omitted.
mkCreatePipeline
    :: Types.Name -- ^ 'name'
    -> Types.BucketName -- ^ 'inputBucket'
    -> Types.Role -- ^ 'role\''
    -> CreatePipeline
mkCreatePipeline name inputBucket role'
  = CreatePipeline'{name, inputBucket, role',
                    awsKmsKeyArn = Core.Nothing, contentConfig = Core.Nothing,
                    notifications = Core.Nothing, outputBucket = Core.Nothing,
                    thumbnailConfig = Core.Nothing}

-- | The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' CreatePipeline Types.Name
cName = Lens.field @"name"
{-# INLINEABLE cName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The Amazon S3 bucket in which you saved the media files that you want to transcode.
--
-- /Note:/ Consider using 'inputBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInputBucket :: Lens.Lens' CreatePipeline Types.BucketName
cInputBucket = Lens.field @"inputBucket"
{-# INLINEABLE cInputBucket #-}
{-# DEPRECATED inputBucket "Use generic-lens or generic-optics with 'inputBucket' instead"  #-}

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic Transcoder to use to create the pipeline.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRole :: Lens.Lens' CreatePipeline Types.Role
cRole = Lens.field @"role'"
{-# INLINEABLE cRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
--
-- /Note:/ Consider using 'awsKmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAwsKmsKeyArn :: Lens.Lens' CreatePipeline (Core.Maybe Types.KeyArn)
cAwsKmsKeyArn = Lens.field @"awsKmsKeyArn"
{-# INLINEABLE cAwsKmsKeyArn #-}
{-# DEPRECATED awsKmsKeyArn "Use generic-lens or generic-optics with 'awsKmsKeyArn' instead"  #-}

-- | The optional @ContentConfig@ object specifies information about the Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists: which bucket to use, which users you want to have access to the files, the type of access you want users to have, and the storage class that you want to assign to the files.
--
-- If you specify values for @ContentConfig@ , you must also specify values for @ThumbnailConfig@ .
-- If you specify values for @ContentConfig@ and @ThumbnailConfig@ , omit the @OutputBucket@ object.
--
--     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists.
--
--
--     * __Permissions__ (Optional): The Permissions object specifies which users you want to have access to transcoded files and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups.
--
--
--     * __Grantee Type__ : Specify the type of value that appears in the @Grantee@ object: 
--
--     * __Canonical__ : The value in the @Grantee@ object is either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. For more information about canonical user IDs, see Access Control List (ACL) Overview in the Amazon Simple Storage Service Developer Guide. For more information about using CloudFront origin access identities to require that users use CloudFront URLs instead of Amazon S3 URLs, see Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content.
-- /Important:/ A canonical user ID is not the same as an AWS account number.
--
--
--     * __Email__ : The value in the @Grantee@ object is the registered email address of an AWS account.
--
--
--     * __Group__ : The value in the @Grantee@ object is one of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
--
--
--
--
--     * __Grantee__ : The AWS user or group that you want to have access to transcoded files and playlists. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group 
--
--
--     * __Access__ : The permission that you want to give to the AWS user that you specified in @Grantee@ . Permissions are granted on the files that Elastic Transcoder adds to the bucket, including playlists and video files. Valid values include: 
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
--     * __StorageClass__ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
--
--
--
-- /Note:/ Consider using 'contentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContentConfig :: Lens.Lens' CreatePipeline (Core.Maybe Types.PipelineOutputConfig)
cContentConfig = Lens.field @"contentConfig"
{-# INLINEABLE cContentConfig #-}
{-# DEPRECATED contentConfig "Use generic-lens or generic-optics with 'contentConfig' instead"  #-}

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status.
--
-- /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.
--
--     * __Progressing__ : The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic. For more information, see Create a Topic in the Amazon Simple Notification Service Developer Guide.
--
--
--     * __Complete__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Warning__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition while processing a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Error__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition while processing a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNotifications :: Lens.Lens' CreatePipeline (Core.Maybe Types.Notifications)
cNotifications = Lens.field @"notifications"
{-# INLINEABLE cNotifications #-}
{-# DEPRECATED notifications "Use generic-lens or generic-optics with 'notifications' instead"  #-}

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save the transcoded files. (Use this, or use ContentConfig:Bucket plus ThumbnailConfig:Bucket.)
--
-- Specify this value when all of the following are true:
--
--     * You want to save transcoded files, thumbnails (if any), and playlists (if any) together in one bucket.
--
--
--     * You do not want to specify the users or groups who have access to the transcoded files, thumbnails, and playlists.
--
--
--     * You do not want to specify the permissions that Elastic Transcoder grants to the files. 
-- /Important:/ When Elastic Transcoder saves files in @OutputBucket@ , it grants full control over the files only to the AWS account that owns the role that is specified by @Role@ .
--
--
--     * You want to associate the transcoded files and thumbnails with the Amazon S3 Standard storage class.
--
--
-- If you want to save transcoded files and playlists in one bucket and thumbnails in another bucket, specify which users can access the transcoded files or the permissions the users have, or change the Amazon S3 storage class, omit @OutputBucket@ and specify values for @ContentConfig@ and @ThumbnailConfig@ instead.
--
-- /Note:/ Consider using 'outputBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOutputBucket :: Lens.Lens' CreatePipeline (Core.Maybe Types.BucketName)
cOutputBucket = Lens.field @"outputBucket"
{-# INLINEABLE cOutputBucket #-}
{-# DEPRECATED outputBucket "Use generic-lens or generic-optics with 'outputBucket' instead"  #-}

-- | The @ThumbnailConfig@ object specifies several values, including the Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files, which users you want to have access to the files, the type of access you want users to have, and the storage class that you want to assign to the files.
--
-- If you specify values for @ContentConfig@ , you must also specify values for @ThumbnailConfig@ even if you don't want to create thumbnails.
-- If you specify values for @ContentConfig@ and @ThumbnailConfig@ , omit the @OutputBucket@ object.
--
--     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files.
--
--
--     * __Permissions__ (Optional): The @Permissions@ object specifies which users and/or predefined Amazon S3 groups you want to have access to thumbnail files, and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups.
--
--
--     * __GranteeType__ : Specify the type of value that appears in the Grantee object: 
--
--     * __Canonical__ : The value in the @Grantee@ object is either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution.
-- /Important:/ A canonical user ID is not the same as an AWS account number.
--
--
--     * __Email__ : The value in the @Grantee@ object is the registered email address of an AWS account. 
--
--
--     * __Group__ : The value in the @Grantee@ object is one of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
--
--
--
--
--     * __Grantee__ : The AWS user or group that you want to have access to thumbnail files. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group. 
--
--
--     * __Access__ : The permission that you want to give to the AWS user that you specified in @Grantee@ . Permissions are granted on the thumbnail files that Elastic Transcoder adds to the bucket. Valid values include: 
--
--     * @READ@ : The grantee can read the thumbnails and metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--     * @FULL_CONTROL@ : The grantee has @READ@ , @READ_ACP@ , and @WRITE_ACP@ permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
--
--
--
--     * __StorageClass__ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the thumbnails that it stores in your Amazon S3 bucket.
--
--
--
-- /Note:/ Consider using 'thumbnailConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cThumbnailConfig :: Lens.Lens' CreatePipeline (Core.Maybe Types.PipelineOutputConfig)
cThumbnailConfig = Lens.field @"thumbnailConfig"
{-# INLINEABLE cThumbnailConfig #-}
{-# DEPRECATED thumbnailConfig "Use generic-lens or generic-optics with 'thumbnailConfig' instead"  #-}

instance Core.ToQuery CreatePipeline where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePipeline where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreatePipeline where
        toJSON CreatePipeline{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("InputBucket" Core..= inputBucket),
                  Core.Just ("Role" Core..= role'),
                  ("AwsKmsKeyArn" Core..=) Core.<$> awsKmsKeyArn,
                  ("ContentConfig" Core..=) Core.<$> contentConfig,
                  ("Notifications" Core..=) Core.<$> notifications,
                  ("OutputBucket" Core..=) Core.<$> outputBucket,
                  ("ThumbnailConfig" Core..=) Core.<$> thumbnailConfig])

instance Core.AWSRequest CreatePipeline where
        type Rs CreatePipeline = CreatePipelineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2012-09-25/pipelines",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePipelineResponse' Core.<$>
                   (x Core..:? "Pipeline") Core.<*> x Core..:? "Warnings" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | When you create a pipeline, Elastic Transcoder returns the values that you specified in the request.
--
-- /See:/ 'mkCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { pipeline :: Core.Maybe Types.Pipeline
    -- ^ A section of the response body that provides information about the pipeline that is created.
  , warnings :: Core.Maybe [Types.Warning]
    -- ^ Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePipelineResponse' value with any optional fields omitted.
mkCreatePipelineResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePipelineResponse
mkCreatePipelineResponse responseStatus
  = CreatePipelineResponse'{pipeline = Core.Nothing,
                            warnings = Core.Nothing, responseStatus}

-- | A section of the response body that provides information about the pipeline that is created.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsPipeline :: Lens.Lens' CreatePipelineResponse (Core.Maybe Types.Pipeline)
crsPipeline = Lens.field @"pipeline"
{-# INLINEABLE crsPipeline #-}
{-# DEPRECATED pipeline "Use generic-lens or generic-optics with 'pipeline' instead"  #-}

-- | Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsWarnings :: Lens.Lens' CreatePipelineResponse (Core.Maybe [Types.Warning])
crsWarnings = Lens.field @"warnings"
{-# INLINEABLE crsWarnings #-}
{-# DEPRECATED warnings "Use generic-lens or generic-optics with 'warnings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreatePipelineResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
