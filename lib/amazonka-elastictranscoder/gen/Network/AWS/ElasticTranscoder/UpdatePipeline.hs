{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.UpdatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @UpdatePipeline@ operation to update settings for a pipeline.
--
-- /Important:/ When you change pipeline settings, your changes take effect immediately. Jobs that you have already submitted and that Elastic Transcoder has not started to process are affected in addition to jobs that you submit after you change settings. 
module Network.AWS.ElasticTranscoder.UpdatePipeline
    (
    -- * Creating a request
      UpdatePipeline (..)
    , mkUpdatePipeline
    -- ** Request lenses
    , upId
    , upAwsKmsKeyArn
    , upContentConfig
    , upInputBucket
    , upName
    , upNotifications
    , upRole
    , upThumbnailConfig

    -- * Destructuring the response
    , UpdatePipelineResponse (..)
    , mkUpdatePipelineResponse
    -- ** Response lenses
    , uprrsPipeline
    , uprrsWarnings
    , uprrsResponseStatus
    ) where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @UpdatePipelineRequest@ structure.
--
-- /See:/ 'mkUpdatePipeline' smart constructor.
data UpdatePipeline = UpdatePipeline'
  { id :: Types.Id
    -- ^ The ID of the pipeline that you want to update.
  , awsKmsKeyArn :: Core.Maybe Types.AwsKmsKeyArn
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
  , inputBucket :: Core.Maybe Types.InputBucket
    -- ^ The Amazon S3 bucket in which you saved the media files that you want to transcode and the graphics that you want to use as watermarks.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
  , notifications :: Core.Maybe Types.Notifications
    -- ^ The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status.
--
-- /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.
--
--     * __Progressing__ : The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process jobs that are added to this pipeline. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Complete__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing a job. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Warning__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Error__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition. This is the ARN that Amazon SNS returned when you created the topic.
--
--
  , role' :: Core.Maybe Types.Role
    -- ^ The IAM Amazon Resource Name (ARN) for the role that you want Elastic Transcoder to use to transcode jobs for this pipeline.
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

-- | Creates a 'UpdatePipeline' value with any optional fields omitted.
mkUpdatePipeline
    :: Types.Id -- ^ 'id'
    -> UpdatePipeline
mkUpdatePipeline id
  = UpdatePipeline'{id, awsKmsKeyArn = Core.Nothing,
                    contentConfig = Core.Nothing, inputBucket = Core.Nothing,
                    name = Core.Nothing, notifications = Core.Nothing,
                    role' = Core.Nothing, thumbnailConfig = Core.Nothing}

-- | The ID of the pipeline that you want to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upId :: Lens.Lens' UpdatePipeline Types.Id
upId = Lens.field @"id"
{-# INLINEABLE upId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
--
-- /Note:/ Consider using 'awsKmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upAwsKmsKeyArn :: Lens.Lens' UpdatePipeline (Core.Maybe Types.AwsKmsKeyArn)
upAwsKmsKeyArn = Lens.field @"awsKmsKeyArn"
{-# INLINEABLE upAwsKmsKeyArn #-}
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
upContentConfig :: Lens.Lens' UpdatePipeline (Core.Maybe Types.PipelineOutputConfig)
upContentConfig = Lens.field @"contentConfig"
{-# INLINEABLE upContentConfig #-}
{-# DEPRECATED contentConfig "Use generic-lens or generic-optics with 'contentConfig' instead"  #-}

-- | The Amazon S3 bucket in which you saved the media files that you want to transcode and the graphics that you want to use as watermarks.
--
-- /Note:/ Consider using 'inputBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upInputBucket :: Lens.Lens' UpdatePipeline (Core.Maybe Types.InputBucket)
upInputBucket = Lens.field @"inputBucket"
{-# INLINEABLE upInputBucket #-}
{-# DEPRECATED inputBucket "Use generic-lens or generic-optics with 'inputBucket' instead"  #-}

-- | The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdatePipeline (Core.Maybe Types.Name)
upName = Lens.field @"name"
{-# INLINEABLE upName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status.
--
-- /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.
--
--     * __Progressing__ : The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process jobs that are added to this pipeline. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Complete__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing a job. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Warning__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Error__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upNotifications :: Lens.Lens' UpdatePipeline (Core.Maybe Types.Notifications)
upNotifications = Lens.field @"notifications"
{-# INLINEABLE upNotifications #-}
{-# DEPRECATED notifications "Use generic-lens or generic-optics with 'notifications' instead"  #-}

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic Transcoder to use to transcode jobs for this pipeline.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upRole :: Lens.Lens' UpdatePipeline (Core.Maybe Types.Role)
upRole = Lens.field @"role'"
{-# INLINEABLE upRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

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
upThumbnailConfig :: Lens.Lens' UpdatePipeline (Core.Maybe Types.PipelineOutputConfig)
upThumbnailConfig = Lens.field @"thumbnailConfig"
{-# INLINEABLE upThumbnailConfig #-}
{-# DEPRECATED thumbnailConfig "Use generic-lens or generic-optics with 'thumbnailConfig' instead"  #-}

instance Core.ToQuery UpdatePipeline where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdatePipeline where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdatePipeline where
        toJSON UpdatePipeline{..}
          = Core.object
              (Core.catMaybes
                 [("AwsKmsKeyArn" Core..=) Core.<$> awsKmsKeyArn,
                  ("ContentConfig" Core..=) Core.<$> contentConfig,
                  ("InputBucket" Core..=) Core.<$> inputBucket,
                  ("Name" Core..=) Core.<$> name,
                  ("Notifications" Core..=) Core.<$> notifications,
                  ("Role" Core..=) Core.<$> role',
                  ("ThumbnailConfig" Core..=) Core.<$> thumbnailConfig])

instance Core.AWSRequest UpdatePipeline where
        type Rs UpdatePipeline = UpdatePipelineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/2012-09-25/pipelines/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdatePipelineResponse' Core.<$>
                   (x Core..:? "Pipeline") Core.<*> x Core..:? "Warnings" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | When you update a pipeline, Elastic Transcoder returns the values that you specified in the request.
--
-- /See:/ 'mkUpdatePipelineResponse' smart constructor.
data UpdatePipelineResponse = UpdatePipelineResponse'
  { pipeline :: Core.Maybe Types.Pipeline
    -- ^ The pipeline updated by this @UpdatePipelineResponse@ call.
  , warnings :: Core.Maybe [Types.Warning]
    -- ^ Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePipelineResponse' value with any optional fields omitted.
mkUpdatePipelineResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdatePipelineResponse
mkUpdatePipelineResponse responseStatus
  = UpdatePipelineResponse'{pipeline = Core.Nothing,
                            warnings = Core.Nothing, responseStatus}

-- | The pipeline updated by this @UpdatePipelineResponse@ call.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsPipeline :: Lens.Lens' UpdatePipelineResponse (Core.Maybe Types.Pipeline)
uprrsPipeline = Lens.field @"pipeline"
{-# INLINEABLE uprrsPipeline #-}
{-# DEPRECATED pipeline "Use generic-lens or generic-optics with 'pipeline' instead"  #-}

-- | Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsWarnings :: Lens.Lens' UpdatePipelineResponse (Core.Maybe [Types.Warning])
uprrsWarnings = Lens.field @"warnings"
{-# INLINEABLE uprrsWarnings #-}
{-# DEPRECATED warnings "Use generic-lens or generic-optics with 'warnings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdatePipelineResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
