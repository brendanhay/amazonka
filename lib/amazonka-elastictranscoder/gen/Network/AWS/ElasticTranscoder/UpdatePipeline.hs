{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdatePipeline (..),
    mkUpdatePipeline,

    -- ** Request lenses
    upInputBucket,
    upContentConfig,
    upRole,
    upName,
    upAWSKMSKeyARN,
    upId,
    upNotifications,
    upThumbnailConfig,

    -- * Destructuring the response
    UpdatePipelineResponse (..),
    mkUpdatePipelineResponse,

    -- ** Response lenses
    uprsWarnings,
    uprsPipeline,
    uprsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @UpdatePipelineRequest@ structure.
--
-- /See:/ 'mkUpdatePipeline' smart constructor.
data UpdatePipeline = UpdatePipeline'
  { -- | The Amazon S3 bucket in which you saved the media files that you want to transcode and the graphics that you want to use as watermarks.
    inputBucket :: Lude.Maybe Lude.Text,
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
    contentConfig :: Lude.Maybe PipelineOutputConfig,
    -- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic Transcoder to use to transcode jobs for this pipeline.
    role' :: Lude.Maybe Lude.Text,
    -- | The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
    --
    -- Constraints: Maximum 40 characters
    name :: Lude.Maybe Lude.Text,
    -- | The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline.
    --
    -- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
    awsKMSKeyARN :: Lude.Maybe Lude.Text,
    -- | The ID of the pipeline that you want to update.
    id :: Lude.Text,
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
    notifications :: Lude.Maybe Notifications,
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
    thumbnailConfig :: Lude.Maybe PipelineOutputConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePipeline' with the minimum fields required to make a request.
--
-- * 'inputBucket' - The Amazon S3 bucket in which you saved the media files that you want to transcode and the graphics that you want to use as watermarks.
-- * 'contentConfig' - The optional @ContentConfig@ object specifies information about the Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists: which bucket to use, which users you want to have access to the files, the type of access you want users to have, and the storage class that you want to assign to the files.
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
-- * 'role'' - The IAM Amazon Resource Name (ARN) for the role that you want Elastic Transcoder to use to transcode jobs for this pipeline.
-- * 'name' - The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
-- * 'awsKMSKeyARN' - The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
-- * 'id' - The ID of the pipeline that you want to update.
-- * 'notifications' - The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status.
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
-- * 'thumbnailConfig' - The @ThumbnailConfig@ object specifies several values, including the Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files, which users you want to have access to the files, the type of access you want users to have, and the storage class that you want to assign to the files.
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
mkUpdatePipeline ::
  -- | 'id'
  Lude.Text ->
  UpdatePipeline
mkUpdatePipeline pId_ =
  UpdatePipeline'
    { inputBucket = Lude.Nothing,
      contentConfig = Lude.Nothing,
      role' = Lude.Nothing,
      name = Lude.Nothing,
      awsKMSKeyARN = Lude.Nothing,
      id = pId_,
      notifications = Lude.Nothing,
      thumbnailConfig = Lude.Nothing
    }

-- | The Amazon S3 bucket in which you saved the media files that you want to transcode and the graphics that you want to use as watermarks.
--
-- /Note:/ Consider using 'inputBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upInputBucket :: Lens.Lens' UpdatePipeline (Lude.Maybe Lude.Text)
upInputBucket = Lens.lens (inputBucket :: UpdatePipeline -> Lude.Maybe Lude.Text) (\s a -> s {inputBucket = a} :: UpdatePipeline)
{-# DEPRECATED upInputBucket "Use generic-lens or generic-optics with 'inputBucket' instead." #-}

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
upContentConfig :: Lens.Lens' UpdatePipeline (Lude.Maybe PipelineOutputConfig)
upContentConfig = Lens.lens (contentConfig :: UpdatePipeline -> Lude.Maybe PipelineOutputConfig) (\s a -> s {contentConfig = a} :: UpdatePipeline)
{-# DEPRECATED upContentConfig "Use generic-lens or generic-optics with 'contentConfig' instead." #-}

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic Transcoder to use to transcode jobs for this pipeline.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upRole :: Lens.Lens' UpdatePipeline (Lude.Maybe Lude.Text)
upRole = Lens.lens (role' :: UpdatePipeline -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: UpdatePipeline)
{-# DEPRECATED upRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdatePipeline (Lude.Maybe Lude.Text)
upName = Lens.lens (name :: UpdatePipeline -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdatePipeline)
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
--
-- /Note:/ Consider using 'awsKMSKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upAWSKMSKeyARN :: Lens.Lens' UpdatePipeline (Lude.Maybe Lude.Text)
upAWSKMSKeyARN = Lens.lens (awsKMSKeyARN :: UpdatePipeline -> Lude.Maybe Lude.Text) (\s a -> s {awsKMSKeyARN = a} :: UpdatePipeline)
{-# DEPRECATED upAWSKMSKeyARN "Use generic-lens or generic-optics with 'awsKMSKeyARN' instead." #-}

-- | The ID of the pipeline that you want to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upId :: Lens.Lens' UpdatePipeline Lude.Text
upId = Lens.lens (id :: UpdatePipeline -> Lude.Text) (\s a -> s {id = a} :: UpdatePipeline)
{-# DEPRECATED upId "Use generic-lens or generic-optics with 'id' instead." #-}

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
upNotifications :: Lens.Lens' UpdatePipeline (Lude.Maybe Notifications)
upNotifications = Lens.lens (notifications :: UpdatePipeline -> Lude.Maybe Notifications) (\s a -> s {notifications = a} :: UpdatePipeline)
{-# DEPRECATED upNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

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
upThumbnailConfig :: Lens.Lens' UpdatePipeline (Lude.Maybe PipelineOutputConfig)
upThumbnailConfig = Lens.lens (thumbnailConfig :: UpdatePipeline -> Lude.Maybe PipelineOutputConfig) (\s a -> s {thumbnailConfig = a} :: UpdatePipeline)
{-# DEPRECATED upThumbnailConfig "Use generic-lens or generic-optics with 'thumbnailConfig' instead." #-}

instance Lude.AWSRequest UpdatePipeline where
  type Rs UpdatePipeline = UpdatePipelineResponse
  request = Req.putJSON elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePipelineResponse'
            Lude.<$> (x Lude..?> "Warnings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Pipeline")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePipeline where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdatePipeline where
  toJSON UpdatePipeline' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InputBucket" Lude..=) Lude.<$> inputBucket,
            ("ContentConfig" Lude..=) Lude.<$> contentConfig,
            ("Role" Lude..=) Lude.<$> role',
            ("Name" Lude..=) Lude.<$> name,
            ("AwsKmsKeyArn" Lude..=) Lude.<$> awsKMSKeyARN,
            ("Notifications" Lude..=) Lude.<$> notifications,
            ("ThumbnailConfig" Lude..=) Lude.<$> thumbnailConfig
          ]
      )

instance Lude.ToPath UpdatePipeline where
  toPath UpdatePipeline' {..} =
    Lude.mconcat ["/2012-09-25/pipelines/", Lude.toBS id]

instance Lude.ToQuery UpdatePipeline where
  toQuery = Lude.const Lude.mempty

-- | When you update a pipeline, Elastic Transcoder returns the values that you specified in the request.
--
-- /See:/ 'mkUpdatePipelineResponse' smart constructor.
data UpdatePipelineResponse = UpdatePipelineResponse'
  { -- | Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
    --
    -- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
    warnings :: Lude.Maybe [Warning],
    -- | The pipeline updated by this @UpdatePipelineResponse@ call.
    pipeline :: Lude.Maybe Pipeline,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePipelineResponse' with the minimum fields required to make a request.
--
-- * 'warnings' - Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
-- * 'pipeline' - The pipeline updated by this @UpdatePipelineResponse@ call.
-- * 'responseStatus' - The response status code.
mkUpdatePipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePipelineResponse
mkUpdatePipelineResponse pResponseStatus_ =
  UpdatePipelineResponse'
    { warnings = Lude.Nothing,
      pipeline = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsWarnings :: Lens.Lens' UpdatePipelineResponse (Lude.Maybe [Warning])
uprsWarnings = Lens.lens (warnings :: UpdatePipelineResponse -> Lude.Maybe [Warning]) (\s a -> s {warnings = a} :: UpdatePipelineResponse)
{-# DEPRECATED uprsWarnings "Use generic-lens or generic-optics with 'warnings' instead." #-}

-- | The pipeline updated by this @UpdatePipelineResponse@ call.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsPipeline :: Lens.Lens' UpdatePipelineResponse (Lude.Maybe Pipeline)
uprsPipeline = Lens.lens (pipeline :: UpdatePipelineResponse -> Lude.Maybe Pipeline) (\s a -> s {pipeline = a} :: UpdatePipelineResponse)
{-# DEPRECATED uprsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UpdatePipelineResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UpdatePipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePipelineResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
