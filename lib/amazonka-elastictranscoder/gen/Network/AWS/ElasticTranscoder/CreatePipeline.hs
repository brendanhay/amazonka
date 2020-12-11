{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreatePipeline (..),
    mkCreatePipeline,

    -- ** Request lenses
    cContentConfig,
    cOutputBucket,
    cAWSKMSKeyARN,
    cNotifications,
    cThumbnailConfig,
    cName,
    cInputBucket,
    cRole,

    -- * Destructuring the response
    CreatePipelineResponse (..),
    mkCreatePipelineResponse,

    -- ** Response lenses
    crsWarnings,
    crsPipeline,
    crsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @CreatePipelineRequest@ structure.
--
-- /See:/ 'mkCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { contentConfig ::
      Lude.Maybe PipelineOutputConfig,
    outputBucket :: Lude.Maybe Lude.Text,
    awsKMSKeyARN :: Lude.Maybe Lude.Text,
    notifications :: Lude.Maybe Notifications,
    thumbnailConfig :: Lude.Maybe PipelineOutputConfig,
    name :: Lude.Text,
    inputBucket :: Lude.Text,
    role' :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePipeline' with the minimum fields required to make a request.
--
-- * 'awsKMSKeyARN' - The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
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
-- * 'inputBucket' - The Amazon S3 bucket in which you saved the media files that you want to transcode.
-- * 'name' - The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters.
-- * 'notifications' - The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status.
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
-- * 'outputBucket' - The Amazon S3 bucket in which you want Elastic Transcoder to save the transcoded files. (Use this, or use ContentConfig:Bucket plus ThumbnailConfig:Bucket.)
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
-- * 'role'' - The IAM Amazon Resource Name (ARN) for the role that you want Elastic Transcoder to use to create the pipeline.
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
mkCreatePipeline ::
  -- | 'name'
  Lude.Text ->
  -- | 'inputBucket'
  Lude.Text ->
  -- | 'role''
  Lude.Text ->
  CreatePipeline
mkCreatePipeline pName_ pInputBucket_ pRole_ =
  CreatePipeline'
    { contentConfig = Lude.Nothing,
      outputBucket = Lude.Nothing,
      awsKMSKeyARN = Lude.Nothing,
      notifications = Lude.Nothing,
      thumbnailConfig = Lude.Nothing,
      name = pName_,
      inputBucket = pInputBucket_,
      role' = pRole_
    }

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
cContentConfig :: Lens.Lens' CreatePipeline (Lude.Maybe PipelineOutputConfig)
cContentConfig = Lens.lens (contentConfig :: CreatePipeline -> Lude.Maybe PipelineOutputConfig) (\s a -> s {contentConfig = a} :: CreatePipeline)
{-# DEPRECATED cContentConfig "Use generic-lens or generic-optics with 'contentConfig' instead." #-}

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
cOutputBucket :: Lens.Lens' CreatePipeline (Lude.Maybe Lude.Text)
cOutputBucket = Lens.lens (outputBucket :: CreatePipeline -> Lude.Maybe Lude.Text) (\s a -> s {outputBucket = a} :: CreatePipeline)
{-# DEPRECATED cOutputBucket "Use generic-lens or generic-optics with 'outputBucket' instead." #-}

-- | The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
--
-- /Note:/ Consider using 'awsKMSKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAWSKMSKeyARN :: Lens.Lens' CreatePipeline (Lude.Maybe Lude.Text)
cAWSKMSKeyARN = Lens.lens (awsKMSKeyARN :: CreatePipeline -> Lude.Maybe Lude.Text) (\s a -> s {awsKMSKeyARN = a} :: CreatePipeline)
{-# DEPRECATED cAWSKMSKeyARN "Use generic-lens or generic-optics with 'awsKMSKeyARN' instead." #-}

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
cNotifications :: Lens.Lens' CreatePipeline (Lude.Maybe Notifications)
cNotifications = Lens.lens (notifications :: CreatePipeline -> Lude.Maybe Notifications) (\s a -> s {notifications = a} :: CreatePipeline)
{-# DEPRECATED cNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

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
cThumbnailConfig :: Lens.Lens' CreatePipeline (Lude.Maybe PipelineOutputConfig)
cThumbnailConfig = Lens.lens (thumbnailConfig :: CreatePipeline -> Lude.Maybe PipelineOutputConfig) (\s a -> s {thumbnailConfig = a} :: CreatePipeline)
{-# DEPRECATED cThumbnailConfig "Use generic-lens or generic-optics with 'thumbnailConfig' instead." #-}

-- | The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' CreatePipeline Lude.Text
cName = Lens.lens (name :: CreatePipeline -> Lude.Text) (\s a -> s {name = a} :: CreatePipeline)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon S3 bucket in which you saved the media files that you want to transcode.
--
-- /Note:/ Consider using 'inputBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInputBucket :: Lens.Lens' CreatePipeline Lude.Text
cInputBucket = Lens.lens (inputBucket :: CreatePipeline -> Lude.Text) (\s a -> s {inputBucket = a} :: CreatePipeline)
{-# DEPRECATED cInputBucket "Use generic-lens or generic-optics with 'inputBucket' instead." #-}

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic Transcoder to use to create the pipeline.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRole :: Lens.Lens' CreatePipeline Lude.Text
cRole = Lens.lens (role' :: CreatePipeline -> Lude.Text) (\s a -> s {role' = a} :: CreatePipeline)
{-# DEPRECATED cRole "Use generic-lens or generic-optics with 'role'' instead." #-}

instance Lude.AWSRequest CreatePipeline where
  type Rs CreatePipeline = CreatePipelineResponse
  request = Req.postJSON elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Lude.<$> (x Lude..?> "Warnings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Pipeline")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePipeline where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreatePipeline where
  toJSON CreatePipeline' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ContentConfig" Lude..=) Lude.<$> contentConfig,
            ("OutputBucket" Lude..=) Lude.<$> outputBucket,
            ("AwsKmsKeyArn" Lude..=) Lude.<$> awsKMSKeyARN,
            ("Notifications" Lude..=) Lude.<$> notifications,
            ("ThumbnailConfig" Lude..=) Lude.<$> thumbnailConfig,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("InputBucket" Lude..= inputBucket),
            Lude.Just ("Role" Lude..= role')
          ]
      )

instance Lude.ToPath CreatePipeline where
  toPath = Lude.const "/2012-09-25/pipelines"

instance Lude.ToQuery CreatePipeline where
  toQuery = Lude.const Lude.mempty

-- | When you create a pipeline, Elastic Transcoder returns the values that you specified in the request.
--
-- /See:/ 'mkCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { warnings ::
      Lude.Maybe [Warning],
    pipeline :: Lude.Maybe Pipeline,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePipelineResponse' with the minimum fields required to make a request.
--
-- * 'pipeline' - A section of the response body that provides information about the pipeline that is created.
-- * 'responseStatus' - The response status code.
-- * 'warnings' - Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
mkCreatePipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePipelineResponse
mkCreatePipelineResponse pResponseStatus_ =
  CreatePipelineResponse'
    { warnings = Lude.Nothing,
      pipeline = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsWarnings :: Lens.Lens' CreatePipelineResponse (Lude.Maybe [Warning])
crsWarnings = Lens.lens (warnings :: CreatePipelineResponse -> Lude.Maybe [Warning]) (\s a -> s {warnings = a} :: CreatePipelineResponse)
{-# DEPRECATED crsWarnings "Use generic-lens or generic-optics with 'warnings' instead." #-}

-- | A section of the response body that provides information about the pipeline that is created.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsPipeline :: Lens.Lens' CreatePipelineResponse (Lude.Maybe Pipeline)
crsPipeline = Lens.lens (pipeline :: CreatePipelineResponse -> Lude.Maybe Pipeline) (\s a -> s {pipeline = a} :: CreatePipelineResponse)
{-# DEPRECATED crsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreatePipelineResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreatePipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePipelineResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
