{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticTranscoder.CreatePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CreatePipeline operation creates a pipeline with settings that you
-- specify.
module Amazonka.ElasticTranscoder.CreatePipeline
  ( -- * Creating a Request
    CreatePipeline (..),
    newCreatePipeline,

    -- * Request Lenses
    createPipeline_awsKmsKeyArn,
    createPipeline_contentConfig,
    createPipeline_notifications,
    createPipeline_outputBucket,
    createPipeline_thumbnailConfig,
    createPipeline_name,
    createPipeline_inputBucket,
    createPipeline_role,

    -- * Destructuring the Response
    CreatePipelineResponse (..),
    newCreatePipelineResponse,

    -- * Response Lenses
    createPipelineResponse_pipeline,
    createPipelineResponse_warnings,
    createPipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The @CreatePipelineRequest@ structure.
--
-- /See:/ 'newCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { -- | The AWS Key Management Service (AWS KMS) key that you want to use with
    -- this pipeline.
    --
    -- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@, you
    -- don\'t need to provide a key with your job because a default key, known
    -- as an AWS-KMS key, is created for you automatically. You need to provide
    -- an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if
    -- you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@, @aes-ctr@, or
    -- @aes-gcm@.
    awsKmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The optional @ContentConfig@ object specifies information about the
    -- Amazon S3 bucket in which you want Elastic Transcoder to save transcoded
    -- files and playlists: which bucket to use, which users you want to have
    -- access to the files, the type of access you want users to have, and the
    -- storage class that you want to assign to the files.
    --
    -- If you specify values for @ContentConfig@, you must also specify values
    -- for @ThumbnailConfig@.
    --
    -- If you specify values for @ContentConfig@ and @ThumbnailConfig@, omit
    -- the @OutputBucket@ object.
    --
    -- -   __Bucket__: The Amazon S3 bucket in which you want Elastic
    --     Transcoder to save transcoded files and playlists.
    --
    -- -   __Permissions__ (Optional): The Permissions object specifies which
    --     users you want to have access to transcoded files and the type of
    --     access you want them to have. You can grant permissions to a maximum
    --     of 30 users and\/or predefined Amazon S3 groups.
    --
    -- -   __Grantee Type__: Specify the type of value that appears in the
    --     @Grantee@ object:
    --
    --     -   __Canonical__: The value in the @Grantee@ object is either the
    --         canonical user ID for an AWS account or an origin access
    --         identity for an Amazon CloudFront distribution. For more
    --         information about canonical user IDs, see Access Control List
    --         (ACL) Overview in the Amazon Simple Storage Service Developer
    --         Guide. For more information about using CloudFront origin access
    --         identities to require that users use CloudFront URLs instead of
    --         Amazon S3 URLs, see Using an Origin Access Identity to Restrict
    --         Access to Your Amazon S3 Content.
    --
    --         A canonical user ID is not the same as an AWS account number.
    --
    --     -   __Email__: The value in the @Grantee@ object is the registered
    --         email address of an AWS account.
    --
    --     -   __Group__: The value in the @Grantee@ object is one of the
    --         following predefined Amazon S3 groups: @AllUsers@,
    --         @AuthenticatedUsers@, or @LogDelivery@.
    --
    -- -   __Grantee__: The AWS user or group that you want to have access to
    --     transcoded files and playlists. To identify the user or group, you
    --     can specify the canonical user ID for an AWS account, an origin
    --     access identity for a CloudFront distribution, the registered email
    --     address of an AWS account, or a predefined Amazon S3 group
    --
    -- -   __Access__: The permission that you want to give to the AWS user
    --     that you specified in @Grantee@. Permissions are granted on the
    --     files that Elastic Transcoder adds to the bucket, including
    --     playlists and video files. Valid values include:
    --
    --     -   @READ@: The grantee can read the objects and metadata for
    --         objects that Elastic Transcoder adds to the Amazon S3 bucket.
    --
    --     -   @READ_ACP@: The grantee can read the object ACL for objects that
    --         Elastic Transcoder adds to the Amazon S3 bucket.
    --
    --     -   @WRITE_ACP@: The grantee can write the ACL for the objects that
    --         Elastic Transcoder adds to the Amazon S3 bucket.
    --
    --     -   @FULL_CONTROL@: The grantee has @READ@, @READ_ACP@, and
    --         @WRITE_ACP@ permissions for the objects that Elastic Transcoder
    --         adds to the Amazon S3 bucket.
    --
    -- -   __StorageClass__: The Amazon S3 storage class, @Standard@ or
    --     @ReducedRedundancy@, that you want Elastic Transcoder to assign to
    --     the video files and playlists that it stores in your Amazon S3
    --     bucket.
    contentConfig :: Prelude.Maybe PipelineOutputConfig,
    -- | The Amazon Simple Notification Service (Amazon SNS) topic that you want
    -- to notify to report job status.
    --
    -- To receive notifications, you must also subscribe to the new topic in
    -- the Amazon SNS console.
    --
    -- -   __Progressing__: The topic ARN for the Amazon Simple Notification
    --     Service (Amazon SNS) topic that you want to notify when Elastic
    --     Transcoder has started to process a job in this pipeline. This is
    --     the ARN that Amazon SNS returned when you created the topic. For
    --     more information, see Create a Topic in the Amazon Simple
    --     Notification Service Developer Guide.
    --
    -- -   __Complete__: The topic ARN for the Amazon SNS topic that you want
    --     to notify when Elastic Transcoder has finished processing a job in
    --     this pipeline. This is the ARN that Amazon SNS returned when you
    --     created the topic.
    --
    -- -   __Warning__: The topic ARN for the Amazon SNS topic that you want to
    --     notify when Elastic Transcoder encounters a warning condition while
    --     processing a job in this pipeline. This is the ARN that Amazon SNS
    --     returned when you created the topic.
    --
    -- -   __Error__: The topic ARN for the Amazon SNS topic that you want to
    --     notify when Elastic Transcoder encounters an error condition while
    --     processing a job in this pipeline. This is the ARN that Amazon SNS
    --     returned when you created the topic.
    notifications :: Prelude.Maybe Notifications,
    -- | The Amazon S3 bucket in which you want Elastic Transcoder to save the
    -- transcoded files. (Use this, or use ContentConfig:Bucket plus
    -- ThumbnailConfig:Bucket.)
    --
    -- Specify this value when all of the following are true:
    --
    -- -   You want to save transcoded files, thumbnails (if any), and
    --     playlists (if any) together in one bucket.
    --
    -- -   You do not want to specify the users or groups who have access to
    --     the transcoded files, thumbnails, and playlists.
    --
    -- -   You do not want to specify the permissions that Elastic Transcoder
    --     grants to the files.
    --
    --     When Elastic Transcoder saves files in @OutputBucket@, it grants
    --     full control over the files only to the AWS account that owns the
    --     role that is specified by @Role@.
    --
    -- -   You want to associate the transcoded files and thumbnails with the
    --     Amazon S3 Standard storage class.
    --
    -- If you want to save transcoded files and playlists in one bucket and
    -- thumbnails in another bucket, specify which users can access the
    -- transcoded files or the permissions the users have, or change the Amazon
    -- S3 storage class, omit @OutputBucket@ and specify values for
    -- @ContentConfig@ and @ThumbnailConfig@ instead.
    outputBucket :: Prelude.Maybe Prelude.Text,
    -- | The @ThumbnailConfig@ object specifies several values, including the
    -- Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail
    -- files, which users you want to have access to the files, the type of
    -- access you want users to have, and the storage class that you want to
    -- assign to the files.
    --
    -- If you specify values for @ContentConfig@, you must also specify values
    -- for @ThumbnailConfig@ even if you don\'t want to create thumbnails.
    --
    -- If you specify values for @ContentConfig@ and @ThumbnailConfig@, omit
    -- the @OutputBucket@ object.
    --
    -- -   __Bucket__: The Amazon S3 bucket in which you want Elastic
    --     Transcoder to save thumbnail files.
    --
    -- -   __Permissions__ (Optional): The @Permissions@ object specifies which
    --     users and\/or predefined Amazon S3 groups you want to have access to
    --     thumbnail files, and the type of access you want them to have. You
    --     can grant permissions to a maximum of 30 users and\/or predefined
    --     Amazon S3 groups.
    --
    -- -   __GranteeType__: Specify the type of value that appears in the
    --     Grantee object:
    --
    --     -   __Canonical__: The value in the @Grantee@ object is either the
    --         canonical user ID for an AWS account or an origin access
    --         identity for an Amazon CloudFront distribution.
    --
    --         A canonical user ID is not the same as an AWS account number.
    --
    --     -   __Email__: The value in the @Grantee@ object is the registered
    --         email address of an AWS account.
    --
    --     -   __Group__: The value in the @Grantee@ object is one of the
    --         following predefined Amazon S3 groups: @AllUsers@,
    --         @AuthenticatedUsers@, or @LogDelivery@.
    --
    -- -   __Grantee__: The AWS user or group that you want to have access to
    --     thumbnail files. To identify the user or group, you can specify the
    --     canonical user ID for an AWS account, an origin access identity for
    --     a CloudFront distribution, the registered email address of an AWS
    --     account, or a predefined Amazon S3 group.
    --
    -- -   __Access__: The permission that you want to give to the AWS user
    --     that you specified in @Grantee@. Permissions are granted on the
    --     thumbnail files that Elastic Transcoder adds to the bucket. Valid
    --     values include:
    --
    --     -   @READ@: The grantee can read the thumbnails and metadata for
    --         objects that Elastic Transcoder adds to the Amazon S3 bucket.
    --
    --     -   @READ_ACP@: The grantee can read the object ACL for thumbnails
    --         that Elastic Transcoder adds to the Amazon S3 bucket.
    --
    --     -   @WRITE_ACP@: The grantee can write the ACL for the thumbnails
    --         that Elastic Transcoder adds to the Amazon S3 bucket.
    --
    --     -   @FULL_CONTROL@: The grantee has @READ@, @READ_ACP@, and
    --         @WRITE_ACP@ permissions for the thumbnails that Elastic
    --         Transcoder adds to the Amazon S3 bucket.
    --
    -- -   __StorageClass__: The Amazon S3 storage class, @Standard@ or
    --     @ReducedRedundancy@, that you want Elastic Transcoder to assign to
    --     the thumbnails that it stores in your Amazon S3 bucket.
    thumbnailConfig :: Prelude.Maybe PipelineOutputConfig,
    -- | The name of the pipeline. We recommend that the name be unique within
    -- the AWS account, but uniqueness is not enforced.
    --
    -- Constraints: Maximum 40 characters.
    name :: Prelude.Text,
    -- | The Amazon S3 bucket in which you saved the media files that you want to
    -- transcode.
    inputBucket :: Prelude.Text,
    -- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic
    -- Transcoder to use to create the pipeline.
    role' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsKmsKeyArn', 'createPipeline_awsKmsKeyArn' - The AWS Key Management Service (AWS KMS) key that you want to use with
-- this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@, you
-- don\'t need to provide a key with your job because a default key, known
-- as an AWS-KMS key, is created for you automatically. You need to provide
-- an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if
-- you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@, @aes-ctr@, or
-- @aes-gcm@.
--
-- 'contentConfig', 'createPipeline_contentConfig' - The optional @ContentConfig@ object specifies information about the
-- Amazon S3 bucket in which you want Elastic Transcoder to save transcoded
-- files and playlists: which bucket to use, which users you want to have
-- access to the files, the type of access you want users to have, and the
-- storage class that you want to assign to the files.
--
-- If you specify values for @ContentConfig@, you must also specify values
-- for @ThumbnailConfig@.
--
-- If you specify values for @ContentConfig@ and @ThumbnailConfig@, omit
-- the @OutputBucket@ object.
--
-- -   __Bucket__: The Amazon S3 bucket in which you want Elastic
--     Transcoder to save transcoded files and playlists.
--
-- -   __Permissions__ (Optional): The Permissions object specifies which
--     users you want to have access to transcoded files and the type of
--     access you want them to have. You can grant permissions to a maximum
--     of 30 users and\/or predefined Amazon S3 groups.
--
-- -   __Grantee Type__: Specify the type of value that appears in the
--     @Grantee@ object:
--
--     -   __Canonical__: The value in the @Grantee@ object is either the
--         canonical user ID for an AWS account or an origin access
--         identity for an Amazon CloudFront distribution. For more
--         information about canonical user IDs, see Access Control List
--         (ACL) Overview in the Amazon Simple Storage Service Developer
--         Guide. For more information about using CloudFront origin access
--         identities to require that users use CloudFront URLs instead of
--         Amazon S3 URLs, see Using an Origin Access Identity to Restrict
--         Access to Your Amazon S3 Content.
--
--         A canonical user ID is not the same as an AWS account number.
--
--     -   __Email__: The value in the @Grantee@ object is the registered
--         email address of an AWS account.
--
--     -   __Group__: The value in the @Grantee@ object is one of the
--         following predefined Amazon S3 groups: @AllUsers@,
--         @AuthenticatedUsers@, or @LogDelivery@.
--
-- -   __Grantee__: The AWS user or group that you want to have access to
--     transcoded files and playlists. To identify the user or group, you
--     can specify the canonical user ID for an AWS account, an origin
--     access identity for a CloudFront distribution, the registered email
--     address of an AWS account, or a predefined Amazon S3 group
--
-- -   __Access__: The permission that you want to give to the AWS user
--     that you specified in @Grantee@. Permissions are granted on the
--     files that Elastic Transcoder adds to the bucket, including
--     playlists and video files. Valid values include:
--
--     -   @READ@: The grantee can read the objects and metadata for
--         objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--     -   @READ_ACP@: The grantee can read the object ACL for objects that
--         Elastic Transcoder adds to the Amazon S3 bucket.
--
--     -   @WRITE_ACP@: The grantee can write the ACL for the objects that
--         Elastic Transcoder adds to the Amazon S3 bucket.
--
--     -   @FULL_CONTROL@: The grantee has @READ@, @READ_ACP@, and
--         @WRITE_ACP@ permissions for the objects that Elastic Transcoder
--         adds to the Amazon S3 bucket.
--
-- -   __StorageClass__: The Amazon S3 storage class, @Standard@ or
--     @ReducedRedundancy@, that you want Elastic Transcoder to assign to
--     the video files and playlists that it stores in your Amazon S3
--     bucket.
--
-- 'notifications', 'createPipeline_notifications' - The Amazon Simple Notification Service (Amazon SNS) topic that you want
-- to notify to report job status.
--
-- To receive notifications, you must also subscribe to the new topic in
-- the Amazon SNS console.
--
-- -   __Progressing__: The topic ARN for the Amazon Simple Notification
--     Service (Amazon SNS) topic that you want to notify when Elastic
--     Transcoder has started to process a job in this pipeline. This is
--     the ARN that Amazon SNS returned when you created the topic. For
--     more information, see Create a Topic in the Amazon Simple
--     Notification Service Developer Guide.
--
-- -   __Complete__: The topic ARN for the Amazon SNS topic that you want
--     to notify when Elastic Transcoder has finished processing a job in
--     this pipeline. This is the ARN that Amazon SNS returned when you
--     created the topic.
--
-- -   __Warning__: The topic ARN for the Amazon SNS topic that you want to
--     notify when Elastic Transcoder encounters a warning condition while
--     processing a job in this pipeline. This is the ARN that Amazon SNS
--     returned when you created the topic.
--
-- -   __Error__: The topic ARN for the Amazon SNS topic that you want to
--     notify when Elastic Transcoder encounters an error condition while
--     processing a job in this pipeline. This is the ARN that Amazon SNS
--     returned when you created the topic.
--
-- 'outputBucket', 'createPipeline_outputBucket' - The Amazon S3 bucket in which you want Elastic Transcoder to save the
-- transcoded files. (Use this, or use ContentConfig:Bucket plus
-- ThumbnailConfig:Bucket.)
--
-- Specify this value when all of the following are true:
--
-- -   You want to save transcoded files, thumbnails (if any), and
--     playlists (if any) together in one bucket.
--
-- -   You do not want to specify the users or groups who have access to
--     the transcoded files, thumbnails, and playlists.
--
-- -   You do not want to specify the permissions that Elastic Transcoder
--     grants to the files.
--
--     When Elastic Transcoder saves files in @OutputBucket@, it grants
--     full control over the files only to the AWS account that owns the
--     role that is specified by @Role@.
--
-- -   You want to associate the transcoded files and thumbnails with the
--     Amazon S3 Standard storage class.
--
-- If you want to save transcoded files and playlists in one bucket and
-- thumbnails in another bucket, specify which users can access the
-- transcoded files or the permissions the users have, or change the Amazon
-- S3 storage class, omit @OutputBucket@ and specify values for
-- @ContentConfig@ and @ThumbnailConfig@ instead.
--
-- 'thumbnailConfig', 'createPipeline_thumbnailConfig' - The @ThumbnailConfig@ object specifies several values, including the
-- Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail
-- files, which users you want to have access to the files, the type of
-- access you want users to have, and the storage class that you want to
-- assign to the files.
--
-- If you specify values for @ContentConfig@, you must also specify values
-- for @ThumbnailConfig@ even if you don\'t want to create thumbnails.
--
-- If you specify values for @ContentConfig@ and @ThumbnailConfig@, omit
-- the @OutputBucket@ object.
--
-- -   __Bucket__: The Amazon S3 bucket in which you want Elastic
--     Transcoder to save thumbnail files.
--
-- -   __Permissions__ (Optional): The @Permissions@ object specifies which
--     users and\/or predefined Amazon S3 groups you want to have access to
--     thumbnail files, and the type of access you want them to have. You
--     can grant permissions to a maximum of 30 users and\/or predefined
--     Amazon S3 groups.
--
-- -   __GranteeType__: Specify the type of value that appears in the
--     Grantee object:
--
--     -   __Canonical__: The value in the @Grantee@ object is either the
--         canonical user ID for an AWS account or an origin access
--         identity for an Amazon CloudFront distribution.
--
--         A canonical user ID is not the same as an AWS account number.
--
--     -   __Email__: The value in the @Grantee@ object is the registered
--         email address of an AWS account.
--
--     -   __Group__: The value in the @Grantee@ object is one of the
--         following predefined Amazon S3 groups: @AllUsers@,
--         @AuthenticatedUsers@, or @LogDelivery@.
--
-- -   __Grantee__: The AWS user or group that you want to have access to
--     thumbnail files. To identify the user or group, you can specify the
--     canonical user ID for an AWS account, an origin access identity for
--     a CloudFront distribution, the registered email address of an AWS
--     account, or a predefined Amazon S3 group.
--
-- -   __Access__: The permission that you want to give to the AWS user
--     that you specified in @Grantee@. Permissions are granted on the
--     thumbnail files that Elastic Transcoder adds to the bucket. Valid
--     values include:
--
--     -   @READ@: The grantee can read the thumbnails and metadata for
--         objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--     -   @READ_ACP@: The grantee can read the object ACL for thumbnails
--         that Elastic Transcoder adds to the Amazon S3 bucket.
--
--     -   @WRITE_ACP@: The grantee can write the ACL for the thumbnails
--         that Elastic Transcoder adds to the Amazon S3 bucket.
--
--     -   @FULL_CONTROL@: The grantee has @READ@, @READ_ACP@, and
--         @WRITE_ACP@ permissions for the thumbnails that Elastic
--         Transcoder adds to the Amazon S3 bucket.
--
-- -   __StorageClass__: The Amazon S3 storage class, @Standard@ or
--     @ReducedRedundancy@, that you want Elastic Transcoder to assign to
--     the thumbnails that it stores in your Amazon S3 bucket.
--
-- 'name', 'createPipeline_name' - The name of the pipeline. We recommend that the name be unique within
-- the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters.
--
-- 'inputBucket', 'createPipeline_inputBucket' - The Amazon S3 bucket in which you saved the media files that you want to
-- transcode.
--
-- 'role'', 'createPipeline_role' - The IAM Amazon Resource Name (ARN) for the role that you want Elastic
-- Transcoder to use to create the pipeline.
newCreatePipeline ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputBucket'
  Prelude.Text ->
  -- | 'role''
  Prelude.Text ->
  CreatePipeline
newCreatePipeline pName_ pInputBucket_ pRole_ =
  CreatePipeline'
    { awsKmsKeyArn = Prelude.Nothing,
      contentConfig = Prelude.Nothing,
      notifications = Prelude.Nothing,
      outputBucket = Prelude.Nothing,
      thumbnailConfig = Prelude.Nothing,
      name = pName_,
      inputBucket = pInputBucket_,
      role' = pRole_
    }

-- | The AWS Key Management Service (AWS KMS) key that you want to use with
-- this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@, you
-- don\'t need to provide a key with your job because a default key, known
-- as an AWS-KMS key, is created for you automatically. You need to provide
-- an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if
-- you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@, @aes-ctr@, or
-- @aes-gcm@.
createPipeline_awsKmsKeyArn :: Lens.Lens' CreatePipeline (Prelude.Maybe Prelude.Text)
createPipeline_awsKmsKeyArn = Lens.lens (\CreatePipeline' {awsKmsKeyArn} -> awsKmsKeyArn) (\s@CreatePipeline' {} a -> s {awsKmsKeyArn = a} :: CreatePipeline)

-- | The optional @ContentConfig@ object specifies information about the
-- Amazon S3 bucket in which you want Elastic Transcoder to save transcoded
-- files and playlists: which bucket to use, which users you want to have
-- access to the files, the type of access you want users to have, and the
-- storage class that you want to assign to the files.
--
-- If you specify values for @ContentConfig@, you must also specify values
-- for @ThumbnailConfig@.
--
-- If you specify values for @ContentConfig@ and @ThumbnailConfig@, omit
-- the @OutputBucket@ object.
--
-- -   __Bucket__: The Amazon S3 bucket in which you want Elastic
--     Transcoder to save transcoded files and playlists.
--
-- -   __Permissions__ (Optional): The Permissions object specifies which
--     users you want to have access to transcoded files and the type of
--     access you want them to have. You can grant permissions to a maximum
--     of 30 users and\/or predefined Amazon S3 groups.
--
-- -   __Grantee Type__: Specify the type of value that appears in the
--     @Grantee@ object:
--
--     -   __Canonical__: The value in the @Grantee@ object is either the
--         canonical user ID for an AWS account or an origin access
--         identity for an Amazon CloudFront distribution. For more
--         information about canonical user IDs, see Access Control List
--         (ACL) Overview in the Amazon Simple Storage Service Developer
--         Guide. For more information about using CloudFront origin access
--         identities to require that users use CloudFront URLs instead of
--         Amazon S3 URLs, see Using an Origin Access Identity to Restrict
--         Access to Your Amazon S3 Content.
--
--         A canonical user ID is not the same as an AWS account number.
--
--     -   __Email__: The value in the @Grantee@ object is the registered
--         email address of an AWS account.
--
--     -   __Group__: The value in the @Grantee@ object is one of the
--         following predefined Amazon S3 groups: @AllUsers@,
--         @AuthenticatedUsers@, or @LogDelivery@.
--
-- -   __Grantee__: The AWS user or group that you want to have access to
--     transcoded files and playlists. To identify the user or group, you
--     can specify the canonical user ID for an AWS account, an origin
--     access identity for a CloudFront distribution, the registered email
--     address of an AWS account, or a predefined Amazon S3 group
--
-- -   __Access__: The permission that you want to give to the AWS user
--     that you specified in @Grantee@. Permissions are granted on the
--     files that Elastic Transcoder adds to the bucket, including
--     playlists and video files. Valid values include:
--
--     -   @READ@: The grantee can read the objects and metadata for
--         objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--     -   @READ_ACP@: The grantee can read the object ACL for objects that
--         Elastic Transcoder adds to the Amazon S3 bucket.
--
--     -   @WRITE_ACP@: The grantee can write the ACL for the objects that
--         Elastic Transcoder adds to the Amazon S3 bucket.
--
--     -   @FULL_CONTROL@: The grantee has @READ@, @READ_ACP@, and
--         @WRITE_ACP@ permissions for the objects that Elastic Transcoder
--         adds to the Amazon S3 bucket.
--
-- -   __StorageClass__: The Amazon S3 storage class, @Standard@ or
--     @ReducedRedundancy@, that you want Elastic Transcoder to assign to
--     the video files and playlists that it stores in your Amazon S3
--     bucket.
createPipeline_contentConfig :: Lens.Lens' CreatePipeline (Prelude.Maybe PipelineOutputConfig)
createPipeline_contentConfig = Lens.lens (\CreatePipeline' {contentConfig} -> contentConfig) (\s@CreatePipeline' {} a -> s {contentConfig = a} :: CreatePipeline)

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want
-- to notify to report job status.
--
-- To receive notifications, you must also subscribe to the new topic in
-- the Amazon SNS console.
--
-- -   __Progressing__: The topic ARN for the Amazon Simple Notification
--     Service (Amazon SNS) topic that you want to notify when Elastic
--     Transcoder has started to process a job in this pipeline. This is
--     the ARN that Amazon SNS returned when you created the topic. For
--     more information, see Create a Topic in the Amazon Simple
--     Notification Service Developer Guide.
--
-- -   __Complete__: The topic ARN for the Amazon SNS topic that you want
--     to notify when Elastic Transcoder has finished processing a job in
--     this pipeline. This is the ARN that Amazon SNS returned when you
--     created the topic.
--
-- -   __Warning__: The topic ARN for the Amazon SNS topic that you want to
--     notify when Elastic Transcoder encounters a warning condition while
--     processing a job in this pipeline. This is the ARN that Amazon SNS
--     returned when you created the topic.
--
-- -   __Error__: The topic ARN for the Amazon SNS topic that you want to
--     notify when Elastic Transcoder encounters an error condition while
--     processing a job in this pipeline. This is the ARN that Amazon SNS
--     returned when you created the topic.
createPipeline_notifications :: Lens.Lens' CreatePipeline (Prelude.Maybe Notifications)
createPipeline_notifications = Lens.lens (\CreatePipeline' {notifications} -> notifications) (\s@CreatePipeline' {} a -> s {notifications = a} :: CreatePipeline)

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save the
-- transcoded files. (Use this, or use ContentConfig:Bucket plus
-- ThumbnailConfig:Bucket.)
--
-- Specify this value when all of the following are true:
--
-- -   You want to save transcoded files, thumbnails (if any), and
--     playlists (if any) together in one bucket.
--
-- -   You do not want to specify the users or groups who have access to
--     the transcoded files, thumbnails, and playlists.
--
-- -   You do not want to specify the permissions that Elastic Transcoder
--     grants to the files.
--
--     When Elastic Transcoder saves files in @OutputBucket@, it grants
--     full control over the files only to the AWS account that owns the
--     role that is specified by @Role@.
--
-- -   You want to associate the transcoded files and thumbnails with the
--     Amazon S3 Standard storage class.
--
-- If you want to save transcoded files and playlists in one bucket and
-- thumbnails in another bucket, specify which users can access the
-- transcoded files or the permissions the users have, or change the Amazon
-- S3 storage class, omit @OutputBucket@ and specify values for
-- @ContentConfig@ and @ThumbnailConfig@ instead.
createPipeline_outputBucket :: Lens.Lens' CreatePipeline (Prelude.Maybe Prelude.Text)
createPipeline_outputBucket = Lens.lens (\CreatePipeline' {outputBucket} -> outputBucket) (\s@CreatePipeline' {} a -> s {outputBucket = a} :: CreatePipeline)

-- | The @ThumbnailConfig@ object specifies several values, including the
-- Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail
-- files, which users you want to have access to the files, the type of
-- access you want users to have, and the storage class that you want to
-- assign to the files.
--
-- If you specify values for @ContentConfig@, you must also specify values
-- for @ThumbnailConfig@ even if you don\'t want to create thumbnails.
--
-- If you specify values for @ContentConfig@ and @ThumbnailConfig@, omit
-- the @OutputBucket@ object.
--
-- -   __Bucket__: The Amazon S3 bucket in which you want Elastic
--     Transcoder to save thumbnail files.
--
-- -   __Permissions__ (Optional): The @Permissions@ object specifies which
--     users and\/or predefined Amazon S3 groups you want to have access to
--     thumbnail files, and the type of access you want them to have. You
--     can grant permissions to a maximum of 30 users and\/or predefined
--     Amazon S3 groups.
--
-- -   __GranteeType__: Specify the type of value that appears in the
--     Grantee object:
--
--     -   __Canonical__: The value in the @Grantee@ object is either the
--         canonical user ID for an AWS account or an origin access
--         identity for an Amazon CloudFront distribution.
--
--         A canonical user ID is not the same as an AWS account number.
--
--     -   __Email__: The value in the @Grantee@ object is the registered
--         email address of an AWS account.
--
--     -   __Group__: The value in the @Grantee@ object is one of the
--         following predefined Amazon S3 groups: @AllUsers@,
--         @AuthenticatedUsers@, or @LogDelivery@.
--
-- -   __Grantee__: The AWS user or group that you want to have access to
--     thumbnail files. To identify the user or group, you can specify the
--     canonical user ID for an AWS account, an origin access identity for
--     a CloudFront distribution, the registered email address of an AWS
--     account, or a predefined Amazon S3 group.
--
-- -   __Access__: The permission that you want to give to the AWS user
--     that you specified in @Grantee@. Permissions are granted on the
--     thumbnail files that Elastic Transcoder adds to the bucket. Valid
--     values include:
--
--     -   @READ@: The grantee can read the thumbnails and metadata for
--         objects that Elastic Transcoder adds to the Amazon S3 bucket.
--
--     -   @READ_ACP@: The grantee can read the object ACL for thumbnails
--         that Elastic Transcoder adds to the Amazon S3 bucket.
--
--     -   @WRITE_ACP@: The grantee can write the ACL for the thumbnails
--         that Elastic Transcoder adds to the Amazon S3 bucket.
--
--     -   @FULL_CONTROL@: The grantee has @READ@, @READ_ACP@, and
--         @WRITE_ACP@ permissions for the thumbnails that Elastic
--         Transcoder adds to the Amazon S3 bucket.
--
-- -   __StorageClass__: The Amazon S3 storage class, @Standard@ or
--     @ReducedRedundancy@, that you want Elastic Transcoder to assign to
--     the thumbnails that it stores in your Amazon S3 bucket.
createPipeline_thumbnailConfig :: Lens.Lens' CreatePipeline (Prelude.Maybe PipelineOutputConfig)
createPipeline_thumbnailConfig = Lens.lens (\CreatePipeline' {thumbnailConfig} -> thumbnailConfig) (\s@CreatePipeline' {} a -> s {thumbnailConfig = a} :: CreatePipeline)

-- | The name of the pipeline. We recommend that the name be unique within
-- the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters.
createPipeline_name :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_name = Lens.lens (\CreatePipeline' {name} -> name) (\s@CreatePipeline' {} a -> s {name = a} :: CreatePipeline)

-- | The Amazon S3 bucket in which you saved the media files that you want to
-- transcode.
createPipeline_inputBucket :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_inputBucket = Lens.lens (\CreatePipeline' {inputBucket} -> inputBucket) (\s@CreatePipeline' {} a -> s {inputBucket = a} :: CreatePipeline)

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic
-- Transcoder to use to create the pipeline.
createPipeline_role :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_role = Lens.lens (\CreatePipeline' {role'} -> role') (\s@CreatePipeline' {} a -> s {role' = a} :: CreatePipeline)

instance Core.AWSRequest CreatePipeline where
  type
    AWSResponse CreatePipeline =
      CreatePipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Prelude.<$> (x Data..?> "Pipeline")
            Prelude.<*> (x Data..?> "Warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePipeline where
  hashWithSalt _salt CreatePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` awsKmsKeyArn
      `Prelude.hashWithSalt` contentConfig
      `Prelude.hashWithSalt` notifications
      `Prelude.hashWithSalt` outputBucket
      `Prelude.hashWithSalt` thumbnailConfig
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputBucket
      `Prelude.hashWithSalt` role'

instance Prelude.NFData CreatePipeline where
  rnf CreatePipeline' {..} =
    Prelude.rnf awsKmsKeyArn
      `Prelude.seq` Prelude.rnf contentConfig
      `Prelude.seq` Prelude.rnf notifications
      `Prelude.seq` Prelude.rnf outputBucket
      `Prelude.seq` Prelude.rnf thumbnailConfig
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputBucket
      `Prelude.seq` Prelude.rnf role'

instance Data.ToHeaders CreatePipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreatePipeline where
  toJSON CreatePipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AwsKmsKeyArn" Data..=) Prelude.<$> awsKmsKeyArn,
            ("ContentConfig" Data..=) Prelude.<$> contentConfig,
            ("Notifications" Data..=) Prelude.<$> notifications,
            ("OutputBucket" Data..=) Prelude.<$> outputBucket,
            ("ThumbnailConfig" Data..=)
              Prelude.<$> thumbnailConfig,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("InputBucket" Data..= inputBucket),
            Prelude.Just ("Role" Data..= role')
          ]
      )

instance Data.ToPath CreatePipeline where
  toPath = Prelude.const "/2012-09-25/pipelines"

instance Data.ToQuery CreatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | When you create a pipeline, Elastic Transcoder returns the values that
-- you specified in the request.
--
-- /See:/ 'newCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { -- | A section of the response body that provides information about the
    -- pipeline that is created.
    pipeline :: Prelude.Maybe Pipeline,
    -- | Elastic Transcoder returns a warning if the resources used by your
    -- pipeline are not in the same region as the pipeline.
    --
    -- Using resources in the same region, such as your Amazon S3 buckets,
    -- Amazon SNS notification topics, and AWS KMS key, reduces processing time
    -- and prevents cross-regional charges.
    warnings :: Prelude.Maybe [Warning],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipeline', 'createPipelineResponse_pipeline' - A section of the response body that provides information about the
-- pipeline that is created.
--
-- 'warnings', 'createPipelineResponse_warnings' - Elastic Transcoder returns a warning if the resources used by your
-- pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets,
-- Amazon SNS notification topics, and AWS KMS key, reduces processing time
-- and prevents cross-regional charges.
--
-- 'httpStatus', 'createPipelineResponse_httpStatus' - The response's http status code.
newCreatePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePipelineResponse
newCreatePipelineResponse pHttpStatus_ =
  CreatePipelineResponse'
    { pipeline = Prelude.Nothing,
      warnings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A section of the response body that provides information about the
-- pipeline that is created.
createPipelineResponse_pipeline :: Lens.Lens' CreatePipelineResponse (Prelude.Maybe Pipeline)
createPipelineResponse_pipeline = Lens.lens (\CreatePipelineResponse' {pipeline} -> pipeline) (\s@CreatePipelineResponse' {} a -> s {pipeline = a} :: CreatePipelineResponse)

-- | Elastic Transcoder returns a warning if the resources used by your
-- pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets,
-- Amazon SNS notification topics, and AWS KMS key, reduces processing time
-- and prevents cross-regional charges.
createPipelineResponse_warnings :: Lens.Lens' CreatePipelineResponse (Prelude.Maybe [Warning])
createPipelineResponse_warnings = Lens.lens (\CreatePipelineResponse' {warnings} -> warnings) (\s@CreatePipelineResponse' {} a -> s {warnings = a} :: CreatePipelineResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createPipelineResponse_httpStatus :: Lens.Lens' CreatePipelineResponse Prelude.Int
createPipelineResponse_httpStatus = Lens.lens (\CreatePipelineResponse' {httpStatus} -> httpStatus) (\s@CreatePipelineResponse' {} a -> s {httpStatus = a} :: CreatePipelineResponse)

instance Prelude.NFData CreatePipelineResponse where
  rnf CreatePipelineResponse' {..} =
    Prelude.rnf pipeline
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf httpStatus
