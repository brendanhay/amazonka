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
-- Module      : Amazonka.ElasticTranscoder.UpdatePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @UpdatePipeline@ operation to update settings for a pipeline.
--
-- When you change pipeline settings, your changes take effect immediately.
-- Jobs that you have already submitted and that Elastic Transcoder has not
-- started to process are affected in addition to jobs that you submit
-- after you change settings.
module Amazonka.ElasticTranscoder.UpdatePipeline
  ( -- * Creating a Request
    UpdatePipeline (..),
    newUpdatePipeline,

    -- * Request Lenses
    updatePipeline_awsKmsKeyArn,
    updatePipeline_contentConfig,
    updatePipeline_inputBucket,
    updatePipeline_name,
    updatePipeline_notifications,
    updatePipeline_role,
    updatePipeline_thumbnailConfig,
    updatePipeline_id,

    -- * Destructuring the Response
    UpdatePipelineResponse (..),
    newUpdatePipelineResponse,

    -- * Response Lenses
    updatePipelineResponse_pipeline,
    updatePipelineResponse_warnings,
    updatePipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The @UpdatePipelineRequest@ structure.
--
-- /See:/ 'newUpdatePipeline' smart constructor.
data UpdatePipeline = UpdatePipeline'
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
    -- | The Amazon S3 bucket in which you saved the media files that you want to
    -- transcode and the graphics that you want to use as watermarks.
    inputBucket :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline. We recommend that the name be unique within
    -- the AWS account, but uniqueness is not enforced.
    --
    -- Constraints: Maximum 40 characters
    name :: Prelude.Maybe Prelude.Text,
    -- | The topic ARN for the Amazon Simple Notification Service (Amazon SNS)
    -- topic that you want to notify to report job status.
    --
    -- To receive notifications, you must also subscribe to the new topic in
    -- the Amazon SNS console.
    --
    -- -   __Progressing__: The topic ARN for the Amazon Simple Notification
    --     Service (Amazon SNS) topic that you want to notify when Elastic
    --     Transcoder has started to process jobs that are added to this
    --     pipeline. This is the ARN that Amazon SNS returned when you created
    --     the topic.
    --
    -- -   __Complete__: The topic ARN for the Amazon SNS topic that you want
    --     to notify when Elastic Transcoder has finished processing a job.
    --     This is the ARN that Amazon SNS returned when you created the topic.
    --
    -- -   __Warning__: The topic ARN for the Amazon SNS topic that you want to
    --     notify when Elastic Transcoder encounters a warning condition. This
    --     is the ARN that Amazon SNS returned when you created the topic.
    --
    -- -   __Error__: The topic ARN for the Amazon SNS topic that you want to
    --     notify when Elastic Transcoder encounters an error condition. This
    --     is the ARN that Amazon SNS returned when you created the topic.
    notifications :: Prelude.Maybe Notifications,
    -- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic
    -- Transcoder to use to transcode jobs for this pipeline.
    role' :: Prelude.Maybe Prelude.Text,
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
    -- | The ID of the pipeline that you want to update.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsKmsKeyArn', 'updatePipeline_awsKmsKeyArn' - The AWS Key Management Service (AWS KMS) key that you want to use with
-- this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@, you
-- don\'t need to provide a key with your job because a default key, known
-- as an AWS-KMS key, is created for you automatically. You need to provide
-- an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if
-- you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@, @aes-ctr@, or
-- @aes-gcm@.
--
-- 'contentConfig', 'updatePipeline_contentConfig' - The optional @ContentConfig@ object specifies information about the
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
-- 'inputBucket', 'updatePipeline_inputBucket' - The Amazon S3 bucket in which you saved the media files that you want to
-- transcode and the graphics that you want to use as watermarks.
--
-- 'name', 'updatePipeline_name' - The name of the pipeline. We recommend that the name be unique within
-- the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
--
-- 'notifications', 'updatePipeline_notifications' - The topic ARN for the Amazon Simple Notification Service (Amazon SNS)
-- topic that you want to notify to report job status.
--
-- To receive notifications, you must also subscribe to the new topic in
-- the Amazon SNS console.
--
-- -   __Progressing__: The topic ARN for the Amazon Simple Notification
--     Service (Amazon SNS) topic that you want to notify when Elastic
--     Transcoder has started to process jobs that are added to this
--     pipeline. This is the ARN that Amazon SNS returned when you created
--     the topic.
--
-- -   __Complete__: The topic ARN for the Amazon SNS topic that you want
--     to notify when Elastic Transcoder has finished processing a job.
--     This is the ARN that Amazon SNS returned when you created the topic.
--
-- -   __Warning__: The topic ARN for the Amazon SNS topic that you want to
--     notify when Elastic Transcoder encounters a warning condition. This
--     is the ARN that Amazon SNS returned when you created the topic.
--
-- -   __Error__: The topic ARN for the Amazon SNS topic that you want to
--     notify when Elastic Transcoder encounters an error condition. This
--     is the ARN that Amazon SNS returned when you created the topic.
--
-- 'role'', 'updatePipeline_role' - The IAM Amazon Resource Name (ARN) for the role that you want Elastic
-- Transcoder to use to transcode jobs for this pipeline.
--
-- 'thumbnailConfig', 'updatePipeline_thumbnailConfig' - The @ThumbnailConfig@ object specifies several values, including the
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
-- 'id', 'updatePipeline_id' - The ID of the pipeline that you want to update.
newUpdatePipeline ::
  -- | 'id'
  Prelude.Text ->
  UpdatePipeline
newUpdatePipeline pId_ =
  UpdatePipeline'
    { awsKmsKeyArn = Prelude.Nothing,
      contentConfig = Prelude.Nothing,
      inputBucket = Prelude.Nothing,
      name = Prelude.Nothing,
      notifications = Prelude.Nothing,
      role' = Prelude.Nothing,
      thumbnailConfig = Prelude.Nothing,
      id = pId_
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
updatePipeline_awsKmsKeyArn :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_awsKmsKeyArn = Lens.lens (\UpdatePipeline' {awsKmsKeyArn} -> awsKmsKeyArn) (\s@UpdatePipeline' {} a -> s {awsKmsKeyArn = a} :: UpdatePipeline)

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
updatePipeline_contentConfig :: Lens.Lens' UpdatePipeline (Prelude.Maybe PipelineOutputConfig)
updatePipeline_contentConfig = Lens.lens (\UpdatePipeline' {contentConfig} -> contentConfig) (\s@UpdatePipeline' {} a -> s {contentConfig = a} :: UpdatePipeline)

-- | The Amazon S3 bucket in which you saved the media files that you want to
-- transcode and the graphics that you want to use as watermarks.
updatePipeline_inputBucket :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_inputBucket = Lens.lens (\UpdatePipeline' {inputBucket} -> inputBucket) (\s@UpdatePipeline' {} a -> s {inputBucket = a} :: UpdatePipeline)

-- | The name of the pipeline. We recommend that the name be unique within
-- the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
updatePipeline_name :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_name = Lens.lens (\UpdatePipeline' {name} -> name) (\s@UpdatePipeline' {} a -> s {name = a} :: UpdatePipeline)

-- | The topic ARN for the Amazon Simple Notification Service (Amazon SNS)
-- topic that you want to notify to report job status.
--
-- To receive notifications, you must also subscribe to the new topic in
-- the Amazon SNS console.
--
-- -   __Progressing__: The topic ARN for the Amazon Simple Notification
--     Service (Amazon SNS) topic that you want to notify when Elastic
--     Transcoder has started to process jobs that are added to this
--     pipeline. This is the ARN that Amazon SNS returned when you created
--     the topic.
--
-- -   __Complete__: The topic ARN for the Amazon SNS topic that you want
--     to notify when Elastic Transcoder has finished processing a job.
--     This is the ARN that Amazon SNS returned when you created the topic.
--
-- -   __Warning__: The topic ARN for the Amazon SNS topic that you want to
--     notify when Elastic Transcoder encounters a warning condition. This
--     is the ARN that Amazon SNS returned when you created the topic.
--
-- -   __Error__: The topic ARN for the Amazon SNS topic that you want to
--     notify when Elastic Transcoder encounters an error condition. This
--     is the ARN that Amazon SNS returned when you created the topic.
updatePipeline_notifications :: Lens.Lens' UpdatePipeline (Prelude.Maybe Notifications)
updatePipeline_notifications = Lens.lens (\UpdatePipeline' {notifications} -> notifications) (\s@UpdatePipeline' {} a -> s {notifications = a} :: UpdatePipeline)

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic
-- Transcoder to use to transcode jobs for this pipeline.
updatePipeline_role :: Lens.Lens' UpdatePipeline (Prelude.Maybe Prelude.Text)
updatePipeline_role = Lens.lens (\UpdatePipeline' {role'} -> role') (\s@UpdatePipeline' {} a -> s {role' = a} :: UpdatePipeline)

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
updatePipeline_thumbnailConfig :: Lens.Lens' UpdatePipeline (Prelude.Maybe PipelineOutputConfig)
updatePipeline_thumbnailConfig = Lens.lens (\UpdatePipeline' {thumbnailConfig} -> thumbnailConfig) (\s@UpdatePipeline' {} a -> s {thumbnailConfig = a} :: UpdatePipeline)

-- | The ID of the pipeline that you want to update.
updatePipeline_id :: Lens.Lens' UpdatePipeline Prelude.Text
updatePipeline_id = Lens.lens (\UpdatePipeline' {id} -> id) (\s@UpdatePipeline' {} a -> s {id = a} :: UpdatePipeline)

instance Core.AWSRequest UpdatePipeline where
  type
    AWSResponse UpdatePipeline =
      UpdatePipelineResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePipelineResponse'
            Prelude.<$> (x Data..?> "Pipeline")
            Prelude.<*> (x Data..?> "Warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePipeline where
  hashWithSalt _salt UpdatePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` awsKmsKeyArn
      `Prelude.hashWithSalt` contentConfig
      `Prelude.hashWithSalt` inputBucket
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` notifications
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` thumbnailConfig
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdatePipeline where
  rnf UpdatePipeline' {..} =
    Prelude.rnf awsKmsKeyArn
      `Prelude.seq` Prelude.rnf contentConfig
      `Prelude.seq` Prelude.rnf inputBucket
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf notifications
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf thumbnailConfig
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdatePipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdatePipeline where
  toJSON UpdatePipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AwsKmsKeyArn" Data..=) Prelude.<$> awsKmsKeyArn,
            ("ContentConfig" Data..=) Prelude.<$> contentConfig,
            ("InputBucket" Data..=) Prelude.<$> inputBucket,
            ("Name" Data..=) Prelude.<$> name,
            ("Notifications" Data..=) Prelude.<$> notifications,
            ("Role" Data..=) Prelude.<$> role',
            ("ThumbnailConfig" Data..=)
              Prelude.<$> thumbnailConfig
          ]
      )

instance Data.ToPath UpdatePipeline where
  toPath UpdatePipeline' {..} =
    Prelude.mconcat
      ["/2012-09-25/pipelines/", Data.toBS id]

instance Data.ToQuery UpdatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | When you update a pipeline, Elastic Transcoder returns the values that
-- you specified in the request.
--
-- /See:/ 'newUpdatePipelineResponse' smart constructor.
data UpdatePipelineResponse = UpdatePipelineResponse'
  { -- | The pipeline updated by this @UpdatePipelineResponse@ call.
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
-- Create a value of 'UpdatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipeline', 'updatePipelineResponse_pipeline' - The pipeline updated by this @UpdatePipelineResponse@ call.
--
-- 'warnings', 'updatePipelineResponse_warnings' - Elastic Transcoder returns a warning if the resources used by your
-- pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets,
-- Amazon SNS notification topics, and AWS KMS key, reduces processing time
-- and prevents cross-regional charges.
--
-- 'httpStatus', 'updatePipelineResponse_httpStatus' - The response's http status code.
newUpdatePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePipelineResponse
newUpdatePipelineResponse pHttpStatus_ =
  UpdatePipelineResponse'
    { pipeline = Prelude.Nothing,
      warnings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pipeline updated by this @UpdatePipelineResponse@ call.
updatePipelineResponse_pipeline :: Lens.Lens' UpdatePipelineResponse (Prelude.Maybe Pipeline)
updatePipelineResponse_pipeline = Lens.lens (\UpdatePipelineResponse' {pipeline} -> pipeline) (\s@UpdatePipelineResponse' {} a -> s {pipeline = a} :: UpdatePipelineResponse)

-- | Elastic Transcoder returns a warning if the resources used by your
-- pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets,
-- Amazon SNS notification topics, and AWS KMS key, reduces processing time
-- and prevents cross-regional charges.
updatePipelineResponse_warnings :: Lens.Lens' UpdatePipelineResponse (Prelude.Maybe [Warning])
updatePipelineResponse_warnings = Lens.lens (\UpdatePipelineResponse' {warnings} -> warnings) (\s@UpdatePipelineResponse' {} a -> s {warnings = a} :: UpdatePipelineResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updatePipelineResponse_httpStatus :: Lens.Lens' UpdatePipelineResponse Prelude.Int
updatePipelineResponse_httpStatus = Lens.lens (\UpdatePipelineResponse' {httpStatus} -> httpStatus) (\s@UpdatePipelineResponse' {} a -> s {httpStatus = a} :: UpdatePipelineResponse)

instance Prelude.NFData UpdatePipelineResponse where
  rnf UpdatePipelineResponse' {..} =
    Prelude.rnf pipeline
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf httpStatus
