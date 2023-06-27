{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticTranscoder.Types.Pipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.Pipeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types.Notifications
import Amazonka.ElasticTranscoder.Types.PipelineOutputConfig
import qualified Amazonka.Prelude as Prelude

-- | The pipeline (queue) that is used to manage jobs.
--
-- /See:/ 'newPipeline' smart constructor.
data Pipeline = Pipeline'
  { -- | The Amazon Resource Name (ARN) for the pipeline.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The AWS Key Management Service (AWS KMS) key that you want to use with
    -- this pipeline.
    --
    -- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@, you
    -- don\'t need to provide a key with your job because a default key, known
    -- as an AWS-KMS key, is created for you automatically. You need to provide
    -- an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if
    -- you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@, @aes-ctr@, or
    -- @aes-gcm@.
    awsKmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the Amazon S3 bucket in which you want Elastic
    -- Transcoder to save transcoded files and playlists. Either you specify
    -- both @ContentConfig@ and @ThumbnailConfig@, or you specify
    -- @OutputBucket@.
    --
    -- -   __Bucket__: The Amazon S3 bucket in which you want Elastic
    --     Transcoder to save transcoded files and playlists.
    --
    -- -   __Permissions__: A list of the users and\/or predefined Amazon S3
    --     groups you want to have access to transcoded files and playlists,
    --     and the type of access that you want them to have.
    --
    --     -   GranteeType: The type of value that appears in the @Grantee@
    --         object:
    --
    --         -   @Canonical@: Either the canonical user ID for an AWS account
    --             or an origin access identity for an Amazon CloudFront
    --             distribution.
    --
    --         -   @Email@: The registered email address of an AWS account.
    --
    --         -   @Group@: One of the following predefined Amazon S3 groups:
    --             @AllUsers@, @AuthenticatedUsers@, or @LogDelivery@.
    --
    --     -   @Grantee@: The AWS user or group that you want to have access to
    --         transcoded files and playlists.
    --
    --     -   @Access@: The permission that you want to give to the AWS user
    --         that is listed in @Grantee@. Valid values include:
    --
    --         -   @READ@: The grantee can read the objects and metadata for
    --             objects that Elastic Transcoder adds to the Amazon S3
    --             bucket.
    --
    --         -   @READ_ACP@: The grantee can read the object ACL for objects
    --             that Elastic Transcoder adds to the Amazon S3 bucket.
    --
    --         -   @WRITE_ACP@: The grantee can write the ACL for the objects
    --             that Elastic Transcoder adds to the Amazon S3 bucket.
    --
    --         -   @FULL_CONTROL@: The grantee has @READ@, @READ_ACP@, and
    --             @WRITE_ACP@ permissions for the objects that Elastic
    --             Transcoder adds to the Amazon S3 bucket.
    --
    -- -   __StorageClass__: The Amazon S3 storage class, Standard or
    --     ReducedRedundancy, that you want Elastic Transcoder to assign to the
    --     video files and playlists that it stores in your Amazon S3 bucket.
    contentConfig :: Prelude.Maybe PipelineOutputConfig,
    -- | The identifier for the pipeline. You use this value to identify the
    -- pipeline in which you want to perform a variety of operations, such as
    -- creating a job or a preset.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket from which Elastic Transcoder gets media files for
    -- transcoding and the graphics files, if any, that you want to use for
    -- watermarks.
    inputBucket :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline. We recommend that the name be unique within
    -- the AWS account, but uniqueness is not enforced.
    --
    -- Constraints: Maximum 40 characters
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Simple Notification Service (Amazon SNS) topic that you want
    -- to notify to report job status.
    --
    -- To receive notifications, you must also subscribe to the new topic in
    -- the Amazon SNS console.
    --
    -- -   __Progressing__ (optional): The Amazon Simple Notification Service
    --     (Amazon SNS) topic that you want to notify when Elastic Transcoder
    --     has started to process the job.
    --
    -- -   __Complete__ (optional): The Amazon SNS topic that you want to
    --     notify when Elastic Transcoder has finished processing the job.
    --
    -- -   __Warning__ (optional): The Amazon SNS topic that you want to notify
    --     when Elastic Transcoder encounters a warning condition.
    --
    -- -   __Error__ (optional): The Amazon SNS topic that you want to notify
    --     when Elastic Transcoder encounters an error condition.
    notifications :: Prelude.Maybe Notifications,
    -- | The Amazon S3 bucket in which you want Elastic Transcoder to save
    -- transcoded files, thumbnails, and playlists. Either you specify this
    -- value, or you specify both @ContentConfig@ and @ThumbnailConfig@.
    outputBucket :: Prelude.Maybe Prelude.Text,
    -- | The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder
    -- uses to transcode jobs for this pipeline.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The current status of the pipeline:
    --
    -- -   @Active@: The pipeline is processing jobs.
    --
    -- -   @Paused@: The pipeline is not currently processing jobs.
    status :: Prelude.Maybe Prelude.Text,
    -- | Information about the Amazon S3 bucket in which you want Elastic
    -- Transcoder to save thumbnail files. Either you specify both
    -- @ContentConfig@ and @ThumbnailConfig@, or you specify @OutputBucket@.
    --
    -- -   @Bucket@: The Amazon S3 bucket in which you want Elastic Transcoder
    --     to save thumbnail files.
    --
    -- -   @Permissions@: A list of the users and\/or predefined Amazon S3
    --     groups you want to have access to thumbnail files, and the type of
    --     access that you want them to have.
    --
    --     -   GranteeType: The type of value that appears in the Grantee
    --         object:
    --
    --         -   @Canonical@: Either the canonical user ID for an AWS account
    --             or an origin access identity for an Amazon CloudFront
    --             distribution.
    --
    --             A canonical user ID is not the same as an AWS account
    --             number.
    --
    --         -   @Email@: The registered email address of an AWS account.
    --
    --         -   @Group@: One of the following predefined Amazon S3 groups:
    --             @AllUsers@, @AuthenticatedUsers@, or @LogDelivery@.
    --
    --     -   @Grantee@: The AWS user or group that you want to have access to
    --         thumbnail files.
    --
    --     -   Access: The permission that you want to give to the AWS user
    --         that is listed in Grantee. Valid values include:
    --
    --         -   @READ@: The grantee can read the thumbnails and metadata for
    --             thumbnails that Elastic Transcoder adds to the Amazon S3
    --             bucket.
    --
    --         -   @READ_ACP@: The grantee can read the object ACL for
    --             thumbnails that Elastic Transcoder adds to the Amazon S3
    --             bucket.
    --
    --         -   @WRITE_ACP@: The grantee can write the ACL for the
    --             thumbnails that Elastic Transcoder adds to the Amazon S3
    --             bucket.
    --
    --         -   @FULL_CONTROL@: The grantee has READ, READ_ACP, and
    --             WRITE_ACP permissions for the thumbnails that Elastic
    --             Transcoder adds to the Amazon S3 bucket.
    --
    -- -   @StorageClass@: The Amazon S3 storage class, @Standard@ or
    --     @ReducedRedundancy@, that you want Elastic Transcoder to assign to
    --     the thumbnails that it stores in your Amazon S3 bucket.
    thumbnailConfig :: Prelude.Maybe PipelineOutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Pipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'pipeline_arn' - The Amazon Resource Name (ARN) for the pipeline.
--
-- 'awsKmsKeyArn', 'pipeline_awsKmsKeyArn' - The AWS Key Management Service (AWS KMS) key that you want to use with
-- this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@, you
-- don\'t need to provide a key with your job because a default key, known
-- as an AWS-KMS key, is created for you automatically. You need to provide
-- an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if
-- you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@, @aes-ctr@, or
-- @aes-gcm@.
--
-- 'contentConfig', 'pipeline_contentConfig' - Information about the Amazon S3 bucket in which you want Elastic
-- Transcoder to save transcoded files and playlists. Either you specify
-- both @ContentConfig@ and @ThumbnailConfig@, or you specify
-- @OutputBucket@.
--
-- -   __Bucket__: The Amazon S3 bucket in which you want Elastic
--     Transcoder to save transcoded files and playlists.
--
-- -   __Permissions__: A list of the users and\/or predefined Amazon S3
--     groups you want to have access to transcoded files and playlists,
--     and the type of access that you want them to have.
--
--     -   GranteeType: The type of value that appears in the @Grantee@
--         object:
--
--         -   @Canonical@: Either the canonical user ID for an AWS account
--             or an origin access identity for an Amazon CloudFront
--             distribution.
--
--         -   @Email@: The registered email address of an AWS account.
--
--         -   @Group@: One of the following predefined Amazon S3 groups:
--             @AllUsers@, @AuthenticatedUsers@, or @LogDelivery@.
--
--     -   @Grantee@: The AWS user or group that you want to have access to
--         transcoded files and playlists.
--
--     -   @Access@: The permission that you want to give to the AWS user
--         that is listed in @Grantee@. Valid values include:
--
--         -   @READ@: The grantee can read the objects and metadata for
--             objects that Elastic Transcoder adds to the Amazon S3
--             bucket.
--
--         -   @READ_ACP@: The grantee can read the object ACL for objects
--             that Elastic Transcoder adds to the Amazon S3 bucket.
--
--         -   @WRITE_ACP@: The grantee can write the ACL for the objects
--             that Elastic Transcoder adds to the Amazon S3 bucket.
--
--         -   @FULL_CONTROL@: The grantee has @READ@, @READ_ACP@, and
--             @WRITE_ACP@ permissions for the objects that Elastic
--             Transcoder adds to the Amazon S3 bucket.
--
-- -   __StorageClass__: The Amazon S3 storage class, Standard or
--     ReducedRedundancy, that you want Elastic Transcoder to assign to the
--     video files and playlists that it stores in your Amazon S3 bucket.
--
-- 'id', 'pipeline_id' - The identifier for the pipeline. You use this value to identify the
-- pipeline in which you want to perform a variety of operations, such as
-- creating a job or a preset.
--
-- 'inputBucket', 'pipeline_inputBucket' - The Amazon S3 bucket from which Elastic Transcoder gets media files for
-- transcoding and the graphics files, if any, that you want to use for
-- watermarks.
--
-- 'name', 'pipeline_name' - The name of the pipeline. We recommend that the name be unique within
-- the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
--
-- 'notifications', 'pipeline_notifications' - The Amazon Simple Notification Service (Amazon SNS) topic that you want
-- to notify to report job status.
--
-- To receive notifications, you must also subscribe to the new topic in
-- the Amazon SNS console.
--
-- -   __Progressing__ (optional): The Amazon Simple Notification Service
--     (Amazon SNS) topic that you want to notify when Elastic Transcoder
--     has started to process the job.
--
-- -   __Complete__ (optional): The Amazon SNS topic that you want to
--     notify when Elastic Transcoder has finished processing the job.
--
-- -   __Warning__ (optional): The Amazon SNS topic that you want to notify
--     when Elastic Transcoder encounters a warning condition.
--
-- -   __Error__ (optional): The Amazon SNS topic that you want to notify
--     when Elastic Transcoder encounters an error condition.
--
-- 'outputBucket', 'pipeline_outputBucket' - The Amazon S3 bucket in which you want Elastic Transcoder to save
-- transcoded files, thumbnails, and playlists. Either you specify this
-- value, or you specify both @ContentConfig@ and @ThumbnailConfig@.
--
-- 'role'', 'pipeline_role' - The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder
-- uses to transcode jobs for this pipeline.
--
-- 'status', 'pipeline_status' - The current status of the pipeline:
--
-- -   @Active@: The pipeline is processing jobs.
--
-- -   @Paused@: The pipeline is not currently processing jobs.
--
-- 'thumbnailConfig', 'pipeline_thumbnailConfig' - Information about the Amazon S3 bucket in which you want Elastic
-- Transcoder to save thumbnail files. Either you specify both
-- @ContentConfig@ and @ThumbnailConfig@, or you specify @OutputBucket@.
--
-- -   @Bucket@: The Amazon S3 bucket in which you want Elastic Transcoder
--     to save thumbnail files.
--
-- -   @Permissions@: A list of the users and\/or predefined Amazon S3
--     groups you want to have access to thumbnail files, and the type of
--     access that you want them to have.
--
--     -   GranteeType: The type of value that appears in the Grantee
--         object:
--
--         -   @Canonical@: Either the canonical user ID for an AWS account
--             or an origin access identity for an Amazon CloudFront
--             distribution.
--
--             A canonical user ID is not the same as an AWS account
--             number.
--
--         -   @Email@: The registered email address of an AWS account.
--
--         -   @Group@: One of the following predefined Amazon S3 groups:
--             @AllUsers@, @AuthenticatedUsers@, or @LogDelivery@.
--
--     -   @Grantee@: The AWS user or group that you want to have access to
--         thumbnail files.
--
--     -   Access: The permission that you want to give to the AWS user
--         that is listed in Grantee. Valid values include:
--
--         -   @READ@: The grantee can read the thumbnails and metadata for
--             thumbnails that Elastic Transcoder adds to the Amazon S3
--             bucket.
--
--         -   @READ_ACP@: The grantee can read the object ACL for
--             thumbnails that Elastic Transcoder adds to the Amazon S3
--             bucket.
--
--         -   @WRITE_ACP@: The grantee can write the ACL for the
--             thumbnails that Elastic Transcoder adds to the Amazon S3
--             bucket.
--
--         -   @FULL_CONTROL@: The grantee has READ, READ_ACP, and
--             WRITE_ACP permissions for the thumbnails that Elastic
--             Transcoder adds to the Amazon S3 bucket.
--
-- -   @StorageClass@: The Amazon S3 storage class, @Standard@ or
--     @ReducedRedundancy@, that you want Elastic Transcoder to assign to
--     the thumbnails that it stores in your Amazon S3 bucket.
newPipeline ::
  Pipeline
newPipeline =
  Pipeline'
    { arn = Prelude.Nothing,
      awsKmsKeyArn = Prelude.Nothing,
      contentConfig = Prelude.Nothing,
      id = Prelude.Nothing,
      inputBucket = Prelude.Nothing,
      name = Prelude.Nothing,
      notifications = Prelude.Nothing,
      outputBucket = Prelude.Nothing,
      role' = Prelude.Nothing,
      status = Prelude.Nothing,
      thumbnailConfig = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the pipeline.
pipeline_arn :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_arn = Lens.lens (\Pipeline' {arn} -> arn) (\s@Pipeline' {} a -> s {arn = a} :: Pipeline)

-- | The AWS Key Management Service (AWS KMS) key that you want to use with
-- this pipeline.
--
-- If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@, you
-- don\'t need to provide a key with your job because a default key, known
-- as an AWS-KMS key, is created for you automatically. You need to provide
-- an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if
-- you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@, @aes-ctr@, or
-- @aes-gcm@.
pipeline_awsKmsKeyArn :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_awsKmsKeyArn = Lens.lens (\Pipeline' {awsKmsKeyArn} -> awsKmsKeyArn) (\s@Pipeline' {} a -> s {awsKmsKeyArn = a} :: Pipeline)

-- | Information about the Amazon S3 bucket in which you want Elastic
-- Transcoder to save transcoded files and playlists. Either you specify
-- both @ContentConfig@ and @ThumbnailConfig@, or you specify
-- @OutputBucket@.
--
-- -   __Bucket__: The Amazon S3 bucket in which you want Elastic
--     Transcoder to save transcoded files and playlists.
--
-- -   __Permissions__: A list of the users and\/or predefined Amazon S3
--     groups you want to have access to transcoded files and playlists,
--     and the type of access that you want them to have.
--
--     -   GranteeType: The type of value that appears in the @Grantee@
--         object:
--
--         -   @Canonical@: Either the canonical user ID for an AWS account
--             or an origin access identity for an Amazon CloudFront
--             distribution.
--
--         -   @Email@: The registered email address of an AWS account.
--
--         -   @Group@: One of the following predefined Amazon S3 groups:
--             @AllUsers@, @AuthenticatedUsers@, or @LogDelivery@.
--
--     -   @Grantee@: The AWS user or group that you want to have access to
--         transcoded files and playlists.
--
--     -   @Access@: The permission that you want to give to the AWS user
--         that is listed in @Grantee@. Valid values include:
--
--         -   @READ@: The grantee can read the objects and metadata for
--             objects that Elastic Transcoder adds to the Amazon S3
--             bucket.
--
--         -   @READ_ACP@: The grantee can read the object ACL for objects
--             that Elastic Transcoder adds to the Amazon S3 bucket.
--
--         -   @WRITE_ACP@: The grantee can write the ACL for the objects
--             that Elastic Transcoder adds to the Amazon S3 bucket.
--
--         -   @FULL_CONTROL@: The grantee has @READ@, @READ_ACP@, and
--             @WRITE_ACP@ permissions for the objects that Elastic
--             Transcoder adds to the Amazon S3 bucket.
--
-- -   __StorageClass__: The Amazon S3 storage class, Standard or
--     ReducedRedundancy, that you want Elastic Transcoder to assign to the
--     video files and playlists that it stores in your Amazon S3 bucket.
pipeline_contentConfig :: Lens.Lens' Pipeline (Prelude.Maybe PipelineOutputConfig)
pipeline_contentConfig = Lens.lens (\Pipeline' {contentConfig} -> contentConfig) (\s@Pipeline' {} a -> s {contentConfig = a} :: Pipeline)

-- | The identifier for the pipeline. You use this value to identify the
-- pipeline in which you want to perform a variety of operations, such as
-- creating a job or a preset.
pipeline_id :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_id = Lens.lens (\Pipeline' {id} -> id) (\s@Pipeline' {} a -> s {id = a} :: Pipeline)

-- | The Amazon S3 bucket from which Elastic Transcoder gets media files for
-- transcoding and the graphics files, if any, that you want to use for
-- watermarks.
pipeline_inputBucket :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_inputBucket = Lens.lens (\Pipeline' {inputBucket} -> inputBucket) (\s@Pipeline' {} a -> s {inputBucket = a} :: Pipeline)

-- | The name of the pipeline. We recommend that the name be unique within
-- the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
pipeline_name :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_name = Lens.lens (\Pipeline' {name} -> name) (\s@Pipeline' {} a -> s {name = a} :: Pipeline)

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want
-- to notify to report job status.
--
-- To receive notifications, you must also subscribe to the new topic in
-- the Amazon SNS console.
--
-- -   __Progressing__ (optional): The Amazon Simple Notification Service
--     (Amazon SNS) topic that you want to notify when Elastic Transcoder
--     has started to process the job.
--
-- -   __Complete__ (optional): The Amazon SNS topic that you want to
--     notify when Elastic Transcoder has finished processing the job.
--
-- -   __Warning__ (optional): The Amazon SNS topic that you want to notify
--     when Elastic Transcoder encounters a warning condition.
--
-- -   __Error__ (optional): The Amazon SNS topic that you want to notify
--     when Elastic Transcoder encounters an error condition.
pipeline_notifications :: Lens.Lens' Pipeline (Prelude.Maybe Notifications)
pipeline_notifications = Lens.lens (\Pipeline' {notifications} -> notifications) (\s@Pipeline' {} a -> s {notifications = a} :: Pipeline)

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save
-- transcoded files, thumbnails, and playlists. Either you specify this
-- value, or you specify both @ContentConfig@ and @ThumbnailConfig@.
pipeline_outputBucket :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_outputBucket = Lens.lens (\Pipeline' {outputBucket} -> outputBucket) (\s@Pipeline' {} a -> s {outputBucket = a} :: Pipeline)

-- | The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder
-- uses to transcode jobs for this pipeline.
pipeline_role :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_role = Lens.lens (\Pipeline' {role'} -> role') (\s@Pipeline' {} a -> s {role' = a} :: Pipeline)

-- | The current status of the pipeline:
--
-- -   @Active@: The pipeline is processing jobs.
--
-- -   @Paused@: The pipeline is not currently processing jobs.
pipeline_status :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_status = Lens.lens (\Pipeline' {status} -> status) (\s@Pipeline' {} a -> s {status = a} :: Pipeline)

-- | Information about the Amazon S3 bucket in which you want Elastic
-- Transcoder to save thumbnail files. Either you specify both
-- @ContentConfig@ and @ThumbnailConfig@, or you specify @OutputBucket@.
--
-- -   @Bucket@: The Amazon S3 bucket in which you want Elastic Transcoder
--     to save thumbnail files.
--
-- -   @Permissions@: A list of the users and\/or predefined Amazon S3
--     groups you want to have access to thumbnail files, and the type of
--     access that you want them to have.
--
--     -   GranteeType: The type of value that appears in the Grantee
--         object:
--
--         -   @Canonical@: Either the canonical user ID for an AWS account
--             or an origin access identity for an Amazon CloudFront
--             distribution.
--
--             A canonical user ID is not the same as an AWS account
--             number.
--
--         -   @Email@: The registered email address of an AWS account.
--
--         -   @Group@: One of the following predefined Amazon S3 groups:
--             @AllUsers@, @AuthenticatedUsers@, or @LogDelivery@.
--
--     -   @Grantee@: The AWS user or group that you want to have access to
--         thumbnail files.
--
--     -   Access: The permission that you want to give to the AWS user
--         that is listed in Grantee. Valid values include:
--
--         -   @READ@: The grantee can read the thumbnails and metadata for
--             thumbnails that Elastic Transcoder adds to the Amazon S3
--             bucket.
--
--         -   @READ_ACP@: The grantee can read the object ACL for
--             thumbnails that Elastic Transcoder adds to the Amazon S3
--             bucket.
--
--         -   @WRITE_ACP@: The grantee can write the ACL for the
--             thumbnails that Elastic Transcoder adds to the Amazon S3
--             bucket.
--
--         -   @FULL_CONTROL@: The grantee has READ, READ_ACP, and
--             WRITE_ACP permissions for the thumbnails that Elastic
--             Transcoder adds to the Amazon S3 bucket.
--
-- -   @StorageClass@: The Amazon S3 storage class, @Standard@ or
--     @ReducedRedundancy@, that you want Elastic Transcoder to assign to
--     the thumbnails that it stores in your Amazon S3 bucket.
pipeline_thumbnailConfig :: Lens.Lens' Pipeline (Prelude.Maybe PipelineOutputConfig)
pipeline_thumbnailConfig = Lens.lens (\Pipeline' {thumbnailConfig} -> thumbnailConfig) (\s@Pipeline' {} a -> s {thumbnailConfig = a} :: Pipeline)

instance Data.FromJSON Pipeline where
  parseJSON =
    Data.withObject
      "Pipeline"
      ( \x ->
          Pipeline'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "AwsKmsKeyArn")
            Prelude.<*> (x Data..:? "ContentConfig")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "InputBucket")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Notifications")
            Prelude.<*> (x Data..:? "OutputBucket")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "ThumbnailConfig")
      )

instance Prelude.Hashable Pipeline where
  hashWithSalt _salt Pipeline' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` awsKmsKeyArn
      `Prelude.hashWithSalt` contentConfig
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` inputBucket
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` notifications
      `Prelude.hashWithSalt` outputBucket
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` thumbnailConfig

instance Prelude.NFData Pipeline where
  rnf Pipeline' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf awsKmsKeyArn
      `Prelude.seq` Prelude.rnf contentConfig
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf inputBucket
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf notifications
      `Prelude.seq` Prelude.rnf outputBucket
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf thumbnailConfig
