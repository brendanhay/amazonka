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
-- Module      : Amazonka.Lambda.UpdateFunctionCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Lambda function\'s code. If code signing is enabled for the
-- function, the code package must be signed by a trusted publisher. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-trustedcode.html Configuring code signing>.
--
-- The function\'s code is locked when you publish a version. You can\'t
-- modify the code of a published version, only the unpublished version.
--
-- For a function defined as a container image, Lambda resolves the image
-- tag to an image digest. In Amazon ECR, if you update the image tag to a
-- new image, Lambda does not automatically update the function.
module Amazonka.Lambda.UpdateFunctionCode
  ( -- * Creating a Request
    UpdateFunctionCode (..),
    newUpdateFunctionCode,

    -- * Request Lenses
    updateFunctionCode_s3ObjectVersion,
    updateFunctionCode_s3Key,
    updateFunctionCode_zipFile,
    updateFunctionCode_architectures,
    updateFunctionCode_imageUri,
    updateFunctionCode_s3Bucket,
    updateFunctionCode_dryRun,
    updateFunctionCode_revisionId,
    updateFunctionCode_publish,
    updateFunctionCode_functionName,

    -- * Destructuring the Response
    FunctionConfiguration (..),
    newFunctionConfiguration,

    -- * Response Lenses
    functionConfiguration_memorySize,
    functionConfiguration_runtime,
    functionConfiguration_state,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_functionArn,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_packageType,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_environment,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_architectures,
    functionConfiguration_signingJobArn,
    functionConfiguration_role,
    functionConfiguration_vpcConfig,
    functionConfiguration_version,
    functionConfiguration_functionName,
    functionConfiguration_layers,
    functionConfiguration_codeSize,
    functionConfiguration_handler,
    functionConfiguration_timeout,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_stateReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSha256,
    functionConfiguration_tracingConfig,
    functionConfiguration_stateReasonCode,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_description,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_masterArn,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Lambda.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFunctionCode' smart constructor.
data UpdateFunctionCode = UpdateFunctionCode'
  { -- | For versioned objects, the version of the deployment package object to
    -- use.
    s3ObjectVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 key of the deployment package.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded contents of the deployment package. Amazon Web
    -- Services SDK and Amazon Web Services CLI clients handle the encoding for
    -- you.
    zipFile :: Prelude.Maybe (Core.Sensitive Core.Base64),
    -- | The instruction set architecture that the function supports. Enter a
    -- string array with one of the valid values. The default value is
    -- @x86_64@.
    architectures :: Prelude.Maybe (Prelude.NonEmpty Architecture),
    -- | URI of a container image in the Amazon ECR registry.
    imageUri :: Prelude.Maybe Prelude.Text,
    -- | An Amazon S3 bucket in the same Amazon Web Services Region as your
    -- function. The bucket can be in a different Amazon Web Services account.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | Set to true to validate the request parameters and access permissions
    -- without modifying the function code.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Only update the function if the revision ID matches the ID that\'s
    -- specified. Use this option to avoid modifying a function that has
    -- changed since you last read it.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | Set to true to publish a new version of the function after updating the
    -- code. This has the same effect as calling PublishVersion separately.
    publish :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFunctionCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ObjectVersion', 'updateFunctionCode_s3ObjectVersion' - For versioned objects, the version of the deployment package object to
-- use.
--
-- 's3Key', 'updateFunctionCode_s3Key' - The Amazon S3 key of the deployment package.
--
-- 'zipFile', 'updateFunctionCode_zipFile' - The base64-encoded contents of the deployment package. Amazon Web
-- Services SDK and Amazon Web Services CLI clients handle the encoding for
-- you.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'architectures', 'updateFunctionCode_architectures' - The instruction set architecture that the function supports. Enter a
-- string array with one of the valid values. The default value is
-- @x86_64@.
--
-- 'imageUri', 'updateFunctionCode_imageUri' - URI of a container image in the Amazon ECR registry.
--
-- 's3Bucket', 'updateFunctionCode_s3Bucket' - An Amazon S3 bucket in the same Amazon Web Services Region as your
-- function. The bucket can be in a different Amazon Web Services account.
--
-- 'dryRun', 'updateFunctionCode_dryRun' - Set to true to validate the request parameters and access permissions
-- without modifying the function code.
--
-- 'revisionId', 'updateFunctionCode_revisionId' - Only update the function if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a function that has
-- changed since you last read it.
--
-- 'publish', 'updateFunctionCode_publish' - Set to true to publish a new version of the function after updating the
-- code. This has the same effect as calling PublishVersion separately.
--
-- 'functionName', 'updateFunctionCode_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newUpdateFunctionCode ::
  -- | 'functionName'
  Prelude.Text ->
  UpdateFunctionCode
newUpdateFunctionCode pFunctionName_ =
  UpdateFunctionCode'
    { s3ObjectVersion =
        Prelude.Nothing,
      s3Key = Prelude.Nothing,
      zipFile = Prelude.Nothing,
      architectures = Prelude.Nothing,
      imageUri = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      publish = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | For versioned objects, the version of the deployment package object to
-- use.
updateFunctionCode_s3ObjectVersion :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Text)
updateFunctionCode_s3ObjectVersion = Lens.lens (\UpdateFunctionCode' {s3ObjectVersion} -> s3ObjectVersion) (\s@UpdateFunctionCode' {} a -> s {s3ObjectVersion = a} :: UpdateFunctionCode)

-- | The Amazon S3 key of the deployment package.
updateFunctionCode_s3Key :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Text)
updateFunctionCode_s3Key = Lens.lens (\UpdateFunctionCode' {s3Key} -> s3Key) (\s@UpdateFunctionCode' {} a -> s {s3Key = a} :: UpdateFunctionCode)

-- | The base64-encoded contents of the deployment package. Amazon Web
-- Services SDK and Amazon Web Services CLI clients handle the encoding for
-- you.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
updateFunctionCode_zipFile :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.ByteString)
updateFunctionCode_zipFile = Lens.lens (\UpdateFunctionCode' {zipFile} -> zipFile) (\s@UpdateFunctionCode' {} a -> s {zipFile = a} :: UpdateFunctionCode) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Core._Base64)

-- | The instruction set architecture that the function supports. Enter a
-- string array with one of the valid values. The default value is
-- @x86_64@.
updateFunctionCode_architectures :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe (Prelude.NonEmpty Architecture))
updateFunctionCode_architectures = Lens.lens (\UpdateFunctionCode' {architectures} -> architectures) (\s@UpdateFunctionCode' {} a -> s {architectures = a} :: UpdateFunctionCode) Prelude.. Lens.mapping Lens.coerced

-- | URI of a container image in the Amazon ECR registry.
updateFunctionCode_imageUri :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Text)
updateFunctionCode_imageUri = Lens.lens (\UpdateFunctionCode' {imageUri} -> imageUri) (\s@UpdateFunctionCode' {} a -> s {imageUri = a} :: UpdateFunctionCode)

-- | An Amazon S3 bucket in the same Amazon Web Services Region as your
-- function. The bucket can be in a different Amazon Web Services account.
updateFunctionCode_s3Bucket :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Text)
updateFunctionCode_s3Bucket = Lens.lens (\UpdateFunctionCode' {s3Bucket} -> s3Bucket) (\s@UpdateFunctionCode' {} a -> s {s3Bucket = a} :: UpdateFunctionCode)

-- | Set to true to validate the request parameters and access permissions
-- without modifying the function code.
updateFunctionCode_dryRun :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Bool)
updateFunctionCode_dryRun = Lens.lens (\UpdateFunctionCode' {dryRun} -> dryRun) (\s@UpdateFunctionCode' {} a -> s {dryRun = a} :: UpdateFunctionCode)

-- | Only update the function if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a function that has
-- changed since you last read it.
updateFunctionCode_revisionId :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Text)
updateFunctionCode_revisionId = Lens.lens (\UpdateFunctionCode' {revisionId} -> revisionId) (\s@UpdateFunctionCode' {} a -> s {revisionId = a} :: UpdateFunctionCode)

-- | Set to true to publish a new version of the function after updating the
-- code. This has the same effect as calling PublishVersion separately.
updateFunctionCode_publish :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Bool)
updateFunctionCode_publish = Lens.lens (\UpdateFunctionCode' {publish} -> publish) (\s@UpdateFunctionCode' {} a -> s {publish = a} :: UpdateFunctionCode)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
updateFunctionCode_functionName :: Lens.Lens' UpdateFunctionCode Prelude.Text
updateFunctionCode_functionName = Lens.lens (\UpdateFunctionCode' {functionName} -> functionName) (\s@UpdateFunctionCode' {} a -> s {functionName = a} :: UpdateFunctionCode)

instance Core.AWSRequest UpdateFunctionCode where
  type
    AWSResponse UpdateFunctionCode =
      FunctionConfiguration
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateFunctionCode

instance Prelude.NFData UpdateFunctionCode

instance Core.ToHeaders UpdateFunctionCode where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateFunctionCode where
  toJSON UpdateFunctionCode' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3ObjectVersion" Core..=)
              Prelude.<$> s3ObjectVersion,
            ("S3Key" Core..=) Prelude.<$> s3Key,
            ("ZipFile" Core..=) Prelude.<$> zipFile,
            ("Architectures" Core..=) Prelude.<$> architectures,
            ("ImageUri" Core..=) Prelude.<$> imageUri,
            ("S3Bucket" Core..=) Prelude.<$> s3Bucket,
            ("DryRun" Core..=) Prelude.<$> dryRun,
            ("RevisionId" Core..=) Prelude.<$> revisionId,
            ("Publish" Core..=) Prelude.<$> publish
          ]
      )

instance Core.ToPath UpdateFunctionCode where
  toPath UpdateFunctionCode' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Core.toBS functionName,
        "/code"
      ]

instance Core.ToQuery UpdateFunctionCode where
  toQuery = Prelude.const Prelude.mempty
