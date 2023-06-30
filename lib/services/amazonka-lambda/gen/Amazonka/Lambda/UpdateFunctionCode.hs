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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Lambda function\'s code. If code signing is enabled for the
-- function, the code package must be signed by a trusted publisher. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-codesigning.html Configuring code signing for Lambda>.
--
-- If the function\'s package type is @Image@, then you must specify the
-- code package in @ImageUri@ as the URI of a
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-images.html container image>
-- in the Amazon ECR registry.
--
-- If the function\'s package type is @Zip@, then you must specify the
-- deployment package as a
-- <https://docs.aws.amazon.com/lambda/latest/dg/gettingstarted-package.html#gettingstarted-package-zip .zip file archive>.
-- Enter the Amazon S3 bucket and key of the code .zip file location. You
-- can also provide the function code inline using the @ZipFile@ field.
--
-- The code in the deployment package must be compatible with the target
-- instruction set architecture of the function (@x86-64@ or @arm64@).
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
    updateFunctionCode_architectures,
    updateFunctionCode_dryRun,
    updateFunctionCode_imageUri,
    updateFunctionCode_publish,
    updateFunctionCode_revisionId,
    updateFunctionCode_s3Bucket,
    updateFunctionCode_s3Key,
    updateFunctionCode_s3ObjectVersion,
    updateFunctionCode_zipFile,
    updateFunctionCode_functionName,

    -- * Destructuring the Response
    FunctionConfiguration (..),
    newFunctionConfiguration,

    -- * Response Lenses
    functionConfiguration_architectures,
    functionConfiguration_codeSha256,
    functionConfiguration_codeSize,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_description,
    functionConfiguration_environment,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_functionArn,
    functionConfiguration_functionName,
    functionConfiguration_handler,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_lastModified,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_layers,
    functionConfiguration_masterArn,
    functionConfiguration_memorySize,
    functionConfiguration_packageType,
    functionConfiguration_revisionId,
    functionConfiguration_role,
    functionConfiguration_runtime,
    functionConfiguration_signingJobArn,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_snapStart,
    functionConfiguration_state,
    functionConfiguration_stateReason,
    functionConfiguration_stateReasonCode,
    functionConfiguration_timeout,
    functionConfiguration_tracingConfig,
    functionConfiguration_version,
    functionConfiguration_vpcConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFunctionCode' smart constructor.
data UpdateFunctionCode = UpdateFunctionCode'
  { -- | The instruction set architecture that the function supports. Enter a
    -- string array with one of the valid values (arm64 or x86_64). The default
    -- value is @x86_64@.
    architectures :: Prelude.Maybe (Prelude.NonEmpty Architecture),
    -- | Set to true to validate the request parameters and access permissions
    -- without modifying the function code.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | URI of a container image in the Amazon ECR registry. Do not use for a
    -- function defined with a .zip file archive.
    imageUri :: Prelude.Maybe Prelude.Text,
    -- | Set to true to publish a new version of the function after updating the
    -- code. This has the same effect as calling PublishVersion separately.
    publish :: Prelude.Maybe Prelude.Bool,
    -- | Update the function only if the revision ID matches the ID that\'s
    -- specified. Use this option to avoid modifying a function that has
    -- changed since you last read it.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | An Amazon S3 bucket in the same Amazon Web Services Region as your
    -- function. The bucket can be in a different Amazon Web Services account.
    -- Use only with a function defined with a .zip file archive deployment
    -- package.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 key of the deployment package. Use only with a function
    -- defined with a .zip file archive deployment package.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | For versioned objects, the version of the deployment package object to
    -- use.
    s3ObjectVersion :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded contents of the deployment package. Amazon Web
    -- Services SDK and CLI clients handle the encoding for you. Use only with
    -- a function defined with a .zip file archive deployment package.
    zipFile :: Prelude.Maybe (Data.Sensitive Data.Base64),
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ – @my-function@.
    --
    -- -   __Function ARN__ –
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ – @123456789012:function:my-function@.
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
-- 'architectures', 'updateFunctionCode_architectures' - The instruction set architecture that the function supports. Enter a
-- string array with one of the valid values (arm64 or x86_64). The default
-- value is @x86_64@.
--
-- 'dryRun', 'updateFunctionCode_dryRun' - Set to true to validate the request parameters and access permissions
-- without modifying the function code.
--
-- 'imageUri', 'updateFunctionCode_imageUri' - URI of a container image in the Amazon ECR registry. Do not use for a
-- function defined with a .zip file archive.
--
-- 'publish', 'updateFunctionCode_publish' - Set to true to publish a new version of the function after updating the
-- code. This has the same effect as calling PublishVersion separately.
--
-- 'revisionId', 'updateFunctionCode_revisionId' - Update the function only if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a function that has
-- changed since you last read it.
--
-- 's3Bucket', 'updateFunctionCode_s3Bucket' - An Amazon S3 bucket in the same Amazon Web Services Region as your
-- function. The bucket can be in a different Amazon Web Services account.
-- Use only with a function defined with a .zip file archive deployment
-- package.
--
-- 's3Key', 'updateFunctionCode_s3Key' - The Amazon S3 key of the deployment package. Use only with a function
-- defined with a .zip file archive deployment package.
--
-- 's3ObjectVersion', 'updateFunctionCode_s3ObjectVersion' - For versioned objects, the version of the deployment package object to
-- use.
--
-- 'zipFile', 'updateFunctionCode_zipFile' - The base64-encoded contents of the deployment package. Amazon Web
-- Services SDK and CLI clients handle the encoding for you. Use only with
-- a function defined with a .zip file archive deployment package.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'functionName', 'updateFunctionCode_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newUpdateFunctionCode ::
  -- | 'functionName'
  Prelude.Text ->
  UpdateFunctionCode
newUpdateFunctionCode pFunctionName_ =
  UpdateFunctionCode'
    { architectures =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      imageUri = Prelude.Nothing,
      publish = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      s3Key = Prelude.Nothing,
      s3ObjectVersion = Prelude.Nothing,
      zipFile = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | The instruction set architecture that the function supports. Enter a
-- string array with one of the valid values (arm64 or x86_64). The default
-- value is @x86_64@.
updateFunctionCode_architectures :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe (Prelude.NonEmpty Architecture))
updateFunctionCode_architectures = Lens.lens (\UpdateFunctionCode' {architectures} -> architectures) (\s@UpdateFunctionCode' {} a -> s {architectures = a} :: UpdateFunctionCode) Prelude.. Lens.mapping Lens.coerced

-- | Set to true to validate the request parameters and access permissions
-- without modifying the function code.
updateFunctionCode_dryRun :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Bool)
updateFunctionCode_dryRun = Lens.lens (\UpdateFunctionCode' {dryRun} -> dryRun) (\s@UpdateFunctionCode' {} a -> s {dryRun = a} :: UpdateFunctionCode)

-- | URI of a container image in the Amazon ECR registry. Do not use for a
-- function defined with a .zip file archive.
updateFunctionCode_imageUri :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Text)
updateFunctionCode_imageUri = Lens.lens (\UpdateFunctionCode' {imageUri} -> imageUri) (\s@UpdateFunctionCode' {} a -> s {imageUri = a} :: UpdateFunctionCode)

-- | Set to true to publish a new version of the function after updating the
-- code. This has the same effect as calling PublishVersion separately.
updateFunctionCode_publish :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Bool)
updateFunctionCode_publish = Lens.lens (\UpdateFunctionCode' {publish} -> publish) (\s@UpdateFunctionCode' {} a -> s {publish = a} :: UpdateFunctionCode)

-- | Update the function only if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a function that has
-- changed since you last read it.
updateFunctionCode_revisionId :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Text)
updateFunctionCode_revisionId = Lens.lens (\UpdateFunctionCode' {revisionId} -> revisionId) (\s@UpdateFunctionCode' {} a -> s {revisionId = a} :: UpdateFunctionCode)

-- | An Amazon S3 bucket in the same Amazon Web Services Region as your
-- function. The bucket can be in a different Amazon Web Services account.
-- Use only with a function defined with a .zip file archive deployment
-- package.
updateFunctionCode_s3Bucket :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Text)
updateFunctionCode_s3Bucket = Lens.lens (\UpdateFunctionCode' {s3Bucket} -> s3Bucket) (\s@UpdateFunctionCode' {} a -> s {s3Bucket = a} :: UpdateFunctionCode)

-- | The Amazon S3 key of the deployment package. Use only with a function
-- defined with a .zip file archive deployment package.
updateFunctionCode_s3Key :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Text)
updateFunctionCode_s3Key = Lens.lens (\UpdateFunctionCode' {s3Key} -> s3Key) (\s@UpdateFunctionCode' {} a -> s {s3Key = a} :: UpdateFunctionCode)

-- | For versioned objects, the version of the deployment package object to
-- use.
updateFunctionCode_s3ObjectVersion :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.Text)
updateFunctionCode_s3ObjectVersion = Lens.lens (\UpdateFunctionCode' {s3ObjectVersion} -> s3ObjectVersion) (\s@UpdateFunctionCode' {} a -> s {s3ObjectVersion = a} :: UpdateFunctionCode)

-- | The base64-encoded contents of the deployment package. Amazon Web
-- Services SDK and CLI clients handle the encoding for you. Use only with
-- a function defined with a .zip file archive deployment package.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
updateFunctionCode_zipFile :: Lens.Lens' UpdateFunctionCode (Prelude.Maybe Prelude.ByteString)
updateFunctionCode_zipFile = Lens.lens (\UpdateFunctionCode' {zipFile} -> zipFile) (\s@UpdateFunctionCode' {} a -> s {zipFile = a} :: UpdateFunctionCode) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
updateFunctionCode_functionName :: Lens.Lens' UpdateFunctionCode Prelude.Text
updateFunctionCode_functionName = Lens.lens (\UpdateFunctionCode' {functionName} -> functionName) (\s@UpdateFunctionCode' {} a -> s {functionName = a} :: UpdateFunctionCode)

instance Core.AWSRequest UpdateFunctionCode where
  type
    AWSResponse UpdateFunctionCode =
      FunctionConfiguration
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateFunctionCode where
  hashWithSalt _salt UpdateFunctionCode' {..} =
    _salt
      `Prelude.hashWithSalt` architectures
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` imageUri
      `Prelude.hashWithSalt` publish
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key
      `Prelude.hashWithSalt` s3ObjectVersion
      `Prelude.hashWithSalt` zipFile
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData UpdateFunctionCode where
  rnf UpdateFunctionCode' {..} =
    Prelude.rnf architectures
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf imageUri
      `Prelude.seq` Prelude.rnf publish
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Key
      `Prelude.seq` Prelude.rnf s3ObjectVersion
      `Prelude.seq` Prelude.rnf zipFile
      `Prelude.seq` Prelude.rnf functionName

instance Data.ToHeaders UpdateFunctionCode where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateFunctionCode where
  toJSON UpdateFunctionCode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Architectures" Data..=) Prelude.<$> architectures,
            ("DryRun" Data..=) Prelude.<$> dryRun,
            ("ImageUri" Data..=) Prelude.<$> imageUri,
            ("Publish" Data..=) Prelude.<$> publish,
            ("RevisionId" Data..=) Prelude.<$> revisionId,
            ("S3Bucket" Data..=) Prelude.<$> s3Bucket,
            ("S3Key" Data..=) Prelude.<$> s3Key,
            ("S3ObjectVersion" Data..=)
              Prelude.<$> s3ObjectVersion,
            ("ZipFile" Data..=) Prelude.<$> zipFile
          ]
      )

instance Data.ToPath UpdateFunctionCode where
  toPath UpdateFunctionCode' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Data.toBS functionName,
        "/code"
      ]

instance Data.ToQuery UpdateFunctionCode where
  toQuery = Prelude.const Prelude.mempty
