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
-- Module      : Amazonka.Rekognition.CopyProjectVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a version of an Amazon Rekognition Custom Labels model from a
-- source project to a destination project. The source and destination
-- projects can be in different AWS accounts but must be in the same AWS
-- Region. You can\'t copy a model to another AWS service.
--
-- To copy a model version to a different AWS account, you need to create a
-- resource-based policy known as a /project policy/. You attach the
-- project policy to the source project by calling PutProjectPolicy. The
-- project policy gives permission to copy the model version from a
-- trusting AWS account to a trusted account.
--
-- For more information creating and attaching a project policy, see
-- Attaching a project policy (SDK) in the /Amazon Rekognition Custom
-- Labels Developer Guide/.
--
-- If you are copying a model version to a project in the same AWS account,
-- you don\'t need to create a project policy.
--
-- To copy a model, the destination project, source project, and source
-- model version must already exist.
--
-- Copying a model version takes a while to complete. To get the current
-- status, call DescribeProjectVersions and check the value of @Status@ in
-- the ProjectVersionDescription object. The copy operation has finished
-- when the value of @Status@ is @COPYING_COMPLETED@.
module Amazonka.Rekognition.CopyProjectVersion
  ( -- * Creating a Request
    CopyProjectVersion (..),
    newCopyProjectVersion,

    -- * Request Lenses
    copyProjectVersion_kmsKeyId,
    copyProjectVersion_tags,
    copyProjectVersion_sourceProjectArn,
    copyProjectVersion_sourceProjectVersionArn,
    copyProjectVersion_destinationProjectArn,
    copyProjectVersion_versionName,
    copyProjectVersion_outputConfig,

    -- * Destructuring the Response
    CopyProjectVersionResponse (..),
    newCopyProjectVersionResponse,

    -- * Response Lenses
    copyProjectVersionResponse_projectVersionArn,
    copyProjectVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCopyProjectVersion' smart constructor.
data CopyProjectVersion = CopyProjectVersion'
  { -- | The identifier for your AWS Key Management Service key (AWS KMS key).
    -- You can supply the Amazon Resource Name (ARN) of your KMS key, the ID of
    -- your KMS key, an alias for your KMS key, or an alias ARN. The key is
    -- used to encrypt training results and manifest files written to the
    -- output Amazon S3 bucket (@OutputConfig@).
    --
    -- If you choose to use your own KMS key, you need the following
    -- permissions on the KMS key.
    --
    -- -   kms:CreateGrant
    --
    -- -   kms:DescribeKey
    --
    -- -   kms:GenerateDataKey
    --
    -- -   kms:Decrypt
    --
    -- If you don\'t specify a value for @KmsKeyId@, images copied into the
    -- service are encrypted using a key that AWS owns and manages.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The key-value tags to assign to the model version.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the source project in the trusting AWS account.
    sourceProjectArn :: Prelude.Text,
    -- | The ARN of the model version in the source project that you want to copy
    -- to a destination project.
    sourceProjectVersionArn :: Prelude.Text,
    -- | The ARN of the project in the trusted AWS account that you want to copy
    -- the model version to.
    destinationProjectArn :: Prelude.Text,
    -- | A name for the version of the model that\'s copied to the destination
    -- project.
    versionName :: Prelude.Text,
    -- | The S3 bucket and folder location where the training output for the
    -- source model version is placed.
    outputConfig :: OutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyProjectVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'copyProjectVersion_kmsKeyId' - The identifier for your AWS Key Management Service key (AWS KMS key).
-- You can supply the Amazon Resource Name (ARN) of your KMS key, the ID of
-- your KMS key, an alias for your KMS key, or an alias ARN. The key is
-- used to encrypt training results and manifest files written to the
-- output Amazon S3 bucket (@OutputConfig@).
--
-- If you choose to use your own KMS key, you need the following
-- permissions on the KMS key.
--
-- -   kms:CreateGrant
--
-- -   kms:DescribeKey
--
-- -   kms:GenerateDataKey
--
-- -   kms:Decrypt
--
-- If you don\'t specify a value for @KmsKeyId@, images copied into the
-- service are encrypted using a key that AWS owns and manages.
--
-- 'tags', 'copyProjectVersion_tags' - The key-value tags to assign to the model version.
--
-- 'sourceProjectArn', 'copyProjectVersion_sourceProjectArn' - The ARN of the source project in the trusting AWS account.
--
-- 'sourceProjectVersionArn', 'copyProjectVersion_sourceProjectVersionArn' - The ARN of the model version in the source project that you want to copy
-- to a destination project.
--
-- 'destinationProjectArn', 'copyProjectVersion_destinationProjectArn' - The ARN of the project in the trusted AWS account that you want to copy
-- the model version to.
--
-- 'versionName', 'copyProjectVersion_versionName' - A name for the version of the model that\'s copied to the destination
-- project.
--
-- 'outputConfig', 'copyProjectVersion_outputConfig' - The S3 bucket and folder location where the training output for the
-- source model version is placed.
newCopyProjectVersion ::
  -- | 'sourceProjectArn'
  Prelude.Text ->
  -- | 'sourceProjectVersionArn'
  Prelude.Text ->
  -- | 'destinationProjectArn'
  Prelude.Text ->
  -- | 'versionName'
  Prelude.Text ->
  -- | 'outputConfig'
  OutputConfig ->
  CopyProjectVersion
newCopyProjectVersion
  pSourceProjectArn_
  pSourceProjectVersionArn_
  pDestinationProjectArn_
  pVersionName_
  pOutputConfig_ =
    CopyProjectVersion'
      { kmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        sourceProjectArn = pSourceProjectArn_,
        sourceProjectVersionArn = pSourceProjectVersionArn_,
        destinationProjectArn = pDestinationProjectArn_,
        versionName = pVersionName_,
        outputConfig = pOutputConfig_
      }

-- | The identifier for your AWS Key Management Service key (AWS KMS key).
-- You can supply the Amazon Resource Name (ARN) of your KMS key, the ID of
-- your KMS key, an alias for your KMS key, or an alias ARN. The key is
-- used to encrypt training results and manifest files written to the
-- output Amazon S3 bucket (@OutputConfig@).
--
-- If you choose to use your own KMS key, you need the following
-- permissions on the KMS key.
--
-- -   kms:CreateGrant
--
-- -   kms:DescribeKey
--
-- -   kms:GenerateDataKey
--
-- -   kms:Decrypt
--
-- If you don\'t specify a value for @KmsKeyId@, images copied into the
-- service are encrypted using a key that AWS owns and manages.
copyProjectVersion_kmsKeyId :: Lens.Lens' CopyProjectVersion (Prelude.Maybe Prelude.Text)
copyProjectVersion_kmsKeyId = Lens.lens (\CopyProjectVersion' {kmsKeyId} -> kmsKeyId) (\s@CopyProjectVersion' {} a -> s {kmsKeyId = a} :: CopyProjectVersion)

-- | The key-value tags to assign to the model version.
copyProjectVersion_tags :: Lens.Lens' CopyProjectVersion (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
copyProjectVersion_tags = Lens.lens (\CopyProjectVersion' {tags} -> tags) (\s@CopyProjectVersion' {} a -> s {tags = a} :: CopyProjectVersion) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the source project in the trusting AWS account.
copyProjectVersion_sourceProjectArn :: Lens.Lens' CopyProjectVersion Prelude.Text
copyProjectVersion_sourceProjectArn = Lens.lens (\CopyProjectVersion' {sourceProjectArn} -> sourceProjectArn) (\s@CopyProjectVersion' {} a -> s {sourceProjectArn = a} :: CopyProjectVersion)

-- | The ARN of the model version in the source project that you want to copy
-- to a destination project.
copyProjectVersion_sourceProjectVersionArn :: Lens.Lens' CopyProjectVersion Prelude.Text
copyProjectVersion_sourceProjectVersionArn = Lens.lens (\CopyProjectVersion' {sourceProjectVersionArn} -> sourceProjectVersionArn) (\s@CopyProjectVersion' {} a -> s {sourceProjectVersionArn = a} :: CopyProjectVersion)

-- | The ARN of the project in the trusted AWS account that you want to copy
-- the model version to.
copyProjectVersion_destinationProjectArn :: Lens.Lens' CopyProjectVersion Prelude.Text
copyProjectVersion_destinationProjectArn = Lens.lens (\CopyProjectVersion' {destinationProjectArn} -> destinationProjectArn) (\s@CopyProjectVersion' {} a -> s {destinationProjectArn = a} :: CopyProjectVersion)

-- | A name for the version of the model that\'s copied to the destination
-- project.
copyProjectVersion_versionName :: Lens.Lens' CopyProjectVersion Prelude.Text
copyProjectVersion_versionName = Lens.lens (\CopyProjectVersion' {versionName} -> versionName) (\s@CopyProjectVersion' {} a -> s {versionName = a} :: CopyProjectVersion)

-- | The S3 bucket and folder location where the training output for the
-- source model version is placed.
copyProjectVersion_outputConfig :: Lens.Lens' CopyProjectVersion OutputConfig
copyProjectVersion_outputConfig = Lens.lens (\CopyProjectVersion' {outputConfig} -> outputConfig) (\s@CopyProjectVersion' {} a -> s {outputConfig = a} :: CopyProjectVersion)

instance Core.AWSRequest CopyProjectVersion where
  type
    AWSResponse CopyProjectVersion =
      CopyProjectVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyProjectVersionResponse'
            Prelude.<$> (x Data..?> "ProjectVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyProjectVersion where
  hashWithSalt _salt CopyProjectVersion' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceProjectArn
      `Prelude.hashWithSalt` sourceProjectVersionArn
      `Prelude.hashWithSalt` destinationProjectArn
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` outputConfig

instance Prelude.NFData CopyProjectVersion where
  rnf CopyProjectVersion' {..} =
    Prelude.rnf kmsKeyId `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf sourceProjectArn `Prelude.seq`
          Prelude.rnf sourceProjectVersionArn `Prelude.seq`
            Prelude.rnf destinationProjectArn `Prelude.seq`
              Prelude.rnf versionName `Prelude.seq`
                Prelude.rnf outputConfig

instance Data.ToHeaders CopyProjectVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.CopyProjectVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CopyProjectVersion where
  toJSON CopyProjectVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("SourceProjectArn" Data..= sourceProjectArn),
            Prelude.Just
              ( "SourceProjectVersionArn"
                  Data..= sourceProjectVersionArn
              ),
            Prelude.Just
              ( "DestinationProjectArn"
                  Data..= destinationProjectArn
              ),
            Prelude.Just ("VersionName" Data..= versionName),
            Prelude.Just ("OutputConfig" Data..= outputConfig)
          ]
      )

instance Data.ToPath CopyProjectVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery CopyProjectVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCopyProjectVersionResponse' smart constructor.
data CopyProjectVersionResponse = CopyProjectVersionResponse'
  { -- | The ARN of the copied model version in the destination project.
    projectVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyProjectVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectVersionArn', 'copyProjectVersionResponse_projectVersionArn' - The ARN of the copied model version in the destination project.
--
-- 'httpStatus', 'copyProjectVersionResponse_httpStatus' - The response's http status code.
newCopyProjectVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyProjectVersionResponse
newCopyProjectVersionResponse pHttpStatus_ =
  CopyProjectVersionResponse'
    { projectVersionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the copied model version in the destination project.
copyProjectVersionResponse_projectVersionArn :: Lens.Lens' CopyProjectVersionResponse (Prelude.Maybe Prelude.Text)
copyProjectVersionResponse_projectVersionArn = Lens.lens (\CopyProjectVersionResponse' {projectVersionArn} -> projectVersionArn) (\s@CopyProjectVersionResponse' {} a -> s {projectVersionArn = a} :: CopyProjectVersionResponse)

-- | The response's http status code.
copyProjectVersionResponse_httpStatus :: Lens.Lens' CopyProjectVersionResponse Prelude.Int
copyProjectVersionResponse_httpStatus = Lens.lens (\CopyProjectVersionResponse' {httpStatus} -> httpStatus) (\s@CopyProjectVersionResponse' {} a -> s {httpStatus = a} :: CopyProjectVersionResponse)

instance Prelude.NFData CopyProjectVersionResponse where
  rnf CopyProjectVersionResponse' {..} =
    Prelude.rnf projectVersionArn `Prelude.seq`
      Prelude.rnf httpStatus
