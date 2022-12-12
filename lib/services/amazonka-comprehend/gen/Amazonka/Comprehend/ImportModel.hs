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
-- Module      : Amazonka.Comprehend.ImportModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom model that replicates a source custom model that
-- you import. The source model can be in your AWS account or another one.
--
-- If the source model is in another AWS account, then it must have a
-- resource-based policy that authorizes you to import it.
--
-- The source model must be in the same AWS region that you\'re using when
-- you import. You can\'t import a model that\'s in a different region.
module Amazonka.Comprehend.ImportModel
  ( -- * Creating a Request
    ImportModel (..),
    newImportModel,

    -- * Request Lenses
    importModel_dataAccessRoleArn,
    importModel_modelKmsKeyId,
    importModel_modelName,
    importModel_tags,
    importModel_versionName,
    importModel_sourceModelArn,

    -- * Destructuring the Response
    ImportModelResponse (..),
    newImportModelResponse,

    -- * Response Lenses
    importModelResponse_modelArn,
    importModelResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportModel' smart constructor.
data ImportModel = ImportModel'
  { -- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
    -- role that allows Amazon Comprehend to use Amazon Key Management Service
    -- (KMS) to encrypt or decrypt the custom model.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt trained custom models. The ModelKmsKeyId can be either
    -- of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    modelKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name to assign to the custom model that is created in Amazon
    -- Comprehend by this import.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | Tags to be associated with the custom model that is created by this
    -- import. A tag is a key-value pair that adds as a metadata to a resource
    -- used by Amazon Comprehend. For example, a tag with \"Sales\" as the key
    -- might be added to a resource to indicate its use by the sales
    -- department.
    tags :: Prelude.Maybe [Tag],
    -- | The version name given to the custom model that is created by this
    -- import. Version names can have a maximum of 256 characters. Alphanumeric
    -- characters, hyphens (-) and underscores (_) are allowed. The version
    -- name must be unique among all models with the same classifier name in
    -- the account\/AWS Region.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the custom model to import.
    sourceModelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAccessRoleArn', 'importModel_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that allows Amazon Comprehend to use Amazon Key Management Service
-- (KMS) to encrypt or decrypt the custom model.
--
-- 'modelKmsKeyId', 'importModel_modelKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt trained custom models. The ModelKmsKeyId can be either
-- of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'modelName', 'importModel_modelName' - The name to assign to the custom model that is created in Amazon
-- Comprehend by this import.
--
-- 'tags', 'importModel_tags' - Tags to be associated with the custom model that is created by this
-- import. A tag is a key-value pair that adds as a metadata to a resource
-- used by Amazon Comprehend. For example, a tag with \"Sales\" as the key
-- might be added to a resource to indicate its use by the sales
-- department.
--
-- 'versionName', 'importModel_versionName' - The version name given to the custom model that is created by this
-- import. Version names can have a maximum of 256 characters. Alphanumeric
-- characters, hyphens (-) and underscores (_) are allowed. The version
-- name must be unique among all models with the same classifier name in
-- the account\/AWS Region.
--
-- 'sourceModelArn', 'importModel_sourceModelArn' - The Amazon Resource Name (ARN) of the custom model to import.
newImportModel ::
  -- | 'sourceModelArn'
  Prelude.Text ->
  ImportModel
newImportModel pSourceModelArn_ =
  ImportModel'
    { dataAccessRoleArn = Prelude.Nothing,
      modelKmsKeyId = Prelude.Nothing,
      modelName = Prelude.Nothing,
      tags = Prelude.Nothing,
      versionName = Prelude.Nothing,
      sourceModelArn = pSourceModelArn_
    }

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that allows Amazon Comprehend to use Amazon Key Management Service
-- (KMS) to encrypt or decrypt the custom model.
importModel_dataAccessRoleArn :: Lens.Lens' ImportModel (Prelude.Maybe Prelude.Text)
importModel_dataAccessRoleArn = Lens.lens (\ImportModel' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@ImportModel' {} a -> s {dataAccessRoleArn = a} :: ImportModel)

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt trained custom models. The ModelKmsKeyId can be either
-- of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
importModel_modelKmsKeyId :: Lens.Lens' ImportModel (Prelude.Maybe Prelude.Text)
importModel_modelKmsKeyId = Lens.lens (\ImportModel' {modelKmsKeyId} -> modelKmsKeyId) (\s@ImportModel' {} a -> s {modelKmsKeyId = a} :: ImportModel)

-- | The name to assign to the custom model that is created in Amazon
-- Comprehend by this import.
importModel_modelName :: Lens.Lens' ImportModel (Prelude.Maybe Prelude.Text)
importModel_modelName = Lens.lens (\ImportModel' {modelName} -> modelName) (\s@ImportModel' {} a -> s {modelName = a} :: ImportModel)

-- | Tags to be associated with the custom model that is created by this
-- import. A tag is a key-value pair that adds as a metadata to a resource
-- used by Amazon Comprehend. For example, a tag with \"Sales\" as the key
-- might be added to a resource to indicate its use by the sales
-- department.
importModel_tags :: Lens.Lens' ImportModel (Prelude.Maybe [Tag])
importModel_tags = Lens.lens (\ImportModel' {tags} -> tags) (\s@ImportModel' {} a -> s {tags = a} :: ImportModel) Prelude.. Lens.mapping Lens.coerced

-- | The version name given to the custom model that is created by this
-- import. Version names can have a maximum of 256 characters. Alphanumeric
-- characters, hyphens (-) and underscores (_) are allowed. The version
-- name must be unique among all models with the same classifier name in
-- the account\/AWS Region.
importModel_versionName :: Lens.Lens' ImportModel (Prelude.Maybe Prelude.Text)
importModel_versionName = Lens.lens (\ImportModel' {versionName} -> versionName) (\s@ImportModel' {} a -> s {versionName = a} :: ImportModel)

-- | The Amazon Resource Name (ARN) of the custom model to import.
importModel_sourceModelArn :: Lens.Lens' ImportModel Prelude.Text
importModel_sourceModelArn = Lens.lens (\ImportModel' {sourceModelArn} -> sourceModelArn) (\s@ImportModel' {} a -> s {sourceModelArn = a} :: ImportModel)

instance Core.AWSRequest ImportModel where
  type AWSResponse ImportModel = ImportModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportModelResponse'
            Prelude.<$> (x Data..?> "ModelArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportModel where
  hashWithSalt _salt ImportModel' {..} =
    _salt `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` modelKmsKeyId
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` sourceModelArn

instance Prelude.NFData ImportModel where
  rnf ImportModel' {..} =
    Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf modelKmsKeyId
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf sourceModelArn

instance Data.ToHeaders ImportModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ImportModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportModel where
  toJSON ImportModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataAccessRoleArn" Data..=)
              Prelude.<$> dataAccessRoleArn,
            ("ModelKmsKeyId" Data..=) Prelude.<$> modelKmsKeyId,
            ("ModelName" Data..=) Prelude.<$> modelName,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VersionName" Data..=) Prelude.<$> versionName,
            Prelude.Just
              ("SourceModelArn" Data..= sourceModelArn)
          ]
      )

instance Data.ToPath ImportModel where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportModelResponse' smart constructor.
data ImportModelResponse = ImportModelResponse'
  { -- | The Amazon Resource Name (ARN) of the custom model being imported.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelArn', 'importModelResponse_modelArn' - The Amazon Resource Name (ARN) of the custom model being imported.
--
-- 'httpStatus', 'importModelResponse_httpStatus' - The response's http status code.
newImportModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportModelResponse
newImportModelResponse pHttpStatus_ =
  ImportModelResponse'
    { modelArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the custom model being imported.
importModelResponse_modelArn :: Lens.Lens' ImportModelResponse (Prelude.Maybe Prelude.Text)
importModelResponse_modelArn = Lens.lens (\ImportModelResponse' {modelArn} -> modelArn) (\s@ImportModelResponse' {} a -> s {modelArn = a} :: ImportModelResponse)

-- | The response's http status code.
importModelResponse_httpStatus :: Lens.Lens' ImportModelResponse Prelude.Int
importModelResponse_httpStatus = Lens.lens (\ImportModelResponse' {httpStatus} -> httpStatus) (\s@ImportModelResponse' {} a -> s {httpStatus = a} :: ImportModelResponse)

instance Prelude.NFData ImportModelResponse where
  rnf ImportModelResponse' {..} =
    Prelude.rnf modelArn
      `Prelude.seq` Prelude.rnf httpStatus
