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
-- Module      : Amazonka.Comprehend.CreateEntityRecognizer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an entity recognizer using submitted files. After your
-- @CreateEntityRecognizer@ request is submitted, you can check job status
-- using the API.
module Amazonka.Comprehend.CreateEntityRecognizer
  ( -- * Creating a Request
    CreateEntityRecognizer (..),
    newCreateEntityRecognizer,

    -- * Request Lenses
    createEntityRecognizer_tags,
    createEntityRecognizer_modelPolicy,
    createEntityRecognizer_modelKmsKeyId,
    createEntityRecognizer_clientRequestToken,
    createEntityRecognizer_vpcConfig,
    createEntityRecognizer_volumeKmsKeyId,
    createEntityRecognizer_versionName,
    createEntityRecognizer_recognizerName,
    createEntityRecognizer_dataAccessRoleArn,
    createEntityRecognizer_inputDataConfig,
    createEntityRecognizer_languageCode,

    -- * Destructuring the Response
    CreateEntityRecognizerResponse (..),
    newCreateEntityRecognizerResponse,

    -- * Response Lenses
    createEntityRecognizerResponse_entityRecognizerArn,
    createEntityRecognizerResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEntityRecognizer' smart constructor.
data CreateEntityRecognizer = CreateEntityRecognizer'
  { -- | Tags to be associated with the entity recognizer being created. A tag is
    -- a key-value pair that adds as a metadata to a resource used by Amazon
    -- Comprehend. For example, a tag with \"Sales\" as the key might be added
    -- to a resource to indicate its use by the sales department.
    tags :: Prelude.Maybe [Tag],
    -- | The JSON resource-based policy to attach to your custom entity
    -- recognizer model. You can use this policy to allow another AWS account
    -- to import your custom model.
    --
    -- Provide your JSON as a UTF-8 encoded string without line breaks. To
    -- provide valid JSON for your policy, enclose the attribute names and
    -- values in double quotes. If the JSON body is also enclosed in double
    -- quotes, then you must escape the double quotes that are inside the
    -- policy:
    --
    -- @\"{\\\"attribute\\\": \\\"value\\\", \\\"attribute\\\": [\\\"value\\\"]}\"@
    --
    -- To avoid escaping quotes, you can use single quotes to enclose the
    -- policy and double quotes to enclose the JSON names and values:
    --
    -- @\'{\"attribute\": \"value\", \"attribute\": [\"value\"]}\'@
    modelPolicy :: Prelude.Maybe Prelude.Text,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt trained custom models. The ModelKmsKeyId can be either
    -- of the following formats
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    modelKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Configuration parameters for an optional private Virtual Private Cloud
    -- (VPC) containing the resources you are using for your custom entity
    -- recognizer. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt data on the storage volume attached to the ML compute
    -- instance(s) that process the analysis job. The VolumeKmsKeyId can be
    -- either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The version name given to the newly created recognizer. Version names
    -- can be a maximum of 256 characters. Alphanumeric characters, hyphens (-)
    -- and underscores (_) are allowed. The version name must be unique among
    -- all models with the same recognizer name in the account\/ AWS Region.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The name given to the newly created recognizer. Recognizer names can be
    -- a maximum of 256 characters. Alphanumeric characters, hyphens (-) and
    -- underscores (_) are allowed. The name must be unique in the
    -- account\/region.
    recognizerName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
    -- role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Prelude.Text,
    -- | Specifies the format and location of the input data. The S3 bucket
    -- containing the input data must be located in the same region as the
    -- entity recognizer being created.
    inputDataConfig :: EntityRecognizerInputDataConfig,
    -- | You can specify any of the following languages supported by Amazon
    -- Comprehend: English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian
    -- (\"it\"), German (\"de\"), or Portuguese (\"pt\"). All documents must be
    -- in the same language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEntityRecognizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createEntityRecognizer_tags' - Tags to be associated with the entity recognizer being created. A tag is
-- a key-value pair that adds as a metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
--
-- 'modelPolicy', 'createEntityRecognizer_modelPolicy' - The JSON resource-based policy to attach to your custom entity
-- recognizer model. You can use this policy to allow another AWS account
-- to import your custom model.
--
-- Provide your JSON as a UTF-8 encoded string without line breaks. To
-- provide valid JSON for your policy, enclose the attribute names and
-- values in double quotes. If the JSON body is also enclosed in double
-- quotes, then you must escape the double quotes that are inside the
-- policy:
--
-- @\"{\\\"attribute\\\": \\\"value\\\", \\\"attribute\\\": [\\\"value\\\"]}\"@
--
-- To avoid escaping quotes, you can use single quotes to enclose the
-- policy and double quotes to enclose the JSON names and values:
--
-- @\'{\"attribute\": \"value\", \"attribute\": [\"value\"]}\'@
--
-- 'modelKmsKeyId', 'createEntityRecognizer_modelKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt trained custom models. The ModelKmsKeyId can be either
-- of the following formats
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'clientRequestToken', 'createEntityRecognizer_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'vpcConfig', 'createEntityRecognizer_vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your custom entity
-- recognizer. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'volumeKmsKeyId', 'createEntityRecognizer_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'versionName', 'createEntityRecognizer_versionName' - The version name given to the newly created recognizer. Version names
-- can be a maximum of 256 characters. Alphanumeric characters, hyphens (-)
-- and underscores (_) are allowed. The version name must be unique among
-- all models with the same recognizer name in the account\/ AWS Region.
--
-- 'recognizerName', 'createEntityRecognizer_recognizerName' - The name given to the newly created recognizer. Recognizer names can be
-- a maximum of 256 characters. Alphanumeric characters, hyphens (-) and
-- underscores (_) are allowed. The name must be unique in the
-- account\/region.
--
-- 'dataAccessRoleArn', 'createEntityRecognizer_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your input data.
--
-- 'inputDataConfig', 'createEntityRecognizer_inputDataConfig' - Specifies the format and location of the input data. The S3 bucket
-- containing the input data must be located in the same region as the
-- entity recognizer being created.
--
-- 'languageCode', 'createEntityRecognizer_languageCode' - You can specify any of the following languages supported by Amazon
-- Comprehend: English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian
-- (\"it\"), German (\"de\"), or Portuguese (\"pt\"). All documents must be
-- in the same language.
newCreateEntityRecognizer ::
  -- | 'recognizerName'
  Prelude.Text ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'inputDataConfig'
  EntityRecognizerInputDataConfig ->
  -- | 'languageCode'
  LanguageCode ->
  CreateEntityRecognizer
newCreateEntityRecognizer
  pRecognizerName_
  pDataAccessRoleArn_
  pInputDataConfig_
  pLanguageCode_ =
    CreateEntityRecognizer'
      { tags = Prelude.Nothing,
        modelPolicy = Prelude.Nothing,
        modelKmsKeyId = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        volumeKmsKeyId = Prelude.Nothing,
        versionName = Prelude.Nothing,
        recognizerName = pRecognizerName_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        inputDataConfig = pInputDataConfig_,
        languageCode = pLanguageCode_
      }

-- | Tags to be associated with the entity recognizer being created. A tag is
-- a key-value pair that adds as a metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
createEntityRecognizer_tags :: Lens.Lens' CreateEntityRecognizer (Prelude.Maybe [Tag])
createEntityRecognizer_tags = Lens.lens (\CreateEntityRecognizer' {tags} -> tags) (\s@CreateEntityRecognizer' {} a -> s {tags = a} :: CreateEntityRecognizer) Prelude.. Lens.mapping Lens.coerced

-- | The JSON resource-based policy to attach to your custom entity
-- recognizer model. You can use this policy to allow another AWS account
-- to import your custom model.
--
-- Provide your JSON as a UTF-8 encoded string without line breaks. To
-- provide valid JSON for your policy, enclose the attribute names and
-- values in double quotes. If the JSON body is also enclosed in double
-- quotes, then you must escape the double quotes that are inside the
-- policy:
--
-- @\"{\\\"attribute\\\": \\\"value\\\", \\\"attribute\\\": [\\\"value\\\"]}\"@
--
-- To avoid escaping quotes, you can use single quotes to enclose the
-- policy and double quotes to enclose the JSON names and values:
--
-- @\'{\"attribute\": \"value\", \"attribute\": [\"value\"]}\'@
createEntityRecognizer_modelPolicy :: Lens.Lens' CreateEntityRecognizer (Prelude.Maybe Prelude.Text)
createEntityRecognizer_modelPolicy = Lens.lens (\CreateEntityRecognizer' {modelPolicy} -> modelPolicy) (\s@CreateEntityRecognizer' {} a -> s {modelPolicy = a} :: CreateEntityRecognizer)

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt trained custom models. The ModelKmsKeyId can be either
-- of the following formats
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
createEntityRecognizer_modelKmsKeyId :: Lens.Lens' CreateEntityRecognizer (Prelude.Maybe Prelude.Text)
createEntityRecognizer_modelKmsKeyId = Lens.lens (\CreateEntityRecognizer' {modelKmsKeyId} -> modelKmsKeyId) (\s@CreateEntityRecognizer' {} a -> s {modelKmsKeyId = a} :: CreateEntityRecognizer)

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
createEntityRecognizer_clientRequestToken :: Lens.Lens' CreateEntityRecognizer (Prelude.Maybe Prelude.Text)
createEntityRecognizer_clientRequestToken = Lens.lens (\CreateEntityRecognizer' {clientRequestToken} -> clientRequestToken) (\s@CreateEntityRecognizer' {} a -> s {clientRequestToken = a} :: CreateEntityRecognizer)

-- | Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your custom entity
-- recognizer. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
createEntityRecognizer_vpcConfig :: Lens.Lens' CreateEntityRecognizer (Prelude.Maybe VpcConfig)
createEntityRecognizer_vpcConfig = Lens.lens (\CreateEntityRecognizer' {vpcConfig} -> vpcConfig) (\s@CreateEntityRecognizer' {} a -> s {vpcConfig = a} :: CreateEntityRecognizer)

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
createEntityRecognizer_volumeKmsKeyId :: Lens.Lens' CreateEntityRecognizer (Prelude.Maybe Prelude.Text)
createEntityRecognizer_volumeKmsKeyId = Lens.lens (\CreateEntityRecognizer' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@CreateEntityRecognizer' {} a -> s {volumeKmsKeyId = a} :: CreateEntityRecognizer)

-- | The version name given to the newly created recognizer. Version names
-- can be a maximum of 256 characters. Alphanumeric characters, hyphens (-)
-- and underscores (_) are allowed. The version name must be unique among
-- all models with the same recognizer name in the account\/ AWS Region.
createEntityRecognizer_versionName :: Lens.Lens' CreateEntityRecognizer (Prelude.Maybe Prelude.Text)
createEntityRecognizer_versionName = Lens.lens (\CreateEntityRecognizer' {versionName} -> versionName) (\s@CreateEntityRecognizer' {} a -> s {versionName = a} :: CreateEntityRecognizer)

-- | The name given to the newly created recognizer. Recognizer names can be
-- a maximum of 256 characters. Alphanumeric characters, hyphens (-) and
-- underscores (_) are allowed. The name must be unique in the
-- account\/region.
createEntityRecognizer_recognizerName :: Lens.Lens' CreateEntityRecognizer Prelude.Text
createEntityRecognizer_recognizerName = Lens.lens (\CreateEntityRecognizer' {recognizerName} -> recognizerName) (\s@CreateEntityRecognizer' {} a -> s {recognizerName = a} :: CreateEntityRecognizer)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your input data.
createEntityRecognizer_dataAccessRoleArn :: Lens.Lens' CreateEntityRecognizer Prelude.Text
createEntityRecognizer_dataAccessRoleArn = Lens.lens (\CreateEntityRecognizer' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@CreateEntityRecognizer' {} a -> s {dataAccessRoleArn = a} :: CreateEntityRecognizer)

-- | Specifies the format and location of the input data. The S3 bucket
-- containing the input data must be located in the same region as the
-- entity recognizer being created.
createEntityRecognizer_inputDataConfig :: Lens.Lens' CreateEntityRecognizer EntityRecognizerInputDataConfig
createEntityRecognizer_inputDataConfig = Lens.lens (\CreateEntityRecognizer' {inputDataConfig} -> inputDataConfig) (\s@CreateEntityRecognizer' {} a -> s {inputDataConfig = a} :: CreateEntityRecognizer)

-- | You can specify any of the following languages supported by Amazon
-- Comprehend: English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian
-- (\"it\"), German (\"de\"), or Portuguese (\"pt\"). All documents must be
-- in the same language.
createEntityRecognizer_languageCode :: Lens.Lens' CreateEntityRecognizer LanguageCode
createEntityRecognizer_languageCode = Lens.lens (\CreateEntityRecognizer' {languageCode} -> languageCode) (\s@CreateEntityRecognizer' {} a -> s {languageCode = a} :: CreateEntityRecognizer)

instance Core.AWSRequest CreateEntityRecognizer where
  type
    AWSResponse CreateEntityRecognizer =
      CreateEntityRecognizerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEntityRecognizerResponse'
            Prelude.<$> (x Data..?> "EntityRecognizerArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEntityRecognizer where
  hashWithSalt _salt CreateEntityRecognizer' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` modelPolicy
      `Prelude.hashWithSalt` modelKmsKeyId
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` recognizerName
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData CreateEntityRecognizer where
  rnf CreateEntityRecognizer' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf modelPolicy
      `Prelude.seq` Prelude.rnf modelKmsKeyId
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf recognizerName
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders CreateEntityRecognizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.CreateEntityRecognizer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEntityRecognizer where
  toJSON CreateEntityRecognizer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ModelPolicy" Data..=) Prelude.<$> modelPolicy,
            ("ModelKmsKeyId" Data..=) Prelude.<$> modelKmsKeyId,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            ("VolumeKmsKeyId" Data..=)
              Prelude.<$> volumeKmsKeyId,
            ("VersionName" Data..=) Prelude.<$> versionName,
            Prelude.Just
              ("RecognizerName" Data..= recognizerName),
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn),
            Prelude.Just
              ("InputDataConfig" Data..= inputDataConfig),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath CreateEntityRecognizer where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEntityRecognizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEntityRecognizerResponse' smart constructor.
data CreateEntityRecognizerResponse = CreateEntityRecognizerResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEntityRecognizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityRecognizerArn', 'createEntityRecognizerResponse_entityRecognizerArn' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- 'httpStatus', 'createEntityRecognizerResponse_httpStatus' - The response's http status code.
newCreateEntityRecognizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEntityRecognizerResponse
newCreateEntityRecognizerResponse pHttpStatus_ =
  CreateEntityRecognizerResponse'
    { entityRecognizerArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
createEntityRecognizerResponse_entityRecognizerArn :: Lens.Lens' CreateEntityRecognizerResponse (Prelude.Maybe Prelude.Text)
createEntityRecognizerResponse_entityRecognizerArn = Lens.lens (\CreateEntityRecognizerResponse' {entityRecognizerArn} -> entityRecognizerArn) (\s@CreateEntityRecognizerResponse' {} a -> s {entityRecognizerArn = a} :: CreateEntityRecognizerResponse)

-- | The response's http status code.
createEntityRecognizerResponse_httpStatus :: Lens.Lens' CreateEntityRecognizerResponse Prelude.Int
createEntityRecognizerResponse_httpStatus = Lens.lens (\CreateEntityRecognizerResponse' {httpStatus} -> httpStatus) (\s@CreateEntityRecognizerResponse' {} a -> s {httpStatus = a} :: CreateEntityRecognizerResponse)

instance
  Prelude.NFData
    CreateEntityRecognizerResponse
  where
  rnf CreateEntityRecognizerResponse' {..} =
    Prelude.rnf entityRecognizerArn
      `Prelude.seq` Prelude.rnf httpStatus
