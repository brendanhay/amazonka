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
-- Module      : Amazonka.Comprehend.CreateDocumentClassifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document classifier that you can use to categorize
-- documents. To create a classifier, you provide a set of training
-- documents that labeled with the categories that you want to use. After
-- the classifier is trained you can use it to categorize a set of labeled
-- documents into the categories. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-document-classification.html Document Classification>
-- in the Comprehend Developer Guide.
module Amazonka.Comprehend.CreateDocumentClassifier
  ( -- * Creating a Request
    CreateDocumentClassifier (..),
    newCreateDocumentClassifier,

    -- * Request Lenses
    createDocumentClassifier_clientRequestToken,
    createDocumentClassifier_mode,
    createDocumentClassifier_modelKmsKeyId,
    createDocumentClassifier_modelPolicy,
    createDocumentClassifier_outputDataConfig,
    createDocumentClassifier_tags,
    createDocumentClassifier_versionName,
    createDocumentClassifier_volumeKmsKeyId,
    createDocumentClassifier_vpcConfig,
    createDocumentClassifier_documentClassifierName,
    createDocumentClassifier_dataAccessRoleArn,
    createDocumentClassifier_inputDataConfig,
    createDocumentClassifier_languageCode,

    -- * Destructuring the Response
    CreateDocumentClassifierResponse (..),
    newCreateDocumentClassifierResponse,

    -- * Response Lenses
    createDocumentClassifierResponse_documentClassifierArn,
    createDocumentClassifierResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDocumentClassifier' smart constructor.
data CreateDocumentClassifier = CreateDocumentClassifier'
  { -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates the mode in which the classifier will be trained. The
    -- classifier can be trained in multi-class mode, which identifies one and
    -- only one class for each document, or multi-label mode, which identifies
    -- one or more labels for each document. In multi-label mode, multiple
    -- labels for an individual document are separated by a delimiter. The
    -- default delimiter between labels is a pipe (|).
    mode :: Prelude.Maybe DocumentClassifierMode,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt trained custom models. The ModelKmsKeyId can be either
    -- of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    modelKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The resource-based policy to attach to your custom document classifier
    -- model. You can use this policy to allow another AWS account to import
    -- your custom model.
    --
    -- Provide your policy as a JSON body that you enter as a UTF-8 encoded
    -- string without line breaks. To provide valid JSON, enclose the attribute
    -- names and values in double quotes. If the JSON body is also enclosed in
    -- double quotes, then you must escape the double quotes that are inside
    -- the policy:
    --
    -- @\"{\\\"attribute\\\": \\\"value\\\", \\\"attribute\\\": [\\\"value\\\"]}\"@
    --
    -- To avoid escaping quotes, you can use single quotes to enclose the
    -- policy and double quotes to enclose the JSON names and values:
    --
    -- @\'{\"attribute\": \"value\", \"attribute\": [\"value\"]}\'@
    modelPolicy :: Prelude.Maybe Prelude.Text,
    -- | Enables the addition of output results configuration parameters for
    -- custom classifier jobs.
    outputDataConfig :: Prelude.Maybe DocumentClassifierOutputDataConfig,
    -- | Tags to be associated with the document classifier being created. A tag
    -- is a key-value pair that adds as a metadata to a resource used by Amazon
    -- Comprehend. For example, a tag with \"Sales\" as the key might be added
    -- to a resource to indicate its use by the sales department.
    tags :: Prelude.Maybe [Tag],
    -- | The version name given to the newly created classifier. Version names
    -- can have a maximum of 256 characters. Alphanumeric characters, hyphens
    -- (-) and underscores (_) are allowed. The version name must be unique
    -- among all models with the same classifier name in the account\/AWS
    -- Region.
    versionName :: Prelude.Maybe Prelude.Text,
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
    -- | Configuration parameters for an optional private Virtual Private Cloud
    -- (VPC) containing the resources you are using for your custom classifier.
    -- For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The name of the document classifier.
    documentClassifierName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
    -- role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Prelude.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: DocumentClassifierInputDataConfig,
    -- | The language of the input documents. You can specify any of the
    -- following languages supported by Amazon Comprehend: German (\"de\"),
    -- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
    -- or Portuguese (\"pt\"). All documents must be in the same language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDocumentClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createDocumentClassifier_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'mode', 'createDocumentClassifier_mode' - Indicates the mode in which the classifier will be trained. The
-- classifier can be trained in multi-class mode, which identifies one and
-- only one class for each document, or multi-label mode, which identifies
-- one or more labels for each document. In multi-label mode, multiple
-- labels for an individual document are separated by a delimiter. The
-- default delimiter between labels is a pipe (|).
--
-- 'modelKmsKeyId', 'createDocumentClassifier_modelKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt trained custom models. The ModelKmsKeyId can be either
-- of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'modelPolicy', 'createDocumentClassifier_modelPolicy' - The resource-based policy to attach to your custom document classifier
-- model. You can use this policy to allow another AWS account to import
-- your custom model.
--
-- Provide your policy as a JSON body that you enter as a UTF-8 encoded
-- string without line breaks. To provide valid JSON, enclose the attribute
-- names and values in double quotes. If the JSON body is also enclosed in
-- double quotes, then you must escape the double quotes that are inside
-- the policy:
--
-- @\"{\\\"attribute\\\": \\\"value\\\", \\\"attribute\\\": [\\\"value\\\"]}\"@
--
-- To avoid escaping quotes, you can use single quotes to enclose the
-- policy and double quotes to enclose the JSON names and values:
--
-- @\'{\"attribute\": \"value\", \"attribute\": [\"value\"]}\'@
--
-- 'outputDataConfig', 'createDocumentClassifier_outputDataConfig' - Enables the addition of output results configuration parameters for
-- custom classifier jobs.
--
-- 'tags', 'createDocumentClassifier_tags' - Tags to be associated with the document classifier being created. A tag
-- is a key-value pair that adds as a metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
--
-- 'versionName', 'createDocumentClassifier_versionName' - The version name given to the newly created classifier. Version names
-- can have a maximum of 256 characters. Alphanumeric characters, hyphens
-- (-) and underscores (_) are allowed. The version name must be unique
-- among all models with the same classifier name in the account\/AWS
-- Region.
--
-- 'volumeKmsKeyId', 'createDocumentClassifier_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'vpcConfig', 'createDocumentClassifier_vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your custom classifier.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'documentClassifierName', 'createDocumentClassifier_documentClassifierName' - The name of the document classifier.
--
-- 'dataAccessRoleArn', 'createDocumentClassifier_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your input data.
--
-- 'inputDataConfig', 'createDocumentClassifier_inputDataConfig' - Specifies the format and location of the input data for the job.
--
-- 'languageCode', 'createDocumentClassifier_languageCode' - The language of the input documents. You can specify any of the
-- following languages supported by Amazon Comprehend: German (\"de\"),
-- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
-- or Portuguese (\"pt\"). All documents must be in the same language.
newCreateDocumentClassifier ::
  -- | 'documentClassifierName'
  Prelude.Text ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'inputDataConfig'
  DocumentClassifierInputDataConfig ->
  -- | 'languageCode'
  LanguageCode ->
  CreateDocumentClassifier
newCreateDocumentClassifier
  pDocumentClassifierName_
  pDataAccessRoleArn_
  pInputDataConfig_
  pLanguageCode_ =
    CreateDocumentClassifier'
      { clientRequestToken =
          Prelude.Nothing,
        mode = Prelude.Nothing,
        modelKmsKeyId = Prelude.Nothing,
        modelPolicy = Prelude.Nothing,
        outputDataConfig = Prelude.Nothing,
        tags = Prelude.Nothing,
        versionName = Prelude.Nothing,
        volumeKmsKeyId = Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        documentClassifierName = pDocumentClassifierName_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        inputDataConfig = pInputDataConfig_,
        languageCode = pLanguageCode_
      }

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
createDocumentClassifier_clientRequestToken :: Lens.Lens' CreateDocumentClassifier (Prelude.Maybe Prelude.Text)
createDocumentClassifier_clientRequestToken = Lens.lens (\CreateDocumentClassifier' {clientRequestToken} -> clientRequestToken) (\s@CreateDocumentClassifier' {} a -> s {clientRequestToken = a} :: CreateDocumentClassifier)

-- | Indicates the mode in which the classifier will be trained. The
-- classifier can be trained in multi-class mode, which identifies one and
-- only one class for each document, or multi-label mode, which identifies
-- one or more labels for each document. In multi-label mode, multiple
-- labels for an individual document are separated by a delimiter. The
-- default delimiter between labels is a pipe (|).
createDocumentClassifier_mode :: Lens.Lens' CreateDocumentClassifier (Prelude.Maybe DocumentClassifierMode)
createDocumentClassifier_mode = Lens.lens (\CreateDocumentClassifier' {mode} -> mode) (\s@CreateDocumentClassifier' {} a -> s {mode = a} :: CreateDocumentClassifier)

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt trained custom models. The ModelKmsKeyId can be either
-- of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
createDocumentClassifier_modelKmsKeyId :: Lens.Lens' CreateDocumentClassifier (Prelude.Maybe Prelude.Text)
createDocumentClassifier_modelKmsKeyId = Lens.lens (\CreateDocumentClassifier' {modelKmsKeyId} -> modelKmsKeyId) (\s@CreateDocumentClassifier' {} a -> s {modelKmsKeyId = a} :: CreateDocumentClassifier)

-- | The resource-based policy to attach to your custom document classifier
-- model. You can use this policy to allow another AWS account to import
-- your custom model.
--
-- Provide your policy as a JSON body that you enter as a UTF-8 encoded
-- string without line breaks. To provide valid JSON, enclose the attribute
-- names and values in double quotes. If the JSON body is also enclosed in
-- double quotes, then you must escape the double quotes that are inside
-- the policy:
--
-- @\"{\\\"attribute\\\": \\\"value\\\", \\\"attribute\\\": [\\\"value\\\"]}\"@
--
-- To avoid escaping quotes, you can use single quotes to enclose the
-- policy and double quotes to enclose the JSON names and values:
--
-- @\'{\"attribute\": \"value\", \"attribute\": [\"value\"]}\'@
createDocumentClassifier_modelPolicy :: Lens.Lens' CreateDocumentClassifier (Prelude.Maybe Prelude.Text)
createDocumentClassifier_modelPolicy = Lens.lens (\CreateDocumentClassifier' {modelPolicy} -> modelPolicy) (\s@CreateDocumentClassifier' {} a -> s {modelPolicy = a} :: CreateDocumentClassifier)

-- | Enables the addition of output results configuration parameters for
-- custom classifier jobs.
createDocumentClassifier_outputDataConfig :: Lens.Lens' CreateDocumentClassifier (Prelude.Maybe DocumentClassifierOutputDataConfig)
createDocumentClassifier_outputDataConfig = Lens.lens (\CreateDocumentClassifier' {outputDataConfig} -> outputDataConfig) (\s@CreateDocumentClassifier' {} a -> s {outputDataConfig = a} :: CreateDocumentClassifier)

-- | Tags to be associated with the document classifier being created. A tag
-- is a key-value pair that adds as a metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
createDocumentClassifier_tags :: Lens.Lens' CreateDocumentClassifier (Prelude.Maybe [Tag])
createDocumentClassifier_tags = Lens.lens (\CreateDocumentClassifier' {tags} -> tags) (\s@CreateDocumentClassifier' {} a -> s {tags = a} :: CreateDocumentClassifier) Prelude.. Lens.mapping Lens.coerced

-- | The version name given to the newly created classifier. Version names
-- can have a maximum of 256 characters. Alphanumeric characters, hyphens
-- (-) and underscores (_) are allowed. The version name must be unique
-- among all models with the same classifier name in the account\/AWS
-- Region.
createDocumentClassifier_versionName :: Lens.Lens' CreateDocumentClassifier (Prelude.Maybe Prelude.Text)
createDocumentClassifier_versionName = Lens.lens (\CreateDocumentClassifier' {versionName} -> versionName) (\s@CreateDocumentClassifier' {} a -> s {versionName = a} :: CreateDocumentClassifier)

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
createDocumentClassifier_volumeKmsKeyId :: Lens.Lens' CreateDocumentClassifier (Prelude.Maybe Prelude.Text)
createDocumentClassifier_volumeKmsKeyId = Lens.lens (\CreateDocumentClassifier' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@CreateDocumentClassifier' {} a -> s {volumeKmsKeyId = a} :: CreateDocumentClassifier)

-- | Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your custom classifier.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
createDocumentClassifier_vpcConfig :: Lens.Lens' CreateDocumentClassifier (Prelude.Maybe VpcConfig)
createDocumentClassifier_vpcConfig = Lens.lens (\CreateDocumentClassifier' {vpcConfig} -> vpcConfig) (\s@CreateDocumentClassifier' {} a -> s {vpcConfig = a} :: CreateDocumentClassifier)

-- | The name of the document classifier.
createDocumentClassifier_documentClassifierName :: Lens.Lens' CreateDocumentClassifier Prelude.Text
createDocumentClassifier_documentClassifierName = Lens.lens (\CreateDocumentClassifier' {documentClassifierName} -> documentClassifierName) (\s@CreateDocumentClassifier' {} a -> s {documentClassifierName = a} :: CreateDocumentClassifier)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your input data.
createDocumentClassifier_dataAccessRoleArn :: Lens.Lens' CreateDocumentClassifier Prelude.Text
createDocumentClassifier_dataAccessRoleArn = Lens.lens (\CreateDocumentClassifier' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@CreateDocumentClassifier' {} a -> s {dataAccessRoleArn = a} :: CreateDocumentClassifier)

-- | Specifies the format and location of the input data for the job.
createDocumentClassifier_inputDataConfig :: Lens.Lens' CreateDocumentClassifier DocumentClassifierInputDataConfig
createDocumentClassifier_inputDataConfig = Lens.lens (\CreateDocumentClassifier' {inputDataConfig} -> inputDataConfig) (\s@CreateDocumentClassifier' {} a -> s {inputDataConfig = a} :: CreateDocumentClassifier)

-- | The language of the input documents. You can specify any of the
-- following languages supported by Amazon Comprehend: German (\"de\"),
-- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
-- or Portuguese (\"pt\"). All documents must be in the same language.
createDocumentClassifier_languageCode :: Lens.Lens' CreateDocumentClassifier LanguageCode
createDocumentClassifier_languageCode = Lens.lens (\CreateDocumentClassifier' {languageCode} -> languageCode) (\s@CreateDocumentClassifier' {} a -> s {languageCode = a} :: CreateDocumentClassifier)

instance Core.AWSRequest CreateDocumentClassifier where
  type
    AWSResponse CreateDocumentClassifier =
      CreateDocumentClassifierResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDocumentClassifierResponse'
            Prelude.<$> (x Data..?> "DocumentClassifierArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDocumentClassifier where
  hashWithSalt _salt CreateDocumentClassifier' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` modelKmsKeyId
      `Prelude.hashWithSalt` modelPolicy
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` documentClassifierName
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData CreateDocumentClassifier where
  rnf CreateDocumentClassifier' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf modelKmsKeyId
      `Prelude.seq` Prelude.rnf modelPolicy
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf documentClassifierName
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders CreateDocumentClassifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.CreateDocumentClassifier" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDocumentClassifier where
  toJSON CreateDocumentClassifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Mode" Data..=) Prelude.<$> mode,
            ("ModelKmsKeyId" Data..=) Prelude.<$> modelKmsKeyId,
            ("ModelPolicy" Data..=) Prelude.<$> modelPolicy,
            ("OutputDataConfig" Data..=)
              Prelude.<$> outputDataConfig,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VersionName" Data..=) Prelude.<$> versionName,
            ("VolumeKmsKeyId" Data..=)
              Prelude.<$> volumeKmsKeyId,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            Prelude.Just
              ( "DocumentClassifierName"
                  Data..= documentClassifierName
              ),
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn),
            Prelude.Just
              ("InputDataConfig" Data..= inputDataConfig),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath CreateDocumentClassifier where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDocumentClassifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDocumentClassifierResponse' smart constructor.
data CreateDocumentClassifierResponse = CreateDocumentClassifierResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    documentClassifierArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDocumentClassifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentClassifierArn', 'createDocumentClassifierResponse_documentClassifierArn' - The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- 'httpStatus', 'createDocumentClassifierResponse_httpStatus' - The response's http status code.
newCreateDocumentClassifierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDocumentClassifierResponse
newCreateDocumentClassifierResponse pHttpStatus_ =
  CreateDocumentClassifierResponse'
    { documentClassifierArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
createDocumentClassifierResponse_documentClassifierArn :: Lens.Lens' CreateDocumentClassifierResponse (Prelude.Maybe Prelude.Text)
createDocumentClassifierResponse_documentClassifierArn = Lens.lens (\CreateDocumentClassifierResponse' {documentClassifierArn} -> documentClassifierArn) (\s@CreateDocumentClassifierResponse' {} a -> s {documentClassifierArn = a} :: CreateDocumentClassifierResponse)

-- | The response's http status code.
createDocumentClassifierResponse_httpStatus :: Lens.Lens' CreateDocumentClassifierResponse Prelude.Int
createDocumentClassifierResponse_httpStatus = Lens.lens (\CreateDocumentClassifierResponse' {httpStatus} -> httpStatus) (\s@CreateDocumentClassifierResponse' {} a -> s {httpStatus = a} :: CreateDocumentClassifierResponse)

instance
  Prelude.NFData
    CreateDocumentClassifierResponse
  where
  rnf CreateDocumentClassifierResponse' {..} =
    Prelude.rnf documentClassifierArn
      `Prelude.seq` Prelude.rnf httpStatus
