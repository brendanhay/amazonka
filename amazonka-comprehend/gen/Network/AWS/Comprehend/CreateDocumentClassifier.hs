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
-- Module      : Network.AWS.Comprehend.CreateDocumentClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- how-document-classification.
module Network.AWS.Comprehend.CreateDocumentClassifier
  ( -- * Creating a Request
    CreateDocumentClassifier (..),
    newCreateDocumentClassifier,

    -- * Request Lenses
    createDocumentClassifier_vpcConfig,
    createDocumentClassifier_mode,
    createDocumentClassifier_outputDataConfig,
    createDocumentClassifier_volumeKmsKeyId,
    createDocumentClassifier_tags,
    createDocumentClassifier_clientRequestToken,
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

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDocumentClassifier' smart constructor.
data CreateDocumentClassifier = CreateDocumentClassifier'
  { -- | Configuration parameters for an optional private Virtual Private Cloud
    -- (VPC) containing the resources you are using for your custom classifier.
    -- For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | Indicates the mode in which the classifier will be trained. The
    -- classifier can be trained in multi-class mode, which identifies one and
    -- only one class for each document, or multi-label mode, which identifies
    -- one or more labels for each document. In multi-label mode, multiple
    -- labels for an individual document are separated by a delimiter. The
    -- default delimiter between labels is a pipe (|).
    mode :: Core.Maybe DocumentClassifierMode,
    -- | Enables the addition of output results configuration parameters for
    -- custom classifier jobs.
    outputDataConfig :: Core.Maybe DocumentClassifierOutputDataConfig,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt data on the storage volume attached to the ML compute
    -- instance(s) that process the analysis job. The VolumeKmsKeyId can be
    -- either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Core.Maybe Core.Text,
    -- | Tags to be associated with the document classifier being created. A tag
    -- is a key-value pair that adds as a metadata to a resource used by Amazon
    -- Comprehend. For example, a tag with \"Sales\" as the key might be added
    -- to a resource to indicate its use by the sales department.
    tags :: Core.Maybe [Tag],
    -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The name of the document classifier.
    documentClassifierName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
    -- role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Core.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: DocumentClassifierInputDataConfig,
    -- | The language of the input documents. You can specify any of the
    -- following languages supported by Amazon Comprehend: German (\"de\"),
    -- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
    -- or Portuguese (\"pt\"). All documents must be in the same language.
    languageCode :: LanguageCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDocumentClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'createDocumentClassifier_vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your custom classifier.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'mode', 'createDocumentClassifier_mode' - Indicates the mode in which the classifier will be trained. The
-- classifier can be trained in multi-class mode, which identifies one and
-- only one class for each document, or multi-label mode, which identifies
-- one or more labels for each document. In multi-label mode, multiple
-- labels for an individual document are separated by a delimiter. The
-- default delimiter between labels is a pipe (|).
--
-- 'outputDataConfig', 'createDocumentClassifier_outputDataConfig' - Enables the addition of output results configuration parameters for
-- custom classifier jobs.
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
-- 'tags', 'createDocumentClassifier_tags' - Tags to be associated with the document classifier being created. A tag
-- is a key-value pair that adds as a metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
--
-- 'clientRequestToken', 'createDocumentClassifier_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
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
  Core.Text ->
  -- | 'dataAccessRoleArn'
  Core.Text ->
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
      { vpcConfig = Core.Nothing,
        mode = Core.Nothing,
        outputDataConfig = Core.Nothing,
        volumeKmsKeyId = Core.Nothing,
        tags = Core.Nothing,
        clientRequestToken = Core.Nothing,
        documentClassifierName = pDocumentClassifierName_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        inputDataConfig = pInputDataConfig_,
        languageCode = pLanguageCode_
      }

-- | Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your custom classifier.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
createDocumentClassifier_vpcConfig :: Lens.Lens' CreateDocumentClassifier (Core.Maybe VpcConfig)
createDocumentClassifier_vpcConfig = Lens.lens (\CreateDocumentClassifier' {vpcConfig} -> vpcConfig) (\s@CreateDocumentClassifier' {} a -> s {vpcConfig = a} :: CreateDocumentClassifier)

-- | Indicates the mode in which the classifier will be trained. The
-- classifier can be trained in multi-class mode, which identifies one and
-- only one class for each document, or multi-label mode, which identifies
-- one or more labels for each document. In multi-label mode, multiple
-- labels for an individual document are separated by a delimiter. The
-- default delimiter between labels is a pipe (|).
createDocumentClassifier_mode :: Lens.Lens' CreateDocumentClassifier (Core.Maybe DocumentClassifierMode)
createDocumentClassifier_mode = Lens.lens (\CreateDocumentClassifier' {mode} -> mode) (\s@CreateDocumentClassifier' {} a -> s {mode = a} :: CreateDocumentClassifier)

-- | Enables the addition of output results configuration parameters for
-- custom classifier jobs.
createDocumentClassifier_outputDataConfig :: Lens.Lens' CreateDocumentClassifier (Core.Maybe DocumentClassifierOutputDataConfig)
createDocumentClassifier_outputDataConfig = Lens.lens (\CreateDocumentClassifier' {outputDataConfig} -> outputDataConfig) (\s@CreateDocumentClassifier' {} a -> s {outputDataConfig = a} :: CreateDocumentClassifier)

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
createDocumentClassifier_volumeKmsKeyId :: Lens.Lens' CreateDocumentClassifier (Core.Maybe Core.Text)
createDocumentClassifier_volumeKmsKeyId = Lens.lens (\CreateDocumentClassifier' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@CreateDocumentClassifier' {} a -> s {volumeKmsKeyId = a} :: CreateDocumentClassifier)

-- | Tags to be associated with the document classifier being created. A tag
-- is a key-value pair that adds as a metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
createDocumentClassifier_tags :: Lens.Lens' CreateDocumentClassifier (Core.Maybe [Tag])
createDocumentClassifier_tags = Lens.lens (\CreateDocumentClassifier' {tags} -> tags) (\s@CreateDocumentClassifier' {} a -> s {tags = a} :: CreateDocumentClassifier) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
createDocumentClassifier_clientRequestToken :: Lens.Lens' CreateDocumentClassifier (Core.Maybe Core.Text)
createDocumentClassifier_clientRequestToken = Lens.lens (\CreateDocumentClassifier' {clientRequestToken} -> clientRequestToken) (\s@CreateDocumentClassifier' {} a -> s {clientRequestToken = a} :: CreateDocumentClassifier)

-- | The name of the document classifier.
createDocumentClassifier_documentClassifierName :: Lens.Lens' CreateDocumentClassifier Core.Text
createDocumentClassifier_documentClassifierName = Lens.lens (\CreateDocumentClassifier' {documentClassifierName} -> documentClassifierName) (\s@CreateDocumentClassifier' {} a -> s {documentClassifierName = a} :: CreateDocumentClassifier)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your input data.
createDocumentClassifier_dataAccessRoleArn :: Lens.Lens' CreateDocumentClassifier Core.Text
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDocumentClassifierResponse'
            Core.<$> (x Core..?> "DocumentClassifierArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDocumentClassifier

instance Core.NFData CreateDocumentClassifier

instance Core.ToHeaders CreateDocumentClassifier where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.CreateDocumentClassifier" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDocumentClassifier where
  toJSON CreateDocumentClassifier' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VpcConfig" Core..=) Core.<$> vpcConfig,
            ("Mode" Core..=) Core.<$> mode,
            ("OutputDataConfig" Core..=)
              Core.<$> outputDataConfig,
            ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId,
            ("Tags" Core..=) Core.<$> tags,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            Core.Just
              ( "DocumentClassifierName"
                  Core..= documentClassifierName
              ),
            Core.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn),
            Core.Just
              ("InputDataConfig" Core..= inputDataConfig),
            Core.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath CreateDocumentClassifier where
  toPath = Core.const "/"

instance Core.ToQuery CreateDocumentClassifier where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDocumentClassifierResponse' smart constructor.
data CreateDocumentClassifierResponse = CreateDocumentClassifierResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    documentClassifierArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateDocumentClassifierResponse
newCreateDocumentClassifierResponse pHttpStatus_ =
  CreateDocumentClassifierResponse'
    { documentClassifierArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
createDocumentClassifierResponse_documentClassifierArn :: Lens.Lens' CreateDocumentClassifierResponse (Core.Maybe Core.Text)
createDocumentClassifierResponse_documentClassifierArn = Lens.lens (\CreateDocumentClassifierResponse' {documentClassifierArn} -> documentClassifierArn) (\s@CreateDocumentClassifierResponse' {} a -> s {documentClassifierArn = a} :: CreateDocumentClassifierResponse)

-- | The response's http status code.
createDocumentClassifierResponse_httpStatus :: Lens.Lens' CreateDocumentClassifierResponse Core.Int
createDocumentClassifierResponse_httpStatus = Lens.lens (\CreateDocumentClassifierResponse' {httpStatus} -> httpStatus) (\s@CreateDocumentClassifierResponse' {} a -> s {httpStatus = a} :: CreateDocumentClassifierResponse)

instance Core.NFData CreateDocumentClassifierResponse
