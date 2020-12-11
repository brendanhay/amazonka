{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.CreateDocumentClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document classifier that you can use to categorize documents. To create a classifier, you provide a set of training documents that labeled with the categories that you want to use. After the classifier is trained you can use it to categorize a set of labeled documents into the categories. For more information, see 'how-document-classification' .
module Network.AWS.Comprehend.CreateDocumentClassifier
  ( -- * Creating a request
    CreateDocumentClassifier (..),
    mkCreateDocumentClassifier,

    -- ** Request lenses
    cdcMode,
    cdcVPCConfig,
    cdcVolumeKMSKeyId,
    cdcOutputDataConfig,
    cdcClientRequestToken,
    cdcTags,
    cdcDocumentClassifierName,
    cdcDataAccessRoleARN,
    cdcInputDataConfig,
    cdcLanguageCode,

    -- * Destructuring the response
    CreateDocumentClassifierResponse (..),
    mkCreateDocumentClassifierResponse,

    -- ** Response lenses
    cdcrsDocumentClassifierARN,
    cdcrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDocumentClassifier' smart constructor.
data CreateDocumentClassifier = CreateDocumentClassifier'
  { mode ::
      Lude.Maybe DocumentClassifierMode,
    vpcConfig :: Lude.Maybe VPCConfig,
    volumeKMSKeyId :: Lude.Maybe Lude.Text,
    outputDataConfig ::
      Lude.Maybe
        DocumentClassifierOutputDataConfig,
    clientRequestToken ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    documentClassifierName :: Lude.Text,
    dataAccessRoleARN :: Lude.Text,
    inputDataConfig ::
      DocumentClassifierInputDataConfig,
    languageCode :: LanguageCode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDocumentClassifier' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
-- * 'documentClassifierName' - The name of the document classifier.
-- * 'inputDataConfig' - Specifies the format and location of the input data for the job.
-- * 'languageCode' - The language of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt"). All documents must be in the same language.
-- * 'mode' - Indicates the mode in which the classifier will be trained. The classifier can be trained in multi-class mode, which identifies one and only one class for each document, or multi-label mode, which identifies one or more labels for each document. In multi-label mode, multiple labels for an individual document are separated by a delimiter. The default delimiter between labels is a pipe (|).
-- * 'outputDataConfig' - Enables the addition of output results configuration parameters for custom classifier jobs.
-- * 'tags' - Tags to be associated with the document classifier being created. A tag is a key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with "Sales" as the key might be added to a resource to indicate its use by the sales department.
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your custom classifier. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
mkCreateDocumentClassifier ::
  -- | 'documentClassifierName'
  Lude.Text ->
  -- | 'dataAccessRoleARN'
  Lude.Text ->
  -- | 'inputDataConfig'
  DocumentClassifierInputDataConfig ->
  -- | 'languageCode'
  LanguageCode ->
  CreateDocumentClassifier
mkCreateDocumentClassifier
  pDocumentClassifierName_
  pDataAccessRoleARN_
  pInputDataConfig_
  pLanguageCode_ =
    CreateDocumentClassifier'
      { mode = Lude.Nothing,
        vpcConfig = Lude.Nothing,
        volumeKMSKeyId = Lude.Nothing,
        outputDataConfig = Lude.Nothing,
        clientRequestToken = Lude.Nothing,
        tags = Lude.Nothing,
        documentClassifierName = pDocumentClassifierName_,
        dataAccessRoleARN = pDataAccessRoleARN_,
        inputDataConfig = pInputDataConfig_,
        languageCode = pLanguageCode_
      }

-- | Indicates the mode in which the classifier will be trained. The classifier can be trained in multi-class mode, which identifies one and only one class for each document, or multi-label mode, which identifies one or more labels for each document. In multi-label mode, multiple labels for an individual document are separated by a delimiter. The default delimiter between labels is a pipe (|).
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcMode :: Lens.Lens' CreateDocumentClassifier (Lude.Maybe DocumentClassifierMode)
cdcMode = Lens.lens (mode :: CreateDocumentClassifier -> Lude.Maybe DocumentClassifierMode) (\s a -> s {mode = a} :: CreateDocumentClassifier)
{-# DEPRECATED cdcMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your custom classifier. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcVPCConfig :: Lens.Lens' CreateDocumentClassifier (Lude.Maybe VPCConfig)
cdcVPCConfig = Lens.lens (vpcConfig :: CreateDocumentClassifier -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: CreateDocumentClassifier)
{-# DEPRECATED cdcVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--
-- /Note:/ Consider using 'volumeKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcVolumeKMSKeyId :: Lens.Lens' CreateDocumentClassifier (Lude.Maybe Lude.Text)
cdcVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: CreateDocumentClassifier -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: CreateDocumentClassifier)
{-# DEPRECATED cdcVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | Enables the addition of output results configuration parameters for custom classifier jobs.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcOutputDataConfig :: Lens.Lens' CreateDocumentClassifier (Lude.Maybe DocumentClassifierOutputDataConfig)
cdcOutputDataConfig = Lens.lens (outputDataConfig :: CreateDocumentClassifier -> Lude.Maybe DocumentClassifierOutputDataConfig) (\s a -> s {outputDataConfig = a} :: CreateDocumentClassifier)
{-# DEPRECATED cdcOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcClientRequestToken :: Lens.Lens' CreateDocumentClassifier (Lude.Maybe Lude.Text)
cdcClientRequestToken = Lens.lens (clientRequestToken :: CreateDocumentClassifier -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateDocumentClassifier)
{-# DEPRECATED cdcClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Tags to be associated with the document classifier being created. A tag is a key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with "Sales" as the key might be added to a resource to indicate its use by the sales department.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcTags :: Lens.Lens' CreateDocumentClassifier (Lude.Maybe [Tag])
cdcTags = Lens.lens (tags :: CreateDocumentClassifier -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDocumentClassifier)
{-# DEPRECATED cdcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the document classifier.
--
-- /Note:/ Consider using 'documentClassifierName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDocumentClassifierName :: Lens.Lens' CreateDocumentClassifier Lude.Text
cdcDocumentClassifierName = Lens.lens (documentClassifierName :: CreateDocumentClassifier -> Lude.Text) (\s a -> s {documentClassifierName = a} :: CreateDocumentClassifier)
{-# DEPRECATED cdcDocumentClassifierName "Use generic-lens or generic-optics with 'documentClassifierName' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDataAccessRoleARN :: Lens.Lens' CreateDocumentClassifier Lude.Text
cdcDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: CreateDocumentClassifier -> Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: CreateDocumentClassifier)
{-# DEPRECATED cdcDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcInputDataConfig :: Lens.Lens' CreateDocumentClassifier DocumentClassifierInputDataConfig
cdcInputDataConfig = Lens.lens (inputDataConfig :: CreateDocumentClassifier -> DocumentClassifierInputDataConfig) (\s a -> s {inputDataConfig = a} :: CreateDocumentClassifier)
{-# DEPRECATED cdcInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | The language of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt"). All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcLanguageCode :: Lens.Lens' CreateDocumentClassifier LanguageCode
cdcLanguageCode = Lens.lens (languageCode :: CreateDocumentClassifier -> LanguageCode) (\s a -> s {languageCode = a} :: CreateDocumentClassifier)
{-# DEPRECATED cdcLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Lude.AWSRequest CreateDocumentClassifier where
  type Rs CreateDocumentClassifier = CreateDocumentClassifierResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDocumentClassifierResponse'
            Lude.<$> (x Lude..?> "DocumentClassifierArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDocumentClassifier where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.CreateDocumentClassifier" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDocumentClassifier where
  toJSON CreateDocumentClassifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Mode" Lude..=) Lude.<$> mode,
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId,
            ("OutputDataConfig" Lude..=) Lude.<$> outputDataConfig,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just
              ("DocumentClassifierName" Lude..= documentClassifierName),
            Lude.Just ("DataAccessRoleArn" Lude..= dataAccessRoleARN),
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig),
            Lude.Just ("LanguageCode" Lude..= languageCode)
          ]
      )

instance Lude.ToPath CreateDocumentClassifier where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDocumentClassifier where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDocumentClassifierResponse' smart constructor.
data CreateDocumentClassifierResponse = CreateDocumentClassifierResponse'
  { documentClassifierARN ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDocumentClassifierResponse' with the minimum fields required to make a request.
--
-- * 'documentClassifierARN' - The Amazon Resource Name (ARN) that identifies the document classifier.
-- * 'responseStatus' - The response status code.
mkCreateDocumentClassifierResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDocumentClassifierResponse
mkCreateDocumentClassifierResponse pResponseStatus_ =
  CreateDocumentClassifierResponse'
    { documentClassifierARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- /Note:/ Consider using 'documentClassifierARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsDocumentClassifierARN :: Lens.Lens' CreateDocumentClassifierResponse (Lude.Maybe Lude.Text)
cdcrsDocumentClassifierARN = Lens.lens (documentClassifierARN :: CreateDocumentClassifierResponse -> Lude.Maybe Lude.Text) (\s a -> s {documentClassifierARN = a} :: CreateDocumentClassifierResponse)
{-# DEPRECATED cdcrsDocumentClassifierARN "Use generic-lens or generic-optics with 'documentClassifierARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsResponseStatus :: Lens.Lens' CreateDocumentClassifierResponse Lude.Int
cdcrsResponseStatus = Lens.lens (responseStatus :: CreateDocumentClassifierResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDocumentClassifierResponse)
{-# DEPRECATED cdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
