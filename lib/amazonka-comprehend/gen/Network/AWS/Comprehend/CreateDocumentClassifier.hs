{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
  ( -- * Creating a Request
    createDocumentClassifier,
    CreateDocumentClassifier,

    -- * Request Lenses
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

    -- * Destructuring the Response
    createDocumentClassifierResponse,
    CreateDocumentClassifierResponse,

    -- * Response Lenses
    cdcrsDocumentClassifierARN,
    cdcrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDocumentClassifier' smart constructor.
data CreateDocumentClassifier = CreateDocumentClassifier'
  { _cdcMode ::
      !(Maybe DocumentClassifierMode),
    _cdcVPCConfig :: !(Maybe VPCConfig),
    _cdcVolumeKMSKeyId :: !(Maybe Text),
    _cdcOutputDataConfig ::
      !( Maybe
           DocumentClassifierOutputDataConfig
       ),
    _cdcClientRequestToken :: !(Maybe Text),
    _cdcTags :: !(Maybe [Tag]),
    _cdcDocumentClassifierName :: !Text,
    _cdcDataAccessRoleARN :: !Text,
    _cdcInputDataConfig ::
      !DocumentClassifierInputDataConfig,
    _cdcLanguageCode :: !LanguageCode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDocumentClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcMode' - Indicates the mode in which the classifier will be trained. The classifier can be trained in multi-class mode, which identifies one and only one class for each document, or multi-label mode, which identifies one or more labels for each document. In multi-label mode, multiple labels for an individual document are separated by a delimiter. The default delimiter between labels is a pipe (|).
--
-- * 'cdcVPCConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your custom classifier. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- * 'cdcVolumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
-- * 'cdcOutputDataConfig' - Enables the addition of output results configuration parameters for custom classifier jobs.
--
-- * 'cdcClientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- * 'cdcTags' - Tags to be associated with the document classifier being created. A tag is a key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with "Sales" as the key might be added to a resource to indicate its use by the sales department.
--
-- * 'cdcDocumentClassifierName' - The name of the document classifier.
--
-- * 'cdcDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- * 'cdcInputDataConfig' - Specifies the format and location of the input data for the job.
--
-- * 'cdcLanguageCode' - The language of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt"). All documents must be in the same language.
createDocumentClassifier ::
  -- | 'cdcDocumentClassifierName'
  Text ->
  -- | 'cdcDataAccessRoleARN'
  Text ->
  -- | 'cdcInputDataConfig'
  DocumentClassifierInputDataConfig ->
  -- | 'cdcLanguageCode'
  LanguageCode ->
  CreateDocumentClassifier
createDocumentClassifier
  pDocumentClassifierName_
  pDataAccessRoleARN_
  pInputDataConfig_
  pLanguageCode_ =
    CreateDocumentClassifier'
      { _cdcMode = Nothing,
        _cdcVPCConfig = Nothing,
        _cdcVolumeKMSKeyId = Nothing,
        _cdcOutputDataConfig = Nothing,
        _cdcClientRequestToken = Nothing,
        _cdcTags = Nothing,
        _cdcDocumentClassifierName = pDocumentClassifierName_,
        _cdcDataAccessRoleARN = pDataAccessRoleARN_,
        _cdcInputDataConfig = pInputDataConfig_,
        _cdcLanguageCode = pLanguageCode_
      }

-- | Indicates the mode in which the classifier will be trained. The classifier can be trained in multi-class mode, which identifies one and only one class for each document, or multi-label mode, which identifies one or more labels for each document. In multi-label mode, multiple labels for an individual document are separated by a delimiter. The default delimiter between labels is a pipe (|).
cdcMode :: Lens' CreateDocumentClassifier (Maybe DocumentClassifierMode)
cdcMode = lens _cdcMode (\s a -> s {_cdcMode = a})

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your custom classifier. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
cdcVPCConfig :: Lens' CreateDocumentClassifier (Maybe VPCConfig)
cdcVPCConfig = lens _cdcVPCConfig (\s a -> s {_cdcVPCConfig = a})

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
cdcVolumeKMSKeyId :: Lens' CreateDocumentClassifier (Maybe Text)
cdcVolumeKMSKeyId = lens _cdcVolumeKMSKeyId (\s a -> s {_cdcVolumeKMSKeyId = a})

-- | Enables the addition of output results configuration parameters for custom classifier jobs.
cdcOutputDataConfig :: Lens' CreateDocumentClassifier (Maybe DocumentClassifierOutputDataConfig)
cdcOutputDataConfig = lens _cdcOutputDataConfig (\s a -> s {_cdcOutputDataConfig = a})

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
cdcClientRequestToken :: Lens' CreateDocumentClassifier (Maybe Text)
cdcClientRequestToken = lens _cdcClientRequestToken (\s a -> s {_cdcClientRequestToken = a})

-- | Tags to be associated with the document classifier being created. A tag is a key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with "Sales" as the key might be added to a resource to indicate its use by the sales department.
cdcTags :: Lens' CreateDocumentClassifier [Tag]
cdcTags = lens _cdcTags (\s a -> s {_cdcTags = a}) . _Default . _Coerce

-- | The name of the document classifier.
cdcDocumentClassifierName :: Lens' CreateDocumentClassifier Text
cdcDocumentClassifierName = lens _cdcDocumentClassifierName (\s a -> s {_cdcDocumentClassifierName = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
cdcDataAccessRoleARN :: Lens' CreateDocumentClassifier Text
cdcDataAccessRoleARN = lens _cdcDataAccessRoleARN (\s a -> s {_cdcDataAccessRoleARN = a})

-- | Specifies the format and location of the input data for the job.
cdcInputDataConfig :: Lens' CreateDocumentClassifier DocumentClassifierInputDataConfig
cdcInputDataConfig = lens _cdcInputDataConfig (\s a -> s {_cdcInputDataConfig = a})

-- | The language of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt"). All documents must be in the same language.
cdcLanguageCode :: Lens' CreateDocumentClassifier LanguageCode
cdcLanguageCode = lens _cdcLanguageCode (\s a -> s {_cdcLanguageCode = a})

instance AWSRequest CreateDocumentClassifier where
  type Rs CreateDocumentClassifier = CreateDocumentClassifierResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          CreateDocumentClassifierResponse'
            <$> (x .?> "DocumentClassifierArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateDocumentClassifier

instance NFData CreateDocumentClassifier

instance ToHeaders CreateDocumentClassifier where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.CreateDocumentClassifier" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateDocumentClassifier where
  toJSON CreateDocumentClassifier' {..} =
    object
      ( catMaybes
          [ ("Mode" .=) <$> _cdcMode,
            ("VpcConfig" .=) <$> _cdcVPCConfig,
            ("VolumeKmsKeyId" .=) <$> _cdcVolumeKMSKeyId,
            ("OutputDataConfig" .=) <$> _cdcOutputDataConfig,
            ("ClientRequestToken" .=) <$> _cdcClientRequestToken,
            ("Tags" .=) <$> _cdcTags,
            Just ("DocumentClassifierName" .= _cdcDocumentClassifierName),
            Just ("DataAccessRoleArn" .= _cdcDataAccessRoleARN),
            Just ("InputDataConfig" .= _cdcInputDataConfig),
            Just ("LanguageCode" .= _cdcLanguageCode)
          ]
      )

instance ToPath CreateDocumentClassifier where
  toPath = const "/"

instance ToQuery CreateDocumentClassifier where
  toQuery = const mempty

-- | /See:/ 'createDocumentClassifierResponse' smart constructor.
data CreateDocumentClassifierResponse = CreateDocumentClassifierResponse'
  { _cdcrsDocumentClassifierARN ::
      !(Maybe Text),
    _cdcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDocumentClassifierResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcrsDocumentClassifierARN' - The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- * 'cdcrsResponseStatus' - -- | The response status code.
createDocumentClassifierResponse ::
  -- | 'cdcrsResponseStatus'
  Int ->
  CreateDocumentClassifierResponse
createDocumentClassifierResponse pResponseStatus_ =
  CreateDocumentClassifierResponse'
    { _cdcrsDocumentClassifierARN =
        Nothing,
      _cdcrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
cdcrsDocumentClassifierARN :: Lens' CreateDocumentClassifierResponse (Maybe Text)
cdcrsDocumentClassifierARN = lens _cdcrsDocumentClassifierARN (\s a -> s {_cdcrsDocumentClassifierARN = a})

-- | -- | The response status code.
cdcrsResponseStatus :: Lens' CreateDocumentClassifierResponse Int
cdcrsResponseStatus = lens _cdcrsResponseStatus (\s a -> s {_cdcrsResponseStatus = a})

instance NFData CreateDocumentClassifierResponse
