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
-- Module      : Network.AWS.Comprehend.CreateEntityRecognizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an entity recognizer using submitted files. After your @CreateEntityRecognizer@ request is submitted, you can check job status using the API.
module Network.AWS.Comprehend.CreateEntityRecognizer
  ( -- * Creating a Request
    createEntityRecognizer,
    CreateEntityRecognizer,

    -- * Request Lenses
    cerVPCConfig,
    cerVolumeKMSKeyId,
    cerClientRequestToken,
    cerTags,
    cerRecognizerName,
    cerDataAccessRoleARN,
    cerInputDataConfig,
    cerLanguageCode,

    -- * Destructuring the Response
    createEntityRecognizerResponse,
    CreateEntityRecognizerResponse,

    -- * Response Lenses
    cerrsEntityRecognizerARN,
    cerrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createEntityRecognizer' smart constructor.
data CreateEntityRecognizer = CreateEntityRecognizer'
  { _cerVPCConfig ::
      !(Maybe VPCConfig),
    _cerVolumeKMSKeyId :: !(Maybe Text),
    _cerClientRequestToken :: !(Maybe Text),
    _cerTags :: !(Maybe [Tag]),
    _cerRecognizerName :: !Text,
    _cerDataAccessRoleARN :: !Text,
    _cerInputDataConfig ::
      !EntityRecognizerInputDataConfig,
    _cerLanguageCode :: !LanguageCode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateEntityRecognizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cerVPCConfig' - Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your custom entity recognizer. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- * 'cerVolumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
-- * 'cerClientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- * 'cerTags' - Tags to be associated with the entity recognizer being created. A tag is a key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with "Sales" as the key might be added to a resource to indicate its use by the sales department.
--
-- * 'cerRecognizerName' - The name given to the newly created recognizer. Recognizer names can be a maximum of 256 characters. Alphanumeric characters, hyphens (-) and underscores (_) are allowed. The name must be unique in the account/region.
--
-- * 'cerDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- * 'cerInputDataConfig' - Specifies the format and location of the input data. The S3 bucket containing the input data must be located in the same region as the entity recognizer being created.
--
-- * 'cerLanguageCode' - You can specify any of the following languages supported by Amazon Comprehend: English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), German ("de"), or Portuguese ("pt"). All documents must be in the same language.
createEntityRecognizer ::
  -- | 'cerRecognizerName'
  Text ->
  -- | 'cerDataAccessRoleARN'
  Text ->
  -- | 'cerInputDataConfig'
  EntityRecognizerInputDataConfig ->
  -- | 'cerLanguageCode'
  LanguageCode ->
  CreateEntityRecognizer
createEntityRecognizer
  pRecognizerName_
  pDataAccessRoleARN_
  pInputDataConfig_
  pLanguageCode_ =
    CreateEntityRecognizer'
      { _cerVPCConfig = Nothing,
        _cerVolumeKMSKeyId = Nothing,
        _cerClientRequestToken = Nothing,
        _cerTags = Nothing,
        _cerRecognizerName = pRecognizerName_,
        _cerDataAccessRoleARN = pDataAccessRoleARN_,
        _cerInputDataConfig = pInputDataConfig_,
        _cerLanguageCode = pLanguageCode_
      }

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for your custom entity recognizer. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
cerVPCConfig :: Lens' CreateEntityRecognizer (Maybe VPCConfig)
cerVPCConfig = lens _cerVPCConfig (\s a -> s {_cerVPCConfig = a})

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
cerVolumeKMSKeyId :: Lens' CreateEntityRecognizer (Maybe Text)
cerVolumeKMSKeyId = lens _cerVolumeKMSKeyId (\s a -> s {_cerVolumeKMSKeyId = a})

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
cerClientRequestToken :: Lens' CreateEntityRecognizer (Maybe Text)
cerClientRequestToken = lens _cerClientRequestToken (\s a -> s {_cerClientRequestToken = a})

-- | Tags to be associated with the entity recognizer being created. A tag is a key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with "Sales" as the key might be added to a resource to indicate its use by the sales department.
cerTags :: Lens' CreateEntityRecognizer [Tag]
cerTags = lens _cerTags (\s a -> s {_cerTags = a}) . _Default . _Coerce

-- | The name given to the newly created recognizer. Recognizer names can be a maximum of 256 characters. Alphanumeric characters, hyphens (-) and underscores (_) are allowed. The name must be unique in the account/region.
cerRecognizerName :: Lens' CreateEntityRecognizer Text
cerRecognizerName = lens _cerRecognizerName (\s a -> s {_cerRecognizerName = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
cerDataAccessRoleARN :: Lens' CreateEntityRecognizer Text
cerDataAccessRoleARN = lens _cerDataAccessRoleARN (\s a -> s {_cerDataAccessRoleARN = a})

-- | Specifies the format and location of the input data. The S3 bucket containing the input data must be located in the same region as the entity recognizer being created.
cerInputDataConfig :: Lens' CreateEntityRecognizer EntityRecognizerInputDataConfig
cerInputDataConfig = lens _cerInputDataConfig (\s a -> s {_cerInputDataConfig = a})

-- | You can specify any of the following languages supported by Amazon Comprehend: English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), German ("de"), or Portuguese ("pt"). All documents must be in the same language.
cerLanguageCode :: Lens' CreateEntityRecognizer LanguageCode
cerLanguageCode = lens _cerLanguageCode (\s a -> s {_cerLanguageCode = a})

instance AWSRequest CreateEntityRecognizer where
  type Rs CreateEntityRecognizer = CreateEntityRecognizerResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          CreateEntityRecognizerResponse'
            <$> (x .?> "EntityRecognizerArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateEntityRecognizer

instance NFData CreateEntityRecognizer

instance ToHeaders CreateEntityRecognizer where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.CreateEntityRecognizer" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateEntityRecognizer where
  toJSON CreateEntityRecognizer' {..} =
    object
      ( catMaybes
          [ ("VpcConfig" .=) <$> _cerVPCConfig,
            ("VolumeKmsKeyId" .=) <$> _cerVolumeKMSKeyId,
            ("ClientRequestToken" .=) <$> _cerClientRequestToken,
            ("Tags" .=) <$> _cerTags,
            Just ("RecognizerName" .= _cerRecognizerName),
            Just ("DataAccessRoleArn" .= _cerDataAccessRoleARN),
            Just ("InputDataConfig" .= _cerInputDataConfig),
            Just ("LanguageCode" .= _cerLanguageCode)
          ]
      )

instance ToPath CreateEntityRecognizer where
  toPath = const "/"

instance ToQuery CreateEntityRecognizer where
  toQuery = const mempty

-- | /See:/ 'createEntityRecognizerResponse' smart constructor.
data CreateEntityRecognizerResponse = CreateEntityRecognizerResponse'
  { _cerrsEntityRecognizerARN ::
      !(Maybe Text),
    _cerrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateEntityRecognizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cerrsEntityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- * 'cerrsResponseStatus' - -- | The response status code.
createEntityRecognizerResponse ::
  -- | 'cerrsResponseStatus'
  Int ->
  CreateEntityRecognizerResponse
createEntityRecognizerResponse pResponseStatus_ =
  CreateEntityRecognizerResponse'
    { _cerrsEntityRecognizerARN =
        Nothing,
      _cerrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
cerrsEntityRecognizerARN :: Lens' CreateEntityRecognizerResponse (Maybe Text)
cerrsEntityRecognizerARN = lens _cerrsEntityRecognizerARN (\s a -> s {_cerrsEntityRecognizerARN = a})

-- | -- | The response status code.
cerrsResponseStatus :: Lens' CreateEntityRecognizerResponse Int
cerrsResponseStatus = lens _cerrsResponseStatus (\s a -> s {_cerrsResponseStatus = a})

instance NFData CreateEntityRecognizerResponse
