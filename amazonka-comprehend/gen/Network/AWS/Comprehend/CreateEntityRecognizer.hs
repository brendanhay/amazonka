{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.CreateEntityRecognizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an entity recognizer using submitted files. After your @CreateEntityRecognizer@ request is submitted, you can check job status using the API.
--
--
module Network.AWS.Comprehend.CreateEntityRecognizer
    (
    -- * Creating a Request
      createEntityRecognizer
    , CreateEntityRecognizer
    -- * Request Lenses
    , cerClientRequestToken
    , cerRecognizerName
    , cerDataAccessRoleARN
    , cerInputDataConfig
    , cerLanguageCode

    -- * Destructuring the Response
    , createEntityRecognizerResponse
    , CreateEntityRecognizerResponse
    -- * Response Lenses
    , cerrsEntityRecognizerARN
    , cerrsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createEntityRecognizer' smart constructor.
data CreateEntityRecognizer = CreateEntityRecognizer'
  { _cerClientRequestToken :: !(Maybe Text)
  , _cerRecognizerName     :: !Text
  , _cerDataAccessRoleARN  :: !Text
  , _cerInputDataConfig    :: !EntityRecognizerInputDataConfig
  , _cerLanguageCode       :: !LanguageCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEntityRecognizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cerClientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- * 'cerRecognizerName' - The name given to the newly created recognizer. Recognizer names can be a maximum of 256 characters. Alphanumeric characters, hyphens (-) and underscores (_) are allowed. The name must be unique in the account/region.
--
-- * 'cerDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- * 'cerInputDataConfig' - Specifies the format and location of the input data. The S3 bucket containing the input data must be located in the same region as the entity recognizer being created.
--
-- * 'cerLanguageCode' - The language of the input documents. All documents must be in the same language. Only English ("en") is currently supported.
createEntityRecognizer
    :: Text -- ^ 'cerRecognizerName'
    -> Text -- ^ 'cerDataAccessRoleARN'
    -> EntityRecognizerInputDataConfig -- ^ 'cerInputDataConfig'
    -> LanguageCode -- ^ 'cerLanguageCode'
    -> CreateEntityRecognizer
createEntityRecognizer pRecognizerName_ pDataAccessRoleARN_ pInputDataConfig_ pLanguageCode_ =
  CreateEntityRecognizer'
    { _cerClientRequestToken = Nothing
    , _cerRecognizerName = pRecognizerName_
    , _cerDataAccessRoleARN = pDataAccessRoleARN_
    , _cerInputDataConfig = pInputDataConfig_
    , _cerLanguageCode = pLanguageCode_
    }


-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
cerClientRequestToken :: Lens' CreateEntityRecognizer (Maybe Text)
cerClientRequestToken = lens _cerClientRequestToken (\ s a -> s{_cerClientRequestToken = a})

-- | The name given to the newly created recognizer. Recognizer names can be a maximum of 256 characters. Alphanumeric characters, hyphens (-) and underscores (_) are allowed. The name must be unique in the account/region.
cerRecognizerName :: Lens' CreateEntityRecognizer Text
cerRecognizerName = lens _cerRecognizerName (\ s a -> s{_cerRecognizerName = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
cerDataAccessRoleARN :: Lens' CreateEntityRecognizer Text
cerDataAccessRoleARN = lens _cerDataAccessRoleARN (\ s a -> s{_cerDataAccessRoleARN = a})

-- | Specifies the format and location of the input data. The S3 bucket containing the input data must be located in the same region as the entity recognizer being created.
cerInputDataConfig :: Lens' CreateEntityRecognizer EntityRecognizerInputDataConfig
cerInputDataConfig = lens _cerInputDataConfig (\ s a -> s{_cerInputDataConfig = a})

-- | The language of the input documents. All documents must be in the same language. Only English ("en") is currently supported.
cerLanguageCode :: Lens' CreateEntityRecognizer LanguageCode
cerLanguageCode = lens _cerLanguageCode (\ s a -> s{_cerLanguageCode = a})

instance AWSRequest CreateEntityRecognizer where
        type Rs CreateEntityRecognizer =
             CreateEntityRecognizerResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 CreateEntityRecognizerResponse' <$>
                   (x .?> "EntityRecognizerArn") <*>
                     (pure (fromEnum s)))

instance Hashable CreateEntityRecognizer where

instance NFData CreateEntityRecognizer where

instance ToHeaders CreateEntityRecognizer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.CreateEntityRecognizer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateEntityRecognizer where
        toJSON CreateEntityRecognizer'{..}
          = object
              (catMaybes
                 [("ClientRequestToken" .=) <$>
                    _cerClientRequestToken,
                  Just ("RecognizerName" .= _cerRecognizerName),
                  Just ("DataAccessRoleArn" .= _cerDataAccessRoleARN),
                  Just ("InputDataConfig" .= _cerInputDataConfig),
                  Just ("LanguageCode" .= _cerLanguageCode)])

instance ToPath CreateEntityRecognizer where
        toPath = const "/"

instance ToQuery CreateEntityRecognizer where
        toQuery = const mempty

-- | /See:/ 'createEntityRecognizerResponse' smart constructor.
data CreateEntityRecognizerResponse = CreateEntityRecognizerResponse'
  { _cerrsEntityRecognizerARN :: !(Maybe Text)
  , _cerrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEntityRecognizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cerrsEntityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- * 'cerrsResponseStatus' - -- | The response status code.
createEntityRecognizerResponse
    :: Int -- ^ 'cerrsResponseStatus'
    -> CreateEntityRecognizerResponse
createEntityRecognizerResponse pResponseStatus_ =
  CreateEntityRecognizerResponse'
    { _cerrsEntityRecognizerARN = Nothing
    , _cerrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
cerrsEntityRecognizerARN :: Lens' CreateEntityRecognizerResponse (Maybe Text)
cerrsEntityRecognizerARN = lens _cerrsEntityRecognizerARN (\ s a -> s{_cerrsEntityRecognizerARN = a})

-- | -- | The response status code.
cerrsResponseStatus :: Lens' CreateEntityRecognizerResponse Int
cerrsResponseStatus = lens _cerrsResponseStatus (\ s a -> s{_cerrsResponseStatus = a})

instance NFData CreateEntityRecognizerResponse where
