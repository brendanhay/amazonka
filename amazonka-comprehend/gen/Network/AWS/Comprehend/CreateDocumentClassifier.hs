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
-- Module      : Network.AWS.Comprehend.CreateDocumentClassifier
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document classifier that you can use to categorize documents. To create a classifier you provide a set of training documents that labeled with the categories that you want to use. After the classifier is trained you can use it to categorize a set of labeled documents into the categories. For more information, see 'how-document-classification' .
--
--
module Network.AWS.Comprehend.CreateDocumentClassifier
    (
    -- * Creating a Request
      createDocumentClassifier
    , CreateDocumentClassifier
    -- * Request Lenses
    , cdcClientRequestToken
    , cdcDocumentClassifierName
    , cdcDataAccessRoleARN
    , cdcInputDataConfig
    , cdcLanguageCode

    -- * Destructuring the Response
    , createDocumentClassifierResponse
    , CreateDocumentClassifierResponse
    -- * Response Lenses
    , cdcrsDocumentClassifierARN
    , cdcrsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDocumentClassifier' smart constructor.
data CreateDocumentClassifier = CreateDocumentClassifier'
  { _cdcClientRequestToken     :: !(Maybe Text)
  , _cdcDocumentClassifierName :: !Text
  , _cdcDataAccessRoleARN      :: !Text
  , _cdcInputDataConfig        :: !DocumentClassifierInputDataConfig
  , _cdcLanguageCode           :: !LanguageCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDocumentClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcClientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- * 'cdcDocumentClassifierName' - The name of the document classifier.
--
-- * 'cdcDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- * 'cdcInputDataConfig' - Specifies the format and location of the input data for the job.
--
-- * 'cdcLanguageCode' - The language of the input documents. You can specify English ("en") or Spanish ("es"). All documents must be in the same language.
createDocumentClassifier
    :: Text -- ^ 'cdcDocumentClassifierName'
    -> Text -- ^ 'cdcDataAccessRoleARN'
    -> DocumentClassifierInputDataConfig -- ^ 'cdcInputDataConfig'
    -> LanguageCode -- ^ 'cdcLanguageCode'
    -> CreateDocumentClassifier
createDocumentClassifier pDocumentClassifierName_ pDataAccessRoleARN_ pInputDataConfig_ pLanguageCode_ =
  CreateDocumentClassifier'
    { _cdcClientRequestToken = Nothing
    , _cdcDocumentClassifierName = pDocumentClassifierName_
    , _cdcDataAccessRoleARN = pDataAccessRoleARN_
    , _cdcInputDataConfig = pInputDataConfig_
    , _cdcLanguageCode = pLanguageCode_
    }


-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
cdcClientRequestToken :: Lens' CreateDocumentClassifier (Maybe Text)
cdcClientRequestToken = lens _cdcClientRequestToken (\ s a -> s{_cdcClientRequestToken = a})

-- | The name of the document classifier.
cdcDocumentClassifierName :: Lens' CreateDocumentClassifier Text
cdcDocumentClassifierName = lens _cdcDocumentClassifierName (\ s a -> s{_cdcDocumentClassifierName = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your input data.
cdcDataAccessRoleARN :: Lens' CreateDocumentClassifier Text
cdcDataAccessRoleARN = lens _cdcDataAccessRoleARN (\ s a -> s{_cdcDataAccessRoleARN = a})

-- | Specifies the format and location of the input data for the job.
cdcInputDataConfig :: Lens' CreateDocumentClassifier DocumentClassifierInputDataConfig
cdcInputDataConfig = lens _cdcInputDataConfig (\ s a -> s{_cdcInputDataConfig = a})

-- | The language of the input documents. You can specify English ("en") or Spanish ("es"). All documents must be in the same language.
cdcLanguageCode :: Lens' CreateDocumentClassifier LanguageCode
cdcLanguageCode = lens _cdcLanguageCode (\ s a -> s{_cdcLanguageCode = a})

instance AWSRequest CreateDocumentClassifier where
        type Rs CreateDocumentClassifier =
             CreateDocumentClassifierResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 CreateDocumentClassifierResponse' <$>
                   (x .?> "DocumentClassifierArn") <*>
                     (pure (fromEnum s)))

instance Hashable CreateDocumentClassifier where

instance NFData CreateDocumentClassifier where

instance ToHeaders CreateDocumentClassifier where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.CreateDocumentClassifier" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDocumentClassifier where
        toJSON CreateDocumentClassifier'{..}
          = object
              (catMaybes
                 [("ClientRequestToken" .=) <$>
                    _cdcClientRequestToken,
                  Just
                    ("DocumentClassifierName" .=
                       _cdcDocumentClassifierName),
                  Just ("DataAccessRoleArn" .= _cdcDataAccessRoleARN),
                  Just ("InputDataConfig" .= _cdcInputDataConfig),
                  Just ("LanguageCode" .= _cdcLanguageCode)])

instance ToPath CreateDocumentClassifier where
        toPath = const "/"

instance ToQuery CreateDocumentClassifier where
        toQuery = const mempty

-- | /See:/ 'createDocumentClassifierResponse' smart constructor.
data CreateDocumentClassifierResponse = CreateDocumentClassifierResponse'
  { _cdcrsDocumentClassifierARN :: !(Maybe Text)
  , _cdcrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDocumentClassifierResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcrsDocumentClassifierARN' - The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- * 'cdcrsResponseStatus' - -- | The response status code.
createDocumentClassifierResponse
    :: Int -- ^ 'cdcrsResponseStatus'
    -> CreateDocumentClassifierResponse
createDocumentClassifierResponse pResponseStatus_ =
  CreateDocumentClassifierResponse'
    { _cdcrsDocumentClassifierARN = Nothing
    , _cdcrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) that identifies the document classifier.
cdcrsDocumentClassifierARN :: Lens' CreateDocumentClassifierResponse (Maybe Text)
cdcrsDocumentClassifierARN = lens _cdcrsDocumentClassifierARN (\ s a -> s{_cdcrsDocumentClassifierARN = a})

-- | -- | The response status code.
cdcrsResponseStatus :: Lens' CreateDocumentClassifierResponse Int
cdcrsResponseStatus = lens _cdcrsResponseStatus (\ s a -> s{_cdcrsResponseStatus = a})

instance NFData CreateDocumentClassifierResponse
         where
