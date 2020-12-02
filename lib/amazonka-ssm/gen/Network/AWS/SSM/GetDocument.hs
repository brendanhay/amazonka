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
-- Module      : Network.AWS.SSM.GetDocument
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contents of the specified Systems Manager document.
--
--
module Network.AWS.SSM.GetDocument
    (
    -- * Creating a Request
      getDocument
    , GetDocument
    -- * Request Lenses
    , gdDocumentFormat
    , gdDocumentVersion
    , gdName

    -- * Destructuring the Response
    , getDocumentResponse
    , GetDocumentResponse
    -- * Response Lenses
    , gdrsDocumentType
    , gdrsContent
    , gdrsDocumentFormat
    , gdrsName
    , gdrsDocumentVersion
    , gdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getDocument' smart constructor.
data GetDocument = GetDocument'
  { _gdDocumentFormat  :: !(Maybe DocumentFormat)
  , _gdDocumentVersion :: !(Maybe Text)
  , _gdName            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdDocumentFormat' - Returns the document in the specified format. The document format can be either JSON or YAML. JSON is the default format.
--
-- * 'gdDocumentVersion' - The document version for which you want information.
--
-- * 'gdName' - The name of the Systems Manager document.
getDocument
    :: Text -- ^ 'gdName'
    -> GetDocument
getDocument pName_ =
  GetDocument'
    { _gdDocumentFormat = Nothing
    , _gdDocumentVersion = Nothing
    , _gdName = pName_
    }


-- | Returns the document in the specified format. The document format can be either JSON or YAML. JSON is the default format.
gdDocumentFormat :: Lens' GetDocument (Maybe DocumentFormat)
gdDocumentFormat = lens _gdDocumentFormat (\ s a -> s{_gdDocumentFormat = a})

-- | The document version for which you want information.
gdDocumentVersion :: Lens' GetDocument (Maybe Text)
gdDocumentVersion = lens _gdDocumentVersion (\ s a -> s{_gdDocumentVersion = a})

-- | The name of the Systems Manager document.
gdName :: Lens' GetDocument Text
gdName = lens _gdName (\ s a -> s{_gdName = a})

instance AWSRequest GetDocument where
        type Rs GetDocument = GetDocumentResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetDocumentResponse' <$>
                   (x .?> "DocumentType") <*> (x .?> "Content") <*>
                     (x .?> "DocumentFormat")
                     <*> (x .?> "Name")
                     <*> (x .?> "DocumentVersion")
                     <*> (pure (fromEnum s)))

instance Hashable GetDocument where

instance NFData GetDocument where

instance ToHeaders GetDocument where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetDocument" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDocument where
        toJSON GetDocument'{..}
          = object
              (catMaybes
                 [("DocumentFormat" .=) <$> _gdDocumentFormat,
                  ("DocumentVersion" .=) <$> _gdDocumentVersion,
                  Just ("Name" .= _gdName)])

instance ToPath GetDocument where
        toPath = const "/"

instance ToQuery GetDocument where
        toQuery = const mempty

-- | /See:/ 'getDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { _gdrsDocumentType    :: !(Maybe DocumentType)
  , _gdrsContent         :: !(Maybe Text)
  , _gdrsDocumentFormat  :: !(Maybe DocumentFormat)
  , _gdrsName            :: !(Maybe Text)
  , _gdrsDocumentVersion :: !(Maybe Text)
  , _gdrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsDocumentType' - The document type.
--
-- * 'gdrsContent' - The contents of the Systems Manager document.
--
-- * 'gdrsDocumentFormat' - The document format, either JSON or YAML.
--
-- * 'gdrsName' - The name of the Systems Manager document.
--
-- * 'gdrsDocumentVersion' - The document version.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDocumentResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDocumentResponse
getDocumentResponse pResponseStatus_ =
  GetDocumentResponse'
    { _gdrsDocumentType = Nothing
    , _gdrsContent = Nothing
    , _gdrsDocumentFormat = Nothing
    , _gdrsName = Nothing
    , _gdrsDocumentVersion = Nothing
    , _gdrsResponseStatus = pResponseStatus_
    }


-- | The document type.
gdrsDocumentType :: Lens' GetDocumentResponse (Maybe DocumentType)
gdrsDocumentType = lens _gdrsDocumentType (\ s a -> s{_gdrsDocumentType = a})

-- | The contents of the Systems Manager document.
gdrsContent :: Lens' GetDocumentResponse (Maybe Text)
gdrsContent = lens _gdrsContent (\ s a -> s{_gdrsContent = a})

-- | The document format, either JSON or YAML.
gdrsDocumentFormat :: Lens' GetDocumentResponse (Maybe DocumentFormat)
gdrsDocumentFormat = lens _gdrsDocumentFormat (\ s a -> s{_gdrsDocumentFormat = a})

-- | The name of the Systems Manager document.
gdrsName :: Lens' GetDocumentResponse (Maybe Text)
gdrsName = lens _gdrsName (\ s a -> s{_gdrsName = a})

-- | The document version.
gdrsDocumentVersion :: Lens' GetDocumentResponse (Maybe Text)
gdrsDocumentVersion = lens _gdrsDocumentVersion (\ s a -> s{_gdrsDocumentVersion = a})

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDocumentResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

instance NFData GetDocumentResponse where
