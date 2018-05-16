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
-- Module      : Network.AWS.SSM.UpdateDocument
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The document you want to update.
--
--
module Network.AWS.SSM.UpdateDocument
    (
    -- * Creating a Request
      updateDocument
    , UpdateDocument
    -- * Request Lenses
    , udTargetType
    , udDocumentFormat
    , udDocumentVersion
    , udContent
    , udName

    -- * Destructuring the Response
    , updateDocumentResponse
    , UpdateDocumentResponse
    -- * Response Lenses
    , udrsDocumentDescription
    , udrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'updateDocument' smart constructor.
data UpdateDocument = UpdateDocument'
  { _udTargetType      :: !(Maybe Text)
  , _udDocumentFormat  :: !(Maybe DocumentFormat)
  , _udDocumentVersion :: !(Maybe Text)
  , _udContent         :: !Text
  , _udName            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udTargetType' - Specify a new target type for the document.
--
-- * 'udDocumentFormat' - Specify the document format for the new document version. Systems Manager supports JSON and YAML documents. JSON is the default format.
--
-- * 'udDocumentVersion' - The version of the document that you want to update.
--
-- * 'udContent' - The content in a document that you want to update.
--
-- * 'udName' - The name of the document that you want to update.
updateDocument
    :: Text -- ^ 'udContent'
    -> Text -- ^ 'udName'
    -> UpdateDocument
updateDocument pContent_ pName_ =
  UpdateDocument'
    { _udTargetType = Nothing
    , _udDocumentFormat = Nothing
    , _udDocumentVersion = Nothing
    , _udContent = pContent_
    , _udName = pName_
    }


-- | Specify a new target type for the document.
udTargetType :: Lens' UpdateDocument (Maybe Text)
udTargetType = lens _udTargetType (\ s a -> s{_udTargetType = a})

-- | Specify the document format for the new document version. Systems Manager supports JSON and YAML documents. JSON is the default format.
udDocumentFormat :: Lens' UpdateDocument (Maybe DocumentFormat)
udDocumentFormat = lens _udDocumentFormat (\ s a -> s{_udDocumentFormat = a})

-- | The version of the document that you want to update.
udDocumentVersion :: Lens' UpdateDocument (Maybe Text)
udDocumentVersion = lens _udDocumentVersion (\ s a -> s{_udDocumentVersion = a})

-- | The content in a document that you want to update.
udContent :: Lens' UpdateDocument Text
udContent = lens _udContent (\ s a -> s{_udContent = a})

-- | The name of the document that you want to update.
udName :: Lens' UpdateDocument Text
udName = lens _udName (\ s a -> s{_udName = a})

instance AWSRequest UpdateDocument where
        type Rs UpdateDocument = UpdateDocumentResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDocumentResponse' <$>
                   (x .?> "DocumentDescription") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateDocument where

instance NFData UpdateDocument where

instance ToHeaders UpdateDocument where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.UpdateDocument" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDocument where
        toJSON UpdateDocument'{..}
          = object
              (catMaybes
                 [("TargetType" .=) <$> _udTargetType,
                  ("DocumentFormat" .=) <$> _udDocumentFormat,
                  ("DocumentVersion" .=) <$> _udDocumentVersion,
                  Just ("Content" .= _udContent),
                  Just ("Name" .= _udName)])

instance ToPath UpdateDocument where
        toPath = const "/"

instance ToQuery UpdateDocument where
        toQuery = const mempty

-- | /See:/ 'updateDocumentResponse' smart constructor.
data UpdateDocumentResponse = UpdateDocumentResponse'
  { _udrsDocumentDescription :: !(Maybe DocumentDescription)
  , _udrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrsDocumentDescription' - A description of the document that was updated.
--
-- * 'udrsResponseStatus' - -- | The response status code.
updateDocumentResponse
    :: Int -- ^ 'udrsResponseStatus'
    -> UpdateDocumentResponse
updateDocumentResponse pResponseStatus_ =
  UpdateDocumentResponse'
    {_udrsDocumentDescription = Nothing, _udrsResponseStatus = pResponseStatus_}


-- | A description of the document that was updated.
udrsDocumentDescription :: Lens' UpdateDocumentResponse (Maybe DocumentDescription)
udrsDocumentDescription = lens _udrsDocumentDescription (\ s a -> s{_udrsDocumentDescription = a})

-- | -- | The response status code.
udrsResponseStatus :: Lens' UpdateDocumentResponse Int
udrsResponseStatus = lens _udrsResponseStatus (\ s a -> s{_udrsResponseStatus = a})

instance NFData UpdateDocumentResponse where
