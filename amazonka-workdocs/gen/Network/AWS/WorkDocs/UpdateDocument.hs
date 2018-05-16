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
-- Module      : Network.AWS.WorkDocs.UpdateDocument
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of a document. The user must have access to both the document and its parent folder, if applicable.
--
--
module Network.AWS.WorkDocs.UpdateDocument
    (
    -- * Creating a Request
      updateDocument
    , UpdateDocument
    -- * Request Lenses
    , udParentFolderId
    , udAuthenticationToken
    , udName
    , udResourceState
    , udDocumentId

    -- * Destructuring the Response
    , updateDocumentResponse
    , UpdateDocumentResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'updateDocument' smart constructor.
data UpdateDocument = UpdateDocument'
  { _udParentFolderId      :: !(Maybe Text)
  , _udAuthenticationToken :: !(Maybe (Sensitive Text))
  , _udName                :: !(Maybe Text)
  , _udResourceState       :: !(Maybe ResourceStateType)
  , _udDocumentId          :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udParentFolderId' - The ID of the parent folder.
--
-- * 'udAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'udName' - The name of the document.
--
-- * 'udResourceState' - The resource state of the document. Only ACTIVE and RECYCLED are supported.
--
-- * 'udDocumentId' - The ID of the document.
updateDocument
    :: Text -- ^ 'udDocumentId'
    -> UpdateDocument
updateDocument pDocumentId_ =
  UpdateDocument'
    { _udParentFolderId = Nothing
    , _udAuthenticationToken = Nothing
    , _udName = Nothing
    , _udResourceState = Nothing
    , _udDocumentId = pDocumentId_
    }


-- | The ID of the parent folder.
udParentFolderId :: Lens' UpdateDocument (Maybe Text)
udParentFolderId = lens _udParentFolderId (\ s a -> s{_udParentFolderId = a})

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
udAuthenticationToken :: Lens' UpdateDocument (Maybe Text)
udAuthenticationToken = lens _udAuthenticationToken (\ s a -> s{_udAuthenticationToken = a}) . mapping _Sensitive

-- | The name of the document.
udName :: Lens' UpdateDocument (Maybe Text)
udName = lens _udName (\ s a -> s{_udName = a})

-- | The resource state of the document. Only ACTIVE and RECYCLED are supported.
udResourceState :: Lens' UpdateDocument (Maybe ResourceStateType)
udResourceState = lens _udResourceState (\ s a -> s{_udResourceState = a})

-- | The ID of the document.
udDocumentId :: Lens' UpdateDocument Text
udDocumentId = lens _udDocumentId (\ s a -> s{_udDocumentId = a})

instance AWSRequest UpdateDocument where
        type Rs UpdateDocument = UpdateDocumentResponse
        request = patchJSON workDocs
        response = receiveNull UpdateDocumentResponse'

instance Hashable UpdateDocument where

instance NFData UpdateDocument where

instance ToHeaders UpdateDocument where
        toHeaders UpdateDocument'{..}
          = mconcat
              ["Authentication" =# _udAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON UpdateDocument where
        toJSON UpdateDocument'{..}
          = object
              (catMaybes
                 [("ParentFolderId" .=) <$> _udParentFolderId,
                  ("Name" .=) <$> _udName,
                  ("ResourceState" .=) <$> _udResourceState])

instance ToPath UpdateDocument where
        toPath UpdateDocument'{..}
          = mconcat ["/api/v1/documents/", toBS _udDocumentId]

instance ToQuery UpdateDocument where
        toQuery = const mempty

-- | /See:/ 'updateDocumentResponse' smart constructor.
data UpdateDocumentResponse =
  UpdateDocumentResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDocumentResponse' with the minimum fields required to make a request.
--
updateDocumentResponse
    :: UpdateDocumentResponse
updateDocumentResponse = UpdateDocumentResponse'


instance NFData UpdateDocumentResponse where
