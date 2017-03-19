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
-- Module      : Network.AWS.WorkDocs.DeleteDocument
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified document and its associated metadata.
--
--
module Network.AWS.WorkDocs.DeleteDocument
    (
    -- * Creating a Request
      deleteDocument
    , DeleteDocument
    -- * Request Lenses
    , ddDocumentId

    -- * Destructuring the Response
    , deleteDocumentResponse
    , DeleteDocumentResponse
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkDocs.Types
import           Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deleteDocument' smart constructor.
newtype DeleteDocument = DeleteDocument'
    { _ddDocumentId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDocumentId' - The ID of the document.
deleteDocument
    :: Text -- ^ 'ddDocumentId'
    -> DeleteDocument
deleteDocument pDocumentId_ =
    DeleteDocument'
    { _ddDocumentId = pDocumentId_
    }

-- | The ID of the document.
ddDocumentId :: Lens' DeleteDocument Text
ddDocumentId = lens _ddDocumentId (\ s a -> s{_ddDocumentId = a});

instance AWSRequest DeleteDocument where
        type Rs DeleteDocument = DeleteDocumentResponse
        request = delete workDocs
        response = receiveNull DeleteDocumentResponse'

instance Hashable DeleteDocument

instance NFData DeleteDocument

instance ToHeaders DeleteDocument where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteDocument where
        toPath DeleteDocument'{..}
          = mconcat ["/api/v1/documents/", toBS _ddDocumentId]

instance ToQuery DeleteDocument where
        toQuery = const mempty

-- | /See:/ 'deleteDocumentResponse' smart constructor.
data DeleteDocumentResponse =
    DeleteDocumentResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteDocumentResponse' with the minimum fields required to make a request.
--
deleteDocumentResponse
    :: DeleteDocumentResponse
deleteDocumentResponse = DeleteDocumentResponse'

instance NFData DeleteDocumentResponse
