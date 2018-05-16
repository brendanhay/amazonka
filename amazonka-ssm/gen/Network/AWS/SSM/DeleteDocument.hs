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
-- Module      : Network.AWS.SSM.DeleteDocument
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Systems Manager document and all instance associations to the document.
--
--
-- Before you delete the document, we recommend that you use 'DeleteAssociation' to disassociate all instances that are associated with the document.
--
module Network.AWS.SSM.DeleteDocument
    (
    -- * Creating a Request
      deleteDocument
    , DeleteDocument
    -- * Request Lenses
    , dddName

    -- * Destructuring the Response
    , deleteDocumentResponse
    , DeleteDocumentResponse
    -- * Response Lenses
    , ddrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'deleteDocument' smart constructor.
newtype DeleteDocument = DeleteDocument'
  { _dddName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dddName' - The name of the document.
deleteDocument
    :: Text -- ^ 'dddName'
    -> DeleteDocument
deleteDocument pName_ = DeleteDocument' {_dddName = pName_}


-- | The name of the document.
dddName :: Lens' DeleteDocument Text
dddName = lens _dddName (\ s a -> s{_dddName = a})

instance AWSRequest DeleteDocument where
        type Rs DeleteDocument = DeleteDocumentResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteDocumentResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDocument where

instance NFData DeleteDocument where

instance ToHeaders DeleteDocument where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DeleteDocument" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDocument where
        toJSON DeleteDocument'{..}
          = object (catMaybes [Just ("Name" .= _dddName)])

instance ToPath DeleteDocument where
        toPath = const "/"

instance ToQuery DeleteDocument where
        toQuery = const mempty

-- | /See:/ 'deleteDocumentResponse' smart constructor.
newtype DeleteDocumentResponse = DeleteDocumentResponse'
  { _ddrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsResponseStatus' - -- | The response status code.
deleteDocumentResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DeleteDocumentResponse
deleteDocumentResponse pResponseStatus_ =
  DeleteDocumentResponse' {_ddrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ddrsResponseStatus :: Lens' DeleteDocumentResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

instance NFData DeleteDocumentResponse where
