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
-- Module      : Network.AWS.Rekognition.DeleteCollection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified collection. Note that this operation removes all faces in the collection. For an example, see 'delete-collection-procedure' .
--
--
-- This operation requires permissions to perform the @rekognition:DeleteCollection@ action.
--
module Network.AWS.Rekognition.DeleteCollection
    (
    -- * Creating a Request
      deleteCollection
    , DeleteCollection
    -- * Request Lenses
    , dcCollectionId

    -- * Destructuring the Response
    , deleteCollectionResponse
    , DeleteCollectionResponse
    -- * Response Lenses
    , dcrsStatusCode
    , dcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCollection' smart constructor.
newtype DeleteCollection = DeleteCollection'
  { _dcCollectionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCollection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCollectionId' - ID of the collection to delete.
deleteCollection
    :: Text -- ^ 'dcCollectionId'
    -> DeleteCollection
deleteCollection pCollectionId_ =
  DeleteCollection' {_dcCollectionId = pCollectionId_}


-- | ID of the collection to delete.
dcCollectionId :: Lens' DeleteCollection Text
dcCollectionId = lens _dcCollectionId (\ s a -> s{_dcCollectionId = a})

instance AWSRequest DeleteCollection where
        type Rs DeleteCollection = DeleteCollectionResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 DeleteCollectionResponse' <$>
                   (x .?> "StatusCode") <*> (pure (fromEnum s)))

instance Hashable DeleteCollection where

instance NFData DeleteCollection where

instance ToHeaders DeleteCollection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.DeleteCollection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteCollection where
        toJSON DeleteCollection'{..}
          = object
              (catMaybes
                 [Just ("CollectionId" .= _dcCollectionId)])

instance ToPath DeleteCollection where
        toPath = const "/"

instance ToQuery DeleteCollection where
        toQuery = const mempty

-- | /See:/ 'deleteCollectionResponse' smart constructor.
data DeleteCollectionResponse = DeleteCollectionResponse'
  { _dcrsStatusCode     :: !(Maybe Nat)
  , _dcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCollectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsStatusCode' - HTTP status code that indicates the result of the operation.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
deleteCollectionResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DeleteCollectionResponse
deleteCollectionResponse pResponseStatus_ =
  DeleteCollectionResponse'
    {_dcrsStatusCode = Nothing, _dcrsResponseStatus = pResponseStatus_}


-- | HTTP status code that indicates the result of the operation.
dcrsStatusCode :: Lens' DeleteCollectionResponse (Maybe Natural)
dcrsStatusCode = lens _dcrsStatusCode (\ s a -> s{_dcrsStatusCode = a}) . mapping _Nat

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DeleteCollectionResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DeleteCollectionResponse where
