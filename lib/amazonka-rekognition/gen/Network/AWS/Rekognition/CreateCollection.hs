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
-- Module      : Network.AWS.Rekognition.CreateCollection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a collection in an AWS Region. You can add faces to the collection using the operation.
--
--
-- For example, you might create collections, one for each of your application users. A user can then index faces using the @IndexFaces@ operation and persist results in a specific collection. Then, a user can search the collection for faces in the user-specific container.
--
-- This operation requires permissions to perform the @rekognition:CreateCollection@ action.
--
module Network.AWS.Rekognition.CreateCollection
    (
    -- * Creating a Request
      createCollection
    , CreateCollection
    -- * Request Lenses
    , ccCollectionId

    -- * Destructuring the Response
    , createCollectionResponse
    , CreateCollectionResponse
    -- * Response Lenses
    , ccrsFaceModelVersion
    , ccrsCollectionARN
    , ccrsStatusCode
    , ccrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCollection' smart constructor.
newtype CreateCollection = CreateCollection'
  { _ccCollectionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCollection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCollectionId' - ID for the collection that you are creating.
createCollection
    :: Text -- ^ 'ccCollectionId'
    -> CreateCollection
createCollection pCollectionId_ =
  CreateCollection' {_ccCollectionId = pCollectionId_}


-- | ID for the collection that you are creating.
ccCollectionId :: Lens' CreateCollection Text
ccCollectionId = lens _ccCollectionId (\ s a -> s{_ccCollectionId = a})

instance AWSRequest CreateCollection where
        type Rs CreateCollection = CreateCollectionResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 CreateCollectionResponse' <$>
                   (x .?> "FaceModelVersion") <*>
                     (x .?> "CollectionArn")
                     <*> (x .?> "StatusCode")
                     <*> (pure (fromEnum s)))

instance Hashable CreateCollection where

instance NFData CreateCollection where

instance ToHeaders CreateCollection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.CreateCollection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCollection where
        toJSON CreateCollection'{..}
          = object
              (catMaybes
                 [Just ("CollectionId" .= _ccCollectionId)])

instance ToPath CreateCollection where
        toPath = const "/"

instance ToQuery CreateCollection where
        toQuery = const mempty

-- | /See:/ 'createCollectionResponse' smart constructor.
data CreateCollectionResponse = CreateCollectionResponse'
  { _ccrsFaceModelVersion :: !(Maybe Text)
  , _ccrsCollectionARN    :: !(Maybe Text)
  , _ccrsStatusCode       :: !(Maybe Nat)
  , _ccrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCollectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsFaceModelVersion' - Version number of the face detection model associated with the collection you are creating.
--
-- * 'ccrsCollectionARN' - Amazon Resource Name (ARN) of the collection. You can use this to manage permissions on your resources.
--
-- * 'ccrsStatusCode' - HTTP status code indicating the result of the operation.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createCollectionResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateCollectionResponse
createCollectionResponse pResponseStatus_ =
  CreateCollectionResponse'
    { _ccrsFaceModelVersion = Nothing
    , _ccrsCollectionARN = Nothing
    , _ccrsStatusCode = Nothing
    , _ccrsResponseStatus = pResponseStatus_
    }


-- | Version number of the face detection model associated with the collection you are creating.
ccrsFaceModelVersion :: Lens' CreateCollectionResponse (Maybe Text)
ccrsFaceModelVersion = lens _ccrsFaceModelVersion (\ s a -> s{_ccrsFaceModelVersion = a})

-- | Amazon Resource Name (ARN) of the collection. You can use this to manage permissions on your resources.
ccrsCollectionARN :: Lens' CreateCollectionResponse (Maybe Text)
ccrsCollectionARN = lens _ccrsCollectionARN (\ s a -> s{_ccrsCollectionARN = a})

-- | HTTP status code indicating the result of the operation.
ccrsStatusCode :: Lens' CreateCollectionResponse (Maybe Natural)
ccrsStatusCode = lens _ccrsStatusCode (\ s a -> s{_ccrsStatusCode = a}) . mapping _Nat

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateCollectionResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateCollectionResponse where
