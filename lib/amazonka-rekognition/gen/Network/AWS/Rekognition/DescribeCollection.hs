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
-- Module      : Network.AWS.Rekognition.DescribeCollection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified collection. You can use @DescribeCollection@ to get information, such as the number of faces indexed into a collection and the version of the model used by the collection for face detection.
--
--
-- For more information, see Describing a Collection in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.DescribeCollection
  ( -- * Creating a Request
    describeCollection,
    DescribeCollection,

    -- * Request Lenses
    dCollectionId,

    -- * Destructuring the Response
    describeCollectionResponse,
    DescribeCollectionResponse,

    -- * Response Lenses
    drsFaceModelVersion,
    drsFaceCount,
    drsCreationTimestamp,
    drsCollectionARN,
    drsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCollection' smart constructor.
newtype DescribeCollection = DescribeCollection'
  { _dCollectionId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCollection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCollectionId' - The ID of the collection to describe.
describeCollection ::
  -- | 'dCollectionId'
  Text ->
  DescribeCollection
describeCollection pCollectionId_ =
  DescribeCollection' {_dCollectionId = pCollectionId_}

-- | The ID of the collection to describe.
dCollectionId :: Lens' DescribeCollection Text
dCollectionId = lens _dCollectionId (\s a -> s {_dCollectionId = a})

instance AWSRequest DescribeCollection where
  type Rs DescribeCollection = DescribeCollectionResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          DescribeCollectionResponse'
            <$> (x .?> "FaceModelVersion")
            <*> (x .?> "FaceCount")
            <*> (x .?> "CreationTimestamp")
            <*> (x .?> "CollectionARN")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeCollection

instance NFData DescribeCollection

instance ToHeaders DescribeCollection where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.DescribeCollection" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeCollection where
  toJSON DescribeCollection' {..} =
    object (catMaybes [Just ("CollectionId" .= _dCollectionId)])

instance ToPath DescribeCollection where
  toPath = const "/"

instance ToQuery DescribeCollection where
  toQuery = const mempty

-- | /See:/ 'describeCollectionResponse' smart constructor.
data DescribeCollectionResponse = DescribeCollectionResponse'
  { _drsFaceModelVersion ::
      !(Maybe Text),
    _drsFaceCount :: !(Maybe Nat),
    _drsCreationTimestamp ::
      !(Maybe POSIX),
    _drsCollectionARN :: !(Maybe Text),
    _drsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCollectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsFaceModelVersion' - The version of the face model that's used by the collection for face detection. For more information, see Model Versioning in the Amazon Rekognition Developer Guide.
--
-- * 'drsFaceCount' - The number of faces that are indexed into the collection. To index faces into a collection, use 'IndexFaces' .
--
-- * 'drsCreationTimestamp' - The number of milliseconds since the Unix epoch time until the creation of the collection. The Unix epoch time is 00:00:00 Coordinated Universal Time (UTC), Thursday, 1 January 1970.
--
-- * 'drsCollectionARN' - The Amazon Resource Name (ARN) of the collection.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeCollectionResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DescribeCollectionResponse
describeCollectionResponse pResponseStatus_ =
  DescribeCollectionResponse'
    { _drsFaceModelVersion = Nothing,
      _drsFaceCount = Nothing,
      _drsCreationTimestamp = Nothing,
      _drsCollectionARN = Nothing,
      _drsResponseStatus = pResponseStatus_
    }

-- | The version of the face model that's used by the collection for face detection. For more information, see Model Versioning in the Amazon Rekognition Developer Guide.
drsFaceModelVersion :: Lens' DescribeCollectionResponse (Maybe Text)
drsFaceModelVersion = lens _drsFaceModelVersion (\s a -> s {_drsFaceModelVersion = a})

-- | The number of faces that are indexed into the collection. To index faces into a collection, use 'IndexFaces' .
drsFaceCount :: Lens' DescribeCollectionResponse (Maybe Natural)
drsFaceCount = lens _drsFaceCount (\s a -> s {_drsFaceCount = a}) . mapping _Nat

-- | The number of milliseconds since the Unix epoch time until the creation of the collection. The Unix epoch time is 00:00:00 Coordinated Universal Time (UTC), Thursday, 1 January 1970.
drsCreationTimestamp :: Lens' DescribeCollectionResponse (Maybe UTCTime)
drsCreationTimestamp = lens _drsCreationTimestamp (\s a -> s {_drsCreationTimestamp = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the collection.
drsCollectionARN :: Lens' DescribeCollectionResponse (Maybe Text)
drsCollectionARN = lens _drsCollectionARN (\s a -> s {_drsCollectionARN = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeCollectionResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

instance NFData DescribeCollectionResponse
