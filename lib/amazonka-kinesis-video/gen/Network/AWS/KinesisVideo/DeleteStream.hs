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
-- Module      : Network.AWS.KinesisVideo.DeleteStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Kinesis video stream and the data contained in the stream.
--
--
-- This method marks the stream for deletion, and makes the data in the stream inaccessible immediately.
--
--
--
-- To ensure that you have the latest version of the stream before deleting it, you can specify the stream version. Kinesis Video Streams assigns a version to each stream. When you update a stream, Kinesis Video Streams assigns a new version number. To get the latest stream version, use the @DescribeStream@ API.
--
-- This operation requires permission for the @KinesisVideo:DeleteStream@ action.
--
module Network.AWS.KinesisVideo.DeleteStream
    (
    -- * Creating a Request
      deleteStream
    , DeleteStream
    -- * Request Lenses
    , dsCurrentVersion
    , dsStreamARN

    -- * Destructuring the Response
    , deleteStreamResponse
    , DeleteStreamResponse
    -- * Response Lenses
    , dsrsResponseStatus
    ) where

import Network.AWS.KinesisVideo.Types
import Network.AWS.KinesisVideo.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteStream' smart constructor.
data DeleteStream = DeleteStream'
  { _dsCurrentVersion :: !(Maybe Text)
  , _dsStreamARN      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsCurrentVersion' - Optional: The version of the stream that you want to delete.  Specify the version as a safeguard to ensure that your are deleting the correct stream. To get the stream version, use the @DescribeStream@ API. If not specified, only the @CreationTime@ is checked before deleting the stream.
--
-- * 'dsStreamARN' - The Amazon Resource Name (ARN) of the stream that you want to delete.
deleteStream
    :: Text -- ^ 'dsStreamARN'
    -> DeleteStream
deleteStream pStreamARN_ =
  DeleteStream' {_dsCurrentVersion = Nothing, _dsStreamARN = pStreamARN_}


-- | Optional: The version of the stream that you want to delete.  Specify the version as a safeguard to ensure that your are deleting the correct stream. To get the stream version, use the @DescribeStream@ API. If not specified, only the @CreationTime@ is checked before deleting the stream.
dsCurrentVersion :: Lens' DeleteStream (Maybe Text)
dsCurrentVersion = lens _dsCurrentVersion (\ s a -> s{_dsCurrentVersion = a})

-- | The Amazon Resource Name (ARN) of the stream that you want to delete.
dsStreamARN :: Lens' DeleteStream Text
dsStreamARN = lens _dsStreamARN (\ s a -> s{_dsStreamARN = a})

instance AWSRequest DeleteStream where
        type Rs DeleteStream = DeleteStreamResponse
        request = postJSON kinesisVideo
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteStreamResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteStream where

instance NFData DeleteStream where

instance ToHeaders DeleteStream where
        toHeaders = const mempty

instance ToJSON DeleteStream where
        toJSON DeleteStream'{..}
          = object
              (catMaybes
                 [("CurrentVersion" .=) <$> _dsCurrentVersion,
                  Just ("StreamARN" .= _dsStreamARN)])

instance ToPath DeleteStream where
        toPath = const "/deleteStream"

instance ToQuery DeleteStream where
        toQuery = const mempty

-- | /See:/ 'deleteStreamResponse' smart constructor.
newtype DeleteStreamResponse = DeleteStreamResponse'
  { _dsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsResponseStatus' - -- | The response status code.
deleteStreamResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DeleteStreamResponse
deleteStreamResponse pResponseStatus_ =
  DeleteStreamResponse' {_dsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsrsResponseStatus :: Lens' DeleteStreamResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DeleteStreamResponse where
