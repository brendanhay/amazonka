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
-- Module      : Network.AWS.IoT.UpdateStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing stream. The stream version will be incremented by one.
--
--
module Network.AWS.IoT.UpdateStream
    (
    -- * Creating a Request
      updateStream
    , UpdateStream
    -- * Request Lenses
    , usFiles
    , usDescription
    , usRoleARN
    , usStreamId

    -- * Destructuring the Response
    , updateStreamResponse
    , UpdateStreamResponse
    -- * Response Lenses
    , usrsStreamVersion
    , usrsStreamARN
    , usrsDescription
    , usrsStreamId
    , usrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateStream' smart constructor.
data UpdateStream = UpdateStream'
  { _usFiles       :: !(Maybe (List1 StreamFile))
  , _usDescription :: !(Maybe Text)
  , _usRoleARN     :: !(Maybe Text)
  , _usStreamId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usFiles' - The files associated with the stream.
--
-- * 'usDescription' - The description of the stream.
--
-- * 'usRoleARN' - An IAM role that allows the IoT service principal assumes to access your S3 files.
--
-- * 'usStreamId' - The stream ID.
updateStream
    :: Text -- ^ 'usStreamId'
    -> UpdateStream
updateStream pStreamId_ =
  UpdateStream'
    { _usFiles = Nothing
    , _usDescription = Nothing
    , _usRoleARN = Nothing
    , _usStreamId = pStreamId_
    }


-- | The files associated with the stream.
usFiles :: Lens' UpdateStream (Maybe (NonEmpty StreamFile))
usFiles = lens _usFiles (\ s a -> s{_usFiles = a}) . mapping _List1

-- | The description of the stream.
usDescription :: Lens' UpdateStream (Maybe Text)
usDescription = lens _usDescription (\ s a -> s{_usDescription = a})

-- | An IAM role that allows the IoT service principal assumes to access your S3 files.
usRoleARN :: Lens' UpdateStream (Maybe Text)
usRoleARN = lens _usRoleARN (\ s a -> s{_usRoleARN = a})

-- | The stream ID.
usStreamId :: Lens' UpdateStream Text
usStreamId = lens _usStreamId (\ s a -> s{_usStreamId = a})

instance AWSRequest UpdateStream where
        type Rs UpdateStream = UpdateStreamResponse
        request = putJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 UpdateStreamResponse' <$>
                   (x .?> "streamVersion") <*> (x .?> "streamArn") <*>
                     (x .?> "description")
                     <*> (x .?> "streamId")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateStream where

instance NFData UpdateStream where

instance ToHeaders UpdateStream where
        toHeaders = const mempty

instance ToJSON UpdateStream where
        toJSON UpdateStream'{..}
          = object
              (catMaybes
                 [("files" .=) <$> _usFiles,
                  ("description" .=) <$> _usDescription,
                  ("roleArn" .=) <$> _usRoleARN])

instance ToPath UpdateStream where
        toPath UpdateStream'{..}
          = mconcat ["/streams/", toBS _usStreamId]

instance ToQuery UpdateStream where
        toQuery = const mempty

-- | /See:/ 'updateStreamResponse' smart constructor.
data UpdateStreamResponse = UpdateStreamResponse'
  { _usrsStreamVersion  :: !(Maybe Nat)
  , _usrsStreamARN      :: !(Maybe Text)
  , _usrsDescription    :: !(Maybe Text)
  , _usrsStreamId       :: !(Maybe Text)
  , _usrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsStreamVersion' - The stream version.
--
-- * 'usrsStreamARN' - The stream ARN.
--
-- * 'usrsDescription' - A description of the stream.
--
-- * 'usrsStreamId' - The stream ID.
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateStreamResponse
    :: Int -- ^ 'usrsResponseStatus'
    -> UpdateStreamResponse
updateStreamResponse pResponseStatus_ =
  UpdateStreamResponse'
    { _usrsStreamVersion = Nothing
    , _usrsStreamARN = Nothing
    , _usrsDescription = Nothing
    , _usrsStreamId = Nothing
    , _usrsResponseStatus = pResponseStatus_
    }


-- | The stream version.
usrsStreamVersion :: Lens' UpdateStreamResponse (Maybe Natural)
usrsStreamVersion = lens _usrsStreamVersion (\ s a -> s{_usrsStreamVersion = a}) . mapping _Nat

-- | The stream ARN.
usrsStreamARN :: Lens' UpdateStreamResponse (Maybe Text)
usrsStreamARN = lens _usrsStreamARN (\ s a -> s{_usrsStreamARN = a})

-- | A description of the stream.
usrsDescription :: Lens' UpdateStreamResponse (Maybe Text)
usrsDescription = lens _usrsDescription (\ s a -> s{_usrsDescription = a})

-- | The stream ID.
usrsStreamId :: Lens' UpdateStreamResponse (Maybe Text)
usrsStreamId = lens _usrsStreamId (\ s a -> s{_usrsStreamId = a})

-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateStreamResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\ s a -> s{_usrsResponseStatus = a})

instance NFData UpdateStreamResponse where
