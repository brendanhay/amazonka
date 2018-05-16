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
-- Module      : Network.AWS.IoT.CreateStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stream for delivering one or more large files in chunks over MQTT. A stream transports data bytes in chunks or blocks packaged as MQTT messages from a source like S3. You can have one or more files associated with a stream. The total size of a file associated with the stream cannot exceed more than 2 MB. The stream will be created with version 0. If a stream is created with the same streamID as a stream that existed and was deleted within last 90 days, we will resurrect that old stream by incrementing the version by 1.
--
--
module Network.AWS.IoT.CreateStream
    (
    -- * Creating a Request
      createStream
    , CreateStream
    -- * Request Lenses
    , csDescription
    , csStreamId
    , csFiles
    , csRoleARN

    -- * Destructuring the Response
    , createStreamResponse
    , CreateStreamResponse
    -- * Response Lenses
    , csrsStreamVersion
    , csrsStreamARN
    , csrsDescription
    , csrsStreamId
    , csrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createStream' smart constructor.
data CreateStream = CreateStream'
  { _csDescription :: !(Maybe Text)
  , _csStreamId    :: !Text
  , _csFiles       :: !(List1 StreamFile)
  , _csRoleARN     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csDescription' - A description of the stream.
--
-- * 'csStreamId' - The stream ID.
--
-- * 'csFiles' - The files to stream.
--
-- * 'csRoleARN' - An IAM role that allows the IoT service principal assumes to access your S3 files.
createStream
    :: Text -- ^ 'csStreamId'
    -> NonEmpty StreamFile -- ^ 'csFiles'
    -> Text -- ^ 'csRoleARN'
    -> CreateStream
createStream pStreamId_ pFiles_ pRoleARN_ =
  CreateStream'
    { _csDescription = Nothing
    , _csStreamId = pStreamId_
    , _csFiles = _List1 # pFiles_
    , _csRoleARN = pRoleARN_
    }


-- | A description of the stream.
csDescription :: Lens' CreateStream (Maybe Text)
csDescription = lens _csDescription (\ s a -> s{_csDescription = a})

-- | The stream ID.
csStreamId :: Lens' CreateStream Text
csStreamId = lens _csStreamId (\ s a -> s{_csStreamId = a})

-- | The files to stream.
csFiles :: Lens' CreateStream (NonEmpty StreamFile)
csFiles = lens _csFiles (\ s a -> s{_csFiles = a}) . _List1

-- | An IAM role that allows the IoT service principal assumes to access your S3 files.
csRoleARN :: Lens' CreateStream Text
csRoleARN = lens _csRoleARN (\ s a -> s{_csRoleARN = a})

instance AWSRequest CreateStream where
        type Rs CreateStream = CreateStreamResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CreateStreamResponse' <$>
                   (x .?> "streamVersion") <*> (x .?> "streamArn") <*>
                     (x .?> "description")
                     <*> (x .?> "streamId")
                     <*> (pure (fromEnum s)))

instance Hashable CreateStream where

instance NFData CreateStream where

instance ToHeaders CreateStream where
        toHeaders = const mempty

instance ToJSON CreateStream where
        toJSON CreateStream'{..}
          = object
              (catMaybes
                 [("description" .=) <$> _csDescription,
                  Just ("files" .= _csFiles),
                  Just ("roleArn" .= _csRoleARN)])

instance ToPath CreateStream where
        toPath CreateStream'{..}
          = mconcat ["/streams/", toBS _csStreamId]

instance ToQuery CreateStream where
        toQuery = const mempty

-- | /See:/ 'createStreamResponse' smart constructor.
data CreateStreamResponse = CreateStreamResponse'
  { _csrsStreamVersion  :: !(Maybe Nat)
  , _csrsStreamARN      :: !(Maybe Text)
  , _csrsDescription    :: !(Maybe Text)
  , _csrsStreamId       :: !(Maybe Text)
  , _csrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsStreamVersion' - The version of the stream.
--
-- * 'csrsStreamARN' - The stream ARN.
--
-- * 'csrsDescription' - A description of the stream.
--
-- * 'csrsStreamId' - The stream ID.
--
-- * 'csrsResponseStatus' - -- | The response status code.
createStreamResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateStreamResponse
createStreamResponse pResponseStatus_ =
  CreateStreamResponse'
    { _csrsStreamVersion = Nothing
    , _csrsStreamARN = Nothing
    , _csrsDescription = Nothing
    , _csrsStreamId = Nothing
    , _csrsResponseStatus = pResponseStatus_
    }


-- | The version of the stream.
csrsStreamVersion :: Lens' CreateStreamResponse (Maybe Natural)
csrsStreamVersion = lens _csrsStreamVersion (\ s a -> s{_csrsStreamVersion = a}) . mapping _Nat

-- | The stream ARN.
csrsStreamARN :: Lens' CreateStreamResponse (Maybe Text)
csrsStreamARN = lens _csrsStreamARN (\ s a -> s{_csrsStreamARN = a})

-- | A description of the stream.
csrsDescription :: Lens' CreateStreamResponse (Maybe Text)
csrsDescription = lens _csrsDescription (\ s a -> s{_csrsDescription = a})

-- | The stream ID.
csrsStreamId :: Lens' CreateStreamResponse (Maybe Text)
csrsStreamId = lens _csrsStreamId (\ s a -> s{_csrsStreamId = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateStreamResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CreateStreamResponse where
