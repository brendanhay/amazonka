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
-- Module      : Network.AWS.IoTAnalytics.SampleChannelData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a sample of messages from the specified channel ingested during the specified timeframe. Up to 10 messages can be retrieved.
--
--
module Network.AWS.IoTAnalytics.SampleChannelData
    (
    -- * Creating a Request
      sampleChannelData
    , SampleChannelData
    -- * Request Lenses
    , scdStartTime
    , scdMaxMessages
    , scdEndTime
    , scdChannelName

    -- * Destructuring the Response
    , sampleChannelDataResponse
    , SampleChannelDataResponse
    -- * Response Lenses
    , scdrsPayloads
    , scdrsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'sampleChannelData' smart constructor.
data SampleChannelData = SampleChannelData'
  { _scdStartTime   :: !(Maybe POSIX)
  , _scdMaxMessages :: !(Maybe Nat)
  , _scdEndTime     :: !(Maybe POSIX)
  , _scdChannelName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SampleChannelData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scdStartTime' - The start of the time window from which sample messages are retrieved.
--
-- * 'scdMaxMessages' - The number of sample messages to be retrieved. The limit is 10, the default is also 10.
--
-- * 'scdEndTime' - The end of the time window from which sample messages are retrieved.
--
-- * 'scdChannelName' - The name of the channel whose message samples are retrieved.
sampleChannelData
    :: Text -- ^ 'scdChannelName'
    -> SampleChannelData
sampleChannelData pChannelName_ =
  SampleChannelData'
    { _scdStartTime = Nothing
    , _scdMaxMessages = Nothing
    , _scdEndTime = Nothing
    , _scdChannelName = pChannelName_
    }


-- | The start of the time window from which sample messages are retrieved.
scdStartTime :: Lens' SampleChannelData (Maybe UTCTime)
scdStartTime = lens _scdStartTime (\ s a -> s{_scdStartTime = a}) . mapping _Time

-- | The number of sample messages to be retrieved. The limit is 10, the default is also 10.
scdMaxMessages :: Lens' SampleChannelData (Maybe Natural)
scdMaxMessages = lens _scdMaxMessages (\ s a -> s{_scdMaxMessages = a}) . mapping _Nat

-- | The end of the time window from which sample messages are retrieved.
scdEndTime :: Lens' SampleChannelData (Maybe UTCTime)
scdEndTime = lens _scdEndTime (\ s a -> s{_scdEndTime = a}) . mapping _Time

-- | The name of the channel whose message samples are retrieved.
scdChannelName :: Lens' SampleChannelData Text
scdChannelName = lens _scdChannelName (\ s a -> s{_scdChannelName = a})

instance AWSRequest SampleChannelData where
        type Rs SampleChannelData = SampleChannelDataResponse
        request = get ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 SampleChannelDataResponse' <$>
                   (x .?> "payloads") <*> (pure (fromEnum s)))

instance Hashable SampleChannelData where

instance NFData SampleChannelData where

instance ToHeaders SampleChannelData where
        toHeaders = const mempty

instance ToPath SampleChannelData where
        toPath SampleChannelData'{..}
          = mconcat
              ["/channels/", toBS _scdChannelName, "/sample"]

instance ToQuery SampleChannelData where
        toQuery SampleChannelData'{..}
          = mconcat
              ["startTime" =: _scdStartTime,
               "maxMessages" =: _scdMaxMessages,
               "endTime" =: _scdEndTime]

-- | /See:/ 'sampleChannelDataResponse' smart constructor.
data SampleChannelDataResponse = SampleChannelDataResponse'
  { _scdrsPayloads       :: !(Maybe (List1 Base64))
  , _scdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SampleChannelDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scdrsPayloads' - The list of message samples. Each sample message is returned as a base64-encoded string.
--
-- * 'scdrsResponseStatus' - -- | The response status code.
sampleChannelDataResponse
    :: Int -- ^ 'scdrsResponseStatus'
    -> SampleChannelDataResponse
sampleChannelDataResponse pResponseStatus_ =
  SampleChannelDataResponse'
    {_scdrsPayloads = Nothing, _scdrsResponseStatus = pResponseStatus_}


-- | The list of message samples. Each sample message is returned as a base64-encoded string.
scdrsPayloads :: Lens' SampleChannelDataResponse (Maybe (NonEmpty ByteString))
scdrsPayloads = lens _scdrsPayloads (\ s a -> s{_scdrsPayloads = a}) . mapping _List1

-- | -- | The response status code.
scdrsResponseStatus :: Lens' SampleChannelDataResponse Int
scdrsResponseStatus = lens _scdrsResponseStatus (\ s a -> s{_scdrsResponseStatus = a})

instance NFData SampleChannelDataResponse where
