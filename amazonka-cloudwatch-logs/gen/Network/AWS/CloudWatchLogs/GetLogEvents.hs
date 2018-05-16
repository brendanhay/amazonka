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
-- Module      : Network.AWS.CloudWatchLogs.GetLogEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists log events from the specified log stream. You can list all the log events or filter using a time range.
--
--
-- By default, this operation returns as many log events as can fit in a response size of 1MB (up to 10,000 log events). You can get additional log events by specifying one of the tokens in a subsequent call.
--
module Network.AWS.CloudWatchLogs.GetLogEvents
    (
    -- * Creating a Request
      getLogEvents
    , GetLogEvents
    -- * Request Lenses
    , gleStartTime
    , gleStartFromHead
    , gleNextToken
    , gleEndTime
    , gleLimit
    , gleLogGroupName
    , gleLogStreamName

    -- * Destructuring the Response
    , getLogEventsResponse
    , GetLogEventsResponse
    -- * Response Lenses
    , glersNextBackwardToken
    , glersNextForwardToken
    , glersEvents
    , glersResponseStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLogEvents' smart constructor.
data GetLogEvents = GetLogEvents'
  { _gleStartTime     :: !(Maybe Nat)
  , _gleStartFromHead :: !(Maybe Bool)
  , _gleNextToken     :: !(Maybe Text)
  , _gleEndTime       :: !(Maybe Nat)
  , _gleLimit         :: !(Maybe Nat)
  , _gleLogGroupName  :: !Text
  , _gleLogStreamName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLogEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gleStartTime' - The start of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp earlier than this time are not included.
--
-- * 'gleStartFromHead' - If the value is true, the earliest log events are returned first. If the value is false, the latest log events are returned first. The default value is false.
--
-- * 'gleNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'gleEndTime' - The end of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp later than this time are not included.
--
-- * 'gleLimit' - The maximum number of log events returned. If you don't specify a value, the maximum is as many log events as can fit in a response size of 1 MB, up to 10,000 log events.
--
-- * 'gleLogGroupName' - The name of the log group.
--
-- * 'gleLogStreamName' - The name of the log stream.
getLogEvents
    :: Text -- ^ 'gleLogGroupName'
    -> Text -- ^ 'gleLogStreamName'
    -> GetLogEvents
getLogEvents pLogGroupName_ pLogStreamName_ =
  GetLogEvents'
    { _gleStartTime = Nothing
    , _gleStartFromHead = Nothing
    , _gleNextToken = Nothing
    , _gleEndTime = Nothing
    , _gleLimit = Nothing
    , _gleLogGroupName = pLogGroupName_
    , _gleLogStreamName = pLogStreamName_
    }


-- | The start of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp earlier than this time are not included.
gleStartTime :: Lens' GetLogEvents (Maybe Natural)
gleStartTime = lens _gleStartTime (\ s a -> s{_gleStartTime = a}) . mapping _Nat

-- | If the value is true, the earliest log events are returned first. If the value is false, the latest log events are returned first. The default value is false.
gleStartFromHead :: Lens' GetLogEvents (Maybe Bool)
gleStartFromHead = lens _gleStartFromHead (\ s a -> s{_gleStartFromHead = a})

-- | The token for the next set of items to return. (You received this token from a previous call.)
gleNextToken :: Lens' GetLogEvents (Maybe Text)
gleNextToken = lens _gleNextToken (\ s a -> s{_gleNextToken = a})

-- | The end of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp later than this time are not included.
gleEndTime :: Lens' GetLogEvents (Maybe Natural)
gleEndTime = lens _gleEndTime (\ s a -> s{_gleEndTime = a}) . mapping _Nat

-- | The maximum number of log events returned. If you don't specify a value, the maximum is as many log events as can fit in a response size of 1 MB, up to 10,000 log events.
gleLimit :: Lens' GetLogEvents (Maybe Natural)
gleLimit = lens _gleLimit (\ s a -> s{_gleLimit = a}) . mapping _Nat

-- | The name of the log group.
gleLogGroupName :: Lens' GetLogEvents Text
gleLogGroupName = lens _gleLogGroupName (\ s a -> s{_gleLogGroupName = a})

-- | The name of the log stream.
gleLogStreamName :: Lens' GetLogEvents Text
gleLogStreamName = lens _gleLogStreamName (\ s a -> s{_gleLogStreamName = a})

instance AWSRequest GetLogEvents where
        type Rs GetLogEvents = GetLogEventsResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 GetLogEventsResponse' <$>
                   (x .?> "nextBackwardToken") <*>
                     (x .?> "nextForwardToken")
                     <*> (x .?> "events" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetLogEvents where

instance NFData GetLogEvents where

instance ToHeaders GetLogEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.GetLogEvents" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetLogEvents where
        toJSON GetLogEvents'{..}
          = object
              (catMaybes
                 [("startTime" .=) <$> _gleStartTime,
                  ("startFromHead" .=) <$> _gleStartFromHead,
                  ("nextToken" .=) <$> _gleNextToken,
                  ("endTime" .=) <$> _gleEndTime,
                  ("limit" .=) <$> _gleLimit,
                  Just ("logGroupName" .= _gleLogGroupName),
                  Just ("logStreamName" .= _gleLogStreamName)])

instance ToPath GetLogEvents where
        toPath = const "/"

instance ToQuery GetLogEvents where
        toQuery = const mempty

-- | /See:/ 'getLogEventsResponse' smart constructor.
data GetLogEventsResponse = GetLogEventsResponse'
  { _glersNextBackwardToken :: !(Maybe Text)
  , _glersNextForwardToken  :: !(Maybe Text)
  , _glersEvents            :: !(Maybe [OutputLogEvent])
  , _glersResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLogEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glersNextBackwardToken' - The token for the next set of items in the backward direction. The token expires after 24 hours.
--
-- * 'glersNextForwardToken' - The token for the next set of items in the forward direction. The token expires after 24 hours.
--
-- * 'glersEvents' - The events.
--
-- * 'glersResponseStatus' - -- | The response status code.
getLogEventsResponse
    :: Int -- ^ 'glersResponseStatus'
    -> GetLogEventsResponse
getLogEventsResponse pResponseStatus_ =
  GetLogEventsResponse'
    { _glersNextBackwardToken = Nothing
    , _glersNextForwardToken = Nothing
    , _glersEvents = Nothing
    , _glersResponseStatus = pResponseStatus_
    }


-- | The token for the next set of items in the backward direction. The token expires after 24 hours.
glersNextBackwardToken :: Lens' GetLogEventsResponse (Maybe Text)
glersNextBackwardToken = lens _glersNextBackwardToken (\ s a -> s{_glersNextBackwardToken = a})

-- | The token for the next set of items in the forward direction. The token expires after 24 hours.
glersNextForwardToken :: Lens' GetLogEventsResponse (Maybe Text)
glersNextForwardToken = lens _glersNextForwardToken (\ s a -> s{_glersNextForwardToken = a})

-- | The events.
glersEvents :: Lens' GetLogEventsResponse [OutputLogEvent]
glersEvents = lens _glersEvents (\ s a -> s{_glersEvents = a}) . _Default . _Coerce

-- | -- | The response status code.
glersResponseStatus :: Lens' GetLogEventsResponse Int
glersResponseStatus = lens _glersResponseStatus (\ s a -> s{_glersResponseStatus = a})

instance NFData GetLogEventsResponse where
