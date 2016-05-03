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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves log events from the specified log stream. You can provide an
-- optional time range to filter the results on the event 'timestamp'.
--
-- By default, this operation returns as much log events as can fit in a
-- response size of 1MB, up to 10,000 log events. The response will always
-- include a 'nextForwardToken' and a 'nextBackwardToken' in the response
-- body. You can use any of these tokens in subsequent 'GetLogEvents'
-- requests to paginate through events in either forward or backward
-- direction. You can also limit the number of log events returned in the
-- response by specifying the 'limit' parameter in the request.
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

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getLogEvents' smart constructor.
data GetLogEvents = GetLogEvents'
    { _gleStartTime     :: !(Maybe Nat)
    , _gleStartFromHead :: !(Maybe Bool)
    , _gleNextToken     :: !(Maybe Text)
    , _gleEndTime       :: !(Maybe Nat)
    , _gleLimit         :: !(Maybe Nat)
    , _gleLogGroupName  :: !Text
    , _gleLogStreamName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetLogEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gleStartTime'
--
-- * 'gleStartFromHead'
--
-- * 'gleNextToken'
--
-- * 'gleEndTime'
--
-- * 'gleLimit'
--
-- * 'gleLogGroupName'
--
-- * 'gleLogStreamName'
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

-- | Undocumented member.
gleStartTime :: Lens' GetLogEvents (Maybe Natural)
gleStartTime = lens _gleStartTime (\ s a -> s{_gleStartTime = a}) . mapping _Nat;

-- | If set to true, the earliest log events would be returned first. The
-- default is false (the latest log events are returned first).
gleStartFromHead :: Lens' GetLogEvents (Maybe Bool)
gleStartFromHead = lens _gleStartFromHead (\ s a -> s{_gleStartFromHead = a});

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the 'nextForwardToken' or
-- 'nextBackwardToken' fields in the response of the previous
-- 'GetLogEvents' request.
gleNextToken :: Lens' GetLogEvents (Maybe Text)
gleNextToken = lens _gleNextToken (\ s a -> s{_gleNextToken = a});

-- | Undocumented member.
gleEndTime :: Lens' GetLogEvents (Maybe Natural)
gleEndTime = lens _gleEndTime (\ s a -> s{_gleEndTime = a}) . mapping _Nat;

-- | The maximum number of log events returned in the response. If you don\'t
-- specify a value, the request would return as many log events as can fit
-- in a response size of 1MB, up to 10,000 log events.
gleLimit :: Lens' GetLogEvents (Maybe Natural)
gleLimit = lens _gleLimit (\ s a -> s{_gleLimit = a}) . mapping _Nat;

-- | The name of the log group to query.
gleLogGroupName :: Lens' GetLogEvents Text
gleLogGroupName = lens _gleLogGroupName (\ s a -> s{_gleLogGroupName = a});

-- | The name of the log stream to query.
gleLogStreamName :: Lens' GetLogEvents Text
gleLogStreamName = lens _gleLogStreamName (\ s a -> s{_gleLogStreamName = a});

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

instance Hashable GetLogEvents

instance NFData GetLogEvents

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetLogEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glersNextBackwardToken'
--
-- * 'glersNextForwardToken'
--
-- * 'glersEvents'
--
-- * 'glersResponseStatus'
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

-- | Undocumented member.
glersNextBackwardToken :: Lens' GetLogEventsResponse (Maybe Text)
glersNextBackwardToken = lens _glersNextBackwardToken (\ s a -> s{_glersNextBackwardToken = a});

-- | Undocumented member.
glersNextForwardToken :: Lens' GetLogEventsResponse (Maybe Text)
glersNextForwardToken = lens _glersNextForwardToken (\ s a -> s{_glersNextForwardToken = a});

-- | Undocumented member.
glersEvents :: Lens' GetLogEventsResponse [OutputLogEvent]
glersEvents = lens _glersEvents (\ s a -> s{_glersEvents = a}) . _Default . _Coerce;

-- | The response status code.
glersResponseStatus :: Lens' GetLogEventsResponse Int
glersResponseStatus = lens _glersResponseStatus (\ s a -> s{_glersResponseStatus = a});

instance NFData GetLogEventsResponse
