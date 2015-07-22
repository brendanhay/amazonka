{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.GetLogEvents
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves log events from the specified log stream. You can provide an
-- optional time range to filter the results on the event @timestamp@.
--
-- By default, this operation returns as much log events as can fit in a
-- response size of 1MB, up to 10,000 log events. The response will always
-- include a @nextForwardToken@ and a @nextBackwardToken@ in the response
-- body. You can use any of these tokens in subsequent @GetLogEvents@
-- requests to paginate through events in either forward or backward
-- direction. You can also limit the number of log events returned in the
-- response by specifying the @limit@ parameter in the request.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_GetLogEvents.html>
module Network.AWS.CloudWatchLogs.GetLogEvents
    (
    -- * Request
      GetLogEvents
    -- ** Request constructor
    , getLogEvents
    -- ** Request lenses
    , glerqStartTime
    , glerqStartFromHead
    , glerqNextToken
    , glerqEndTime
    , glerqLimit
    , glerqLogGroupName
    , glerqLogStreamName

    -- * Response
    , GetLogEventsResponse
    -- ** Response constructor
    , getLogEventsResponse
    -- ** Response lenses
    , glersNextBackwardToken
    , glersNextForwardToken
    , glersEvents
    , glersStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getLogEvents' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'glerqStartTime'
--
-- * 'glerqStartFromHead'
--
-- * 'glerqNextToken'
--
-- * 'glerqEndTime'
--
-- * 'glerqLimit'
--
-- * 'glerqLogGroupName'
--
-- * 'glerqLogStreamName'
data GetLogEvents = GetLogEvents'
    { _glerqStartTime     :: !(Maybe Nat)
    , _glerqStartFromHead :: !(Maybe Bool)
    , _glerqNextToken     :: !(Maybe Text)
    , _glerqEndTime       :: !(Maybe Nat)
    , _glerqLimit         :: !(Maybe Nat)
    , _glerqLogGroupName  :: !Text
    , _glerqLogStreamName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetLogEvents' smart constructor.
getLogEvents :: Text -> Text -> GetLogEvents
getLogEvents pLogGroupName_ pLogStreamName_ =
    GetLogEvents'
    { _glerqStartTime = Nothing
    , _glerqStartFromHead = Nothing
    , _glerqNextToken = Nothing
    , _glerqEndTime = Nothing
    , _glerqLimit = Nothing
    , _glerqLogGroupName = pLogGroupName_
    , _glerqLogStreamName = pLogStreamName_
    }

-- | FIXME: Undocumented member.
glerqStartTime :: Lens' GetLogEvents (Maybe Natural)
glerqStartTime = lens _glerqStartTime (\ s a -> s{_glerqStartTime = a}) . mapping _Nat;

-- | If set to true, the earliest log events would be returned first. The
-- default is false (the latest log events are returned first).
glerqStartFromHead :: Lens' GetLogEvents (Maybe Bool)
glerqStartFromHead = lens _glerqStartFromHead (\ s a -> s{_glerqStartFromHead = a});

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the @nextForwardToken@ or
-- @nextBackwardToken@ fields in the response of the previous
-- @GetLogEvents@ request.
glerqNextToken :: Lens' GetLogEvents (Maybe Text)
glerqNextToken = lens _glerqNextToken (\ s a -> s{_glerqNextToken = a});

-- | FIXME: Undocumented member.
glerqEndTime :: Lens' GetLogEvents (Maybe Natural)
glerqEndTime = lens _glerqEndTime (\ s a -> s{_glerqEndTime = a}) . mapping _Nat;

-- | The maximum number of log events returned in the response. If you don\'t
-- specify a value, the request would return as many log events as can fit
-- in a response size of 1MB, up to 10,000 log events.
glerqLimit :: Lens' GetLogEvents (Maybe Natural)
glerqLimit = lens _glerqLimit (\ s a -> s{_glerqLimit = a}) . mapping _Nat;

-- | The name of the log group to query.
glerqLogGroupName :: Lens' GetLogEvents Text
glerqLogGroupName = lens _glerqLogGroupName (\ s a -> s{_glerqLogGroupName = a});

-- | The name of the log stream to query.
glerqLogStreamName :: Lens' GetLogEvents Text
glerqLogStreamName = lens _glerqLogStreamName (\ s a -> s{_glerqLogStreamName = a});

instance AWSRequest GetLogEvents where
        type Sv GetLogEvents = CloudWatchLogs
        type Rs GetLogEvents = GetLogEventsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetLogEventsResponse' <$>
                   (x .?> "nextBackwardToken") <*>
                     (x .?> "nextForwardToken")
                     <*> (x .?> "events" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
              ["startTime" .= _glerqStartTime,
               "startFromHead" .= _glerqStartFromHead,
               "nextToken" .= _glerqNextToken,
               "endTime" .= _glerqEndTime, "limit" .= _glerqLimit,
               "logGroupName" .= _glerqLogGroupName,
               "logStreamName" .= _glerqLogStreamName]

instance ToPath GetLogEvents where
        toPath = const "/"

instance ToQuery GetLogEvents where
        toQuery = const mempty

-- | /See:/ 'getLogEventsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'glersNextBackwardToken'
--
-- * 'glersNextForwardToken'
--
-- * 'glersEvents'
--
-- * 'glersStatus'
data GetLogEventsResponse = GetLogEventsResponse'
    { _glersNextBackwardToken :: !(Maybe Text)
    , _glersNextForwardToken  :: !(Maybe Text)
    , _glersEvents            :: !(Maybe [OutputLogEvent])
    , _glersStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetLogEventsResponse' smart constructor.
getLogEventsResponse :: Int -> GetLogEventsResponse
getLogEventsResponse pStatus_ =
    GetLogEventsResponse'
    { _glersNextBackwardToken = Nothing
    , _glersNextForwardToken = Nothing
    , _glersEvents = Nothing
    , _glersStatus = pStatus_
    }

-- | FIXME: Undocumented member.
glersNextBackwardToken :: Lens' GetLogEventsResponse (Maybe Text)
glersNextBackwardToken = lens _glersNextBackwardToken (\ s a -> s{_glersNextBackwardToken = a});

-- | FIXME: Undocumented member.
glersNextForwardToken :: Lens' GetLogEventsResponse (Maybe Text)
glersNextForwardToken = lens _glersNextForwardToken (\ s a -> s{_glersNextForwardToken = a});

-- | FIXME: Undocumented member.
glersEvents :: Lens' GetLogEventsResponse [OutputLogEvent]
glersEvents = lens _glersEvents (\ s a -> s{_glersEvents = a}) . _Default;

-- | FIXME: Undocumented member.
glersStatus :: Lens' GetLogEventsResponse Int
glersStatus = lens _glersStatus (\ s a -> s{_glersStatus = a});
