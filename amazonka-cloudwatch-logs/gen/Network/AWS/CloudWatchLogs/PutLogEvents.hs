{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutLogEvents
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Uploads a batch of log events to the specified log stream.
--
-- Every PutLogEvents request must include the @sequenceToken@ obtained
-- from the response of the previous request. An upload in a newly created
-- log stream does not require a @sequenceToken@.
--
-- The batch of events must satisfy the following constraints:
--
-- -   The maximum batch size is 1,048,576 bytes, and this size is
--     calculated as the sum of all event messages in UTF-8, plus 26 bytes
--     for each log event.
-- -   None of the log events in the batch can be more than 2 hours in the
--     future.
-- -   None of the log events in the batch can be older than 14 days or the
--     retention period of the log group.
-- -   The log events in the batch must be in chronological ordered by
--     their @timestamp@.
-- -   The maximum number of log events in a batch is 10,000.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html>
module Network.AWS.CloudWatchLogs.PutLogEvents
    (
    -- * Request
      PutLogEvents
    -- ** Request constructor
    , putLogEvents
    -- ** Request lenses
    , pleSequenceToken
    , pleLogGroupName
    , pleLogStreamName
    , pleLogEvents

    -- * Response
    , PutLogEventsResponse
    -- ** Response constructor
    , putLogEventsResponse
    -- ** Response lenses
    , plersRejectedLogEventsInfo
    , plersNextSequenceToken
    , plersStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putLogEvents' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pleSequenceToken'
--
-- * 'pleLogGroupName'
--
-- * 'pleLogStreamName'
--
-- * 'pleLogEvents'
data PutLogEvents = PutLogEvents'
    { _pleSequenceToken :: !(Maybe Text)
    , _pleLogGroupName  :: !Text
    , _pleLogStreamName :: !Text
    , _pleLogEvents     :: !(List1 InputLogEvent)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutLogEvents' smart constructor.
putLogEvents :: Text -> Text -> NonEmpty InputLogEvent -> PutLogEvents
putLogEvents pLogGroupName_ pLogStreamName_ pLogEvents_ =
    PutLogEvents'
    { _pleSequenceToken = Nothing
    , _pleLogGroupName = pLogGroupName_
    , _pleLogStreamName = pLogStreamName_
    , _pleLogEvents = _List1 # pLogEvents_
    }

-- | A string token that must be obtained from the response of the previous
-- @PutLogEvents@ request.
pleSequenceToken :: Lens' PutLogEvents (Maybe Text)
pleSequenceToken = lens _pleSequenceToken (\ s a -> s{_pleSequenceToken = a});

-- | The name of the log group to put log events to.
pleLogGroupName :: Lens' PutLogEvents Text
pleLogGroupName = lens _pleLogGroupName (\ s a -> s{_pleLogGroupName = a});

-- | The name of the log stream to put log events to.
pleLogStreamName :: Lens' PutLogEvents Text
pleLogStreamName = lens _pleLogStreamName (\ s a -> s{_pleLogStreamName = a});

-- | FIXME: Undocumented member.
pleLogEvents :: Lens' PutLogEvents (NonEmpty InputLogEvent)
pleLogEvents = lens _pleLogEvents (\ s a -> s{_pleLogEvents = a}) . _List1;

instance AWSRequest PutLogEvents where
        type Sv PutLogEvents = CloudWatchLogs
        type Rs PutLogEvents = PutLogEventsResponse
        request = postJSON "PutLogEvents"
        response
          = receiveJSON
              (\ s h x ->
                 PutLogEventsResponse' <$>
                   (x .?> "rejectedLogEventsInfo") <*>
                     (x .?> "nextSequenceToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders PutLogEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.PutLogEvents" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutLogEvents where
        toJSON PutLogEvents'{..}
          = object
              ["sequenceToken" .= _pleSequenceToken,
               "logGroupName" .= _pleLogGroupName,
               "logStreamName" .= _pleLogStreamName,
               "logEvents" .= _pleLogEvents]

instance ToPath PutLogEvents where
        toPath = const "/"

instance ToQuery PutLogEvents where
        toQuery = const mempty

-- | /See:/ 'putLogEventsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'plersRejectedLogEventsInfo'
--
-- * 'plersNextSequenceToken'
--
-- * 'plersStatus'
data PutLogEventsResponse = PutLogEventsResponse'
    { _plersRejectedLogEventsInfo :: !(Maybe RejectedLogEventsInfo)
    , _plersNextSequenceToken     :: !(Maybe Text)
    , _plersStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutLogEventsResponse' smart constructor.
putLogEventsResponse :: Int -> PutLogEventsResponse
putLogEventsResponse pStatus_ =
    PutLogEventsResponse'
    { _plersRejectedLogEventsInfo = Nothing
    , _plersNextSequenceToken = Nothing
    , _plersStatus = pStatus_
    }

-- | FIXME: Undocumented member.
plersRejectedLogEventsInfo :: Lens' PutLogEventsResponse (Maybe RejectedLogEventsInfo)
plersRejectedLogEventsInfo = lens _plersRejectedLogEventsInfo (\ s a -> s{_plersRejectedLogEventsInfo = a});

-- | FIXME: Undocumented member.
plersNextSequenceToken :: Lens' PutLogEventsResponse (Maybe Text)
plersNextSequenceToken = lens _plersNextSequenceToken (\ s a -> s{_plersNextSequenceToken = a});

-- | FIXME: Undocumented member.
plersStatus :: Lens' PutLogEventsResponse Int
plersStatus = lens _plersStatus (\ s a -> s{_plersStatus = a});
