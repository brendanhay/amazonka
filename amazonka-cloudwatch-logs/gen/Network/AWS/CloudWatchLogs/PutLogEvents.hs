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
-- Module      : Network.AWS.CloudWatchLogs.PutLogEvents
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a batch of log events to the specified log stream.
--
-- Every PutLogEvents request must include the 'sequenceToken' obtained
-- from the response of the previous request. An upload in a newly created
-- log stream does not require a 'sequenceToken'.
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
--     their 'timestamp'.
-- -   The maximum number of log events in a batch is 10,000.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html AWS API Reference> for PutLogEvents.
module Network.AWS.CloudWatchLogs.PutLogEvents
    (
    -- * Creating a Request
      putLogEvents
    , PutLogEvents
    -- * Request Lenses
    , pleSequenceToken
    , pleLogGroupName
    , pleLogStreamName
    , pleLogEvents

    -- * Destructuring the Response
    , putLogEventsResponse
    , PutLogEventsResponse
    -- * Response Lenses
    , plersRejectedLogEventsInfo
    , plersNextSequenceToken
    , plersStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putLogEvents' smart constructor.
data PutLogEvents = PutLogEvents'
    { _pleSequenceToken :: !(Maybe Text)
    , _pleLogGroupName  :: !Text
    , _pleLogStreamName :: !Text
    , _pleLogEvents     :: !(List1 InputLogEvent)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutLogEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pleSequenceToken'
--
-- * 'pleLogGroupName'
--
-- * 'pleLogStreamName'
--
-- * 'pleLogEvents'
putLogEvents
    :: Text -- ^ 'pleLogGroupName'
    -> Text -- ^ 'pleLogStreamName'
    -> NonEmpty InputLogEvent -- ^ 'pleLogEvents'
    -> PutLogEvents
putLogEvents pLogGroupName_ pLogStreamName_ pLogEvents_ =
    PutLogEvents'
    { _pleSequenceToken = Nothing
    , _pleLogGroupName = pLogGroupName_
    , _pleLogStreamName = pLogStreamName_
    , _pleLogEvents = _List1 # pLogEvents_
    }

-- | A string token that must be obtained from the response of the previous
-- 'PutLogEvents' request.
pleSequenceToken :: Lens' PutLogEvents (Maybe Text)
pleSequenceToken = lens _pleSequenceToken (\ s a -> s{_pleSequenceToken = a});

-- | The name of the log group to put log events to.
pleLogGroupName :: Lens' PutLogEvents Text
pleLogGroupName = lens _pleLogGroupName (\ s a -> s{_pleLogGroupName = a});

-- | The name of the log stream to put log events to.
pleLogStreamName :: Lens' PutLogEvents Text
pleLogStreamName = lens _pleLogStreamName (\ s a -> s{_pleLogStreamName = a});

-- | Undocumented member.
pleLogEvents :: Lens' PutLogEvents (NonEmpty InputLogEvent)
pleLogEvents = lens _pleLogEvents (\ s a -> s{_pleLogEvents = a}) . _List1;

instance AWSRequest PutLogEvents where
        type Rs PutLogEvents = PutLogEventsResponse
        request = postJSON cloudWatchLogs
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
              (catMaybes
                 [("sequenceToken" .=) <$> _pleSequenceToken,
                  Just ("logGroupName" .= _pleLogGroupName),
                  Just ("logStreamName" .= _pleLogStreamName),
                  Just ("logEvents" .= _pleLogEvents)])

instance ToPath PutLogEvents where
        toPath = const "/"

instance ToQuery PutLogEvents where
        toQuery = const mempty

-- | /See:/ 'putLogEventsResponse' smart constructor.
data PutLogEventsResponse = PutLogEventsResponse'
    { _plersRejectedLogEventsInfo :: !(Maybe RejectedLogEventsInfo)
    , _plersNextSequenceToken     :: !(Maybe Text)
    , _plersStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutLogEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plersRejectedLogEventsInfo'
--
-- * 'plersNextSequenceToken'
--
-- * 'plersStatus'
putLogEventsResponse
    :: Int -- ^ 'plersStatus'
    -> PutLogEventsResponse
putLogEventsResponse pStatus_ =
    PutLogEventsResponse'
    { _plersRejectedLogEventsInfo = Nothing
    , _plersNextSequenceToken = Nothing
    , _plersStatus = pStatus_
    }

-- | Undocumented member.
plersRejectedLogEventsInfo :: Lens' PutLogEventsResponse (Maybe RejectedLogEventsInfo)
plersRejectedLogEventsInfo = lens _plersRejectedLogEventsInfo (\ s a -> s{_plersRejectedLogEventsInfo = a});

-- | Undocumented member.
plersNextSequenceToken :: Lens' PutLogEventsResponse (Maybe Text)
plersNextSequenceToken = lens _plersNextSequenceToken (\ s a -> s{_plersNextSequenceToken = a});

-- | The response status code.
plersStatus :: Lens' PutLogEventsResponse Int
plersStatus = lens _plersStatus (\ s a -> s{_plersStatus = a});
