{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CloudWatchLogs.PutLogEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Uploads a batch of log events to the specified log stream. Every
-- PutLogEvents request must include the sequenceToken obtained from the
-- response of the previous request. An upload in a newly created log stream
-- does not require a sequenceToken. The batch of events must satisfy the
-- following constraints: The maximum batch size is 32,768 bytes, and this
-- size is calculated as the sum of all event messages in UTF-8, plus 26 bytes
-- for each log event. None of the log events in the batch can be more than 2
-- hours in the future. None of the log events in the batch can be older than
-- 14 days or the retention period of the log group. The log events in the
-- batch must be in chronological ordered by their timestamp. The maximum
-- number of log events in a batch is 1,000.
module Network.AWS.CloudWatchLogs.PutLogEvents
    (
    -- * Request
      PutLogEvents
    -- ** Request constructor
    , putLogEvents
    -- ** Request lenses
    , pleLogEvents
    , pleLogGroupName
    , pleLogStreamName
    , pleSequenceToken

    -- * Response
    , PutLogEventsResponse
    -- ** Response constructor
    , putLogEventsResponse
    -- ** Response lenses
    , plerNextSequenceToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudWatchLogs.Types

data PutLogEvents = PutLogEvents
    { _pleLogEvents     :: List1 InputLogEvent
    , _pleLogGroupName  :: Text
    , _pleLogStreamName :: Text
    , _pleSequenceToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'PutLogEvents' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pleLogEvents' @::@ 'NonEmpty' 'InputLogEvent'
--
-- * 'pleLogGroupName' @::@ 'Text'
--
-- * 'pleLogStreamName' @::@ 'Text'
--
-- * 'pleSequenceToken' @::@ 'Maybe' 'Text'
--
putLogEvents :: Text -- ^ 'pleLogGroupName'
             -> Text -- ^ 'pleLogStreamName'
             -> NonEmpty InputLogEvent -- ^ 'pleLogEvents'
             -> PutLogEvents
putLogEvents p1 p2 p3 = PutLogEvents
    { _pleLogGroupName  = p1
    , _pleLogStreamName = p2
    , _pleLogEvents     = withIso _List1 (const id) p3
    , _pleSequenceToken = Nothing
    }

pleLogEvents :: Lens' PutLogEvents (NonEmpty InputLogEvent)
pleLogEvents = lens _pleLogEvents (\s a -> s { _pleLogEvents = a })
    . _List1

pleLogGroupName :: Lens' PutLogEvents Text
pleLogGroupName = lens _pleLogGroupName (\s a -> s { _pleLogGroupName = a })

pleLogStreamName :: Lens' PutLogEvents Text
pleLogStreamName = lens _pleLogStreamName (\s a -> s { _pleLogStreamName = a })

-- | A string token that must be obtained from the response of the previous
-- PutLogEvents request.
pleSequenceToken :: Lens' PutLogEvents (Maybe Text)
pleSequenceToken = lens _pleSequenceToken (\s a -> s { _pleSequenceToken = a })

instance ToPath PutLogEvents where
    toPath = const "/"

instance ToQuery PutLogEvents where
    toQuery = const mempty

instance ToHeaders PutLogEvents

instance ToBody PutLogEvents where
    toBody = toBody . encode . _pleLogGroupName

newtype PutLogEventsResponse = PutLogEventsResponse
    { _plerNextSequenceToken :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'PutLogEventsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'plerNextSequenceToken' @::@ 'Maybe' 'Text'
--
putLogEventsResponse :: PutLogEventsResponse
putLogEventsResponse = PutLogEventsResponse
    { _plerNextSequenceToken = Nothing
    }

plerNextSequenceToken :: Lens' PutLogEventsResponse (Maybe Text)
plerNextSequenceToken =
    lens _plerNextSequenceToken (\s a -> s { _plerNextSequenceToken = a })

instance AWSRequest PutLogEvents where
    type Sv PutLogEvents = CloudWatchLogs
    type Rs PutLogEvents = PutLogEventsResponse

    request  = post
    response = jsonResponse $ \h o -> PutLogEventsResponse
        <$> o .: "nextSequenceToken"
