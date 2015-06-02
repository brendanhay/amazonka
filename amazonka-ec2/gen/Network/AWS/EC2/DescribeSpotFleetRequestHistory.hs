{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSpotFleetRequestHistory
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the events for the specified Spot fleet request during the
-- specified time.
--
-- Spot fleet events are delayed by up to 30 seconds before they can be
-- described. This ensures that you can query by the last evaluated time and not
-- miss a recorded event.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotFleetRequestHistory.html>
module Network.AWS.EC2.DescribeSpotFleetRequestHistory
    (
    -- * Request
      DescribeSpotFleetRequestHistory
    -- ** Request constructor
    , describeSpotFleetRequestHistory
    -- ** Request lenses
    , dsfrhDryRun
    , dsfrhEventType
    , dsfrhMaxResults
    , dsfrhNextToken
    , dsfrhSpotFleetRequestId
    , dsfrhStartTime

    -- * Response
    , DescribeSpotFleetRequestHistoryResponse
    -- ** Response constructor
    , describeSpotFleetRequestHistoryResponse
    -- ** Response lenses
    , dsfrhrHistoryRecords
    , dsfrhrLastEvaluatedTime
    , dsfrhrNextToken
    , dsfrhrSpotFleetRequestId
    , dsfrhrStartTime
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeSpotFleetRequestHistory = DescribeSpotFleetRequestHistory
    { _dsfrhDryRun             :: Maybe Bool
    , _dsfrhEventType          :: Maybe EventType
    , _dsfrhMaxResults         :: Maybe Int
    , _dsfrhNextToken          :: Maybe Text
    , _dsfrhSpotFleetRequestId :: Text
    , _dsfrhStartTime          :: ISO8601
    } deriving (Eq, Read, Show)

-- | 'DescribeSpotFleetRequestHistory' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfrhDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dsfrhEventType' @::@ 'Maybe' 'EventType'
--
-- * 'dsfrhMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dsfrhNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsfrhSpotFleetRequestId' @::@ 'Text'
--
-- * 'dsfrhStartTime' @::@ 'UTCTime'
--
describeSpotFleetRequestHistory :: Text -- ^ 'dsfrhSpotFleetRequestId'
                                -> UTCTime -- ^ 'dsfrhStartTime'
                                -> DescribeSpotFleetRequestHistory
describeSpotFleetRequestHistory p1 p2 = DescribeSpotFleetRequestHistory
    { _dsfrhSpotFleetRequestId = p1
    , _dsfrhStartTime          = withIso _Time (const id) p2
    , _dsfrhDryRun             = Nothing
    , _dsfrhEventType          = Nothing
    , _dsfrhNextToken          = Nothing
    , _dsfrhMaxResults         = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dsfrhDryRun :: Lens' DescribeSpotFleetRequestHistory (Maybe Bool)
dsfrhDryRun = lens _dsfrhDryRun (\s a -> s { _dsfrhDryRun = a })

-- | The type of events to describe. By default, all events are described.
dsfrhEventType :: Lens' DescribeSpotFleetRequestHistory (Maybe EventType)
dsfrhEventType = lens _dsfrhEventType (\s a -> s { _dsfrhEventType = a })

-- | The maximum number of results to return in a single call. Specify a value
-- between 1 and 1000. The default value is 1000. To retrieve the remaining
-- results, make another call with the returned 'NextToken' value.
dsfrhMaxResults :: Lens' DescribeSpotFleetRequestHistory (Maybe Int)
dsfrhMaxResults = lens _dsfrhMaxResults (\s a -> s { _dsfrhMaxResults = a })

-- | The token for the next set of results.
dsfrhNextToken :: Lens' DescribeSpotFleetRequestHistory (Maybe Text)
dsfrhNextToken = lens _dsfrhNextToken (\s a -> s { _dsfrhNextToken = a })

-- | The ID of the Spot fleet request.
dsfrhSpotFleetRequestId :: Lens' DescribeSpotFleetRequestHistory Text
dsfrhSpotFleetRequestId =
    lens _dsfrhSpotFleetRequestId (\s a -> s { _dsfrhSpotFleetRequestId = a })

-- | The starting date and time for the events, in UTC format (for example, /YYYY/-/MM/
-- -/DD/T/HH/:/MM/:/SS/Z).
dsfrhStartTime :: Lens' DescribeSpotFleetRequestHistory UTCTime
dsfrhStartTime = lens _dsfrhStartTime (\s a -> s { _dsfrhStartTime = a }) . _Time

data DescribeSpotFleetRequestHistoryResponse = DescribeSpotFleetRequestHistoryResponse
    { _dsfrhrHistoryRecords     :: List "item" HistoryRecord
    , _dsfrhrLastEvaluatedTime  :: ISO8601
    , _dsfrhrNextToken          :: Maybe Text
    , _dsfrhrSpotFleetRequestId :: Text
    , _dsfrhrStartTime          :: ISO8601
    } deriving (Eq, Read, Show)

-- | 'DescribeSpotFleetRequestHistoryResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfrhrHistoryRecords' @::@ ['HistoryRecord']
--
-- * 'dsfrhrLastEvaluatedTime' @::@ 'UTCTime'
--
-- * 'dsfrhrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsfrhrSpotFleetRequestId' @::@ 'Text'
--
-- * 'dsfrhrStartTime' @::@ 'UTCTime'
--
describeSpotFleetRequestHistoryResponse :: Text -- ^ 'dsfrhrSpotFleetRequestId'
                                        -> UTCTime -- ^ 'dsfrhrStartTime'
                                        -> UTCTime -- ^ 'dsfrhrLastEvaluatedTime'
                                        -> DescribeSpotFleetRequestHistoryResponse
describeSpotFleetRequestHistoryResponse p1 p2 p3 = DescribeSpotFleetRequestHistoryResponse
    { _dsfrhrSpotFleetRequestId = p1
    , _dsfrhrStartTime          = withIso _Time (const id) p2
    , _dsfrhrLastEvaluatedTime  = withIso _Time (const id) p3
    , _dsfrhrHistoryRecords     = mempty
    , _dsfrhrNextToken          = Nothing
    }

-- | Information about the events in the history of the Spot fleet request.
dsfrhrHistoryRecords :: Lens' DescribeSpotFleetRequestHistoryResponse [HistoryRecord]
dsfrhrHistoryRecords =
    lens _dsfrhrHistoryRecords (\s a -> s { _dsfrhrHistoryRecords = a })
        . _List

-- | The last date and time for the events, in UTC format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). All records up to this time were retrieved.
--
-- If 'nextToken' indicates that there are more results, this value is not
-- present.
dsfrhrLastEvaluatedTime :: Lens' DescribeSpotFleetRequestHistoryResponse UTCTime
dsfrhrLastEvaluatedTime =
    lens _dsfrhrLastEvaluatedTime (\s a -> s { _dsfrhrLastEvaluatedTime = a })
        . _Time

-- | The token required to retrieve the next set of results. This value is 'null'
-- when there are no more results to return.
dsfrhrNextToken :: Lens' DescribeSpotFleetRequestHistoryResponse (Maybe Text)
dsfrhrNextToken = lens _dsfrhrNextToken (\s a -> s { _dsfrhrNextToken = a })

-- | The ID of the Spot fleet request.
dsfrhrSpotFleetRequestId :: Lens' DescribeSpotFleetRequestHistoryResponse Text
dsfrhrSpotFleetRequestId =
    lens _dsfrhrSpotFleetRequestId
        (\s a -> s { _dsfrhrSpotFleetRequestId = a })

-- | The starting date and time for the events, in UTC format (for example, /YYYY/-/MM/
-- -/DD/T/HH/:/MM/:/SS/Z).
dsfrhrStartTime :: Lens' DescribeSpotFleetRequestHistoryResponse UTCTime
dsfrhrStartTime = lens _dsfrhrStartTime (\s a -> s { _dsfrhrStartTime = a }) . _Time

instance ToPath DescribeSpotFleetRequestHistory where
    toPath = const "/"

instance ToQuery DescribeSpotFleetRequestHistory where
    toQuery DescribeSpotFleetRequestHistory{..} = mconcat
        [ "DryRun"             =? _dsfrhDryRun
        , "EventType"          =? _dsfrhEventType
        , "MaxResults"         =? _dsfrhMaxResults
        , "NextToken"          =? _dsfrhNextToken
        , "SpotFleetRequestId" =? _dsfrhSpotFleetRequestId
        , "StartTime"          =? _dsfrhStartTime
        ]

instance ToHeaders DescribeSpotFleetRequestHistory

instance AWSRequest DescribeSpotFleetRequestHistory where
    type Sv DescribeSpotFleetRequestHistory = EC2
    type Rs DescribeSpotFleetRequestHistory = DescribeSpotFleetRequestHistoryResponse

    request  = post "DescribeSpotFleetRequestHistory"
    response = xmlResponse

instance FromXML DescribeSpotFleetRequestHistoryResponse where
    parseXML x = DescribeSpotFleetRequestHistoryResponse
        <$> x .@? "historyRecordSet" .!@ mempty
        <*> x .@  "lastEvaluatedTime"
        <*> x .@? "nextToken"
        <*> x .@  "spotFleetRequestId"
        <*> x .@  "startTime"
