{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotFleetRequestHistory
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the events for the specified Spot fleet request during the
-- specified time.
--
-- Spot fleet events are delayed by up to 30 seconds before they can be
-- described. This ensures that you can query by the last evaluated time
-- and not miss a recorded event.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotFleetRequestHistory.html>
module Network.AWS.EC2.DescribeSpotFleetRequestHistory
    (
    -- * Request
      DescribeSpotFleetRequestHistory
    -- ** Request constructor
    , describeSpotFleetRequestHistory
    -- ** Request lenses
    , dsfrhrqNextToken
    , dsfrhrqEventType
    , dsfrhrqDryRun
    , dsfrhrqMaxResults
    , dsfrhrqSpotFleetRequestId
    , dsfrhrqStartTime

    -- * Response
    , DescribeSpotFleetRequestHistoryResponse
    -- ** Response constructor
    , describeSpotFleetRequestHistoryResponse
    -- ** Response lenses
    , dsfrhrsNextToken
    , dsfrhrsStatus
    , dsfrhrsSpotFleetRequestId
    , dsfrhrsStartTime
    , dsfrhrsLastEvaluatedTime
    , dsfrhrsHistoryRecords
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribeSpotFleetRequestHistory.
--
-- /See:/ 'describeSpotFleetRequestHistory' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfrhrqNextToken'
--
-- * 'dsfrhrqEventType'
--
-- * 'dsfrhrqDryRun'
--
-- * 'dsfrhrqMaxResults'
--
-- * 'dsfrhrqSpotFleetRequestId'
--
-- * 'dsfrhrqStartTime'
data DescribeSpotFleetRequestHistory = DescribeSpotFleetRequestHistory'
    { _dsfrhrqNextToken          :: !(Maybe Text)
    , _dsfrhrqEventType          :: !(Maybe EventType)
    , _dsfrhrqDryRun             :: !(Maybe Bool)
    , _dsfrhrqMaxResults         :: !(Maybe Int)
    , _dsfrhrqSpotFleetRequestId :: !Text
    , _dsfrhrqStartTime          :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSpotFleetRequestHistory' smart constructor.
describeSpotFleetRequestHistory :: Text -> UTCTime -> DescribeSpotFleetRequestHistory
describeSpotFleetRequestHistory pSpotFleetRequestId pStartTime =
    DescribeSpotFleetRequestHistory'
    { _dsfrhrqNextToken = Nothing
    , _dsfrhrqEventType = Nothing
    , _dsfrhrqDryRun = Nothing
    , _dsfrhrqMaxResults = Nothing
    , _dsfrhrqSpotFleetRequestId = pSpotFleetRequestId
    , _dsfrhrqStartTime = _Time # pStartTime
    }

-- | The token for the next set of results.
dsfrhrqNextToken :: Lens' DescribeSpotFleetRequestHistory (Maybe Text)
dsfrhrqNextToken = lens _dsfrhrqNextToken (\ s a -> s{_dsfrhrqNextToken = a});

-- | The type of events to describe. By default, all events are described.
dsfrhrqEventType :: Lens' DescribeSpotFleetRequestHistory (Maybe EventType)
dsfrhrqEventType = lens _dsfrhrqEventType (\ s a -> s{_dsfrhrqEventType = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsfrhrqDryRun :: Lens' DescribeSpotFleetRequestHistory (Maybe Bool)
dsfrhrqDryRun = lens _dsfrhrqDryRun (\ s a -> s{_dsfrhrqDryRun = a});

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
dsfrhrqMaxResults :: Lens' DescribeSpotFleetRequestHistory (Maybe Int)
dsfrhrqMaxResults = lens _dsfrhrqMaxResults (\ s a -> s{_dsfrhrqMaxResults = a});

-- | The ID of the Spot fleet request.
dsfrhrqSpotFleetRequestId :: Lens' DescribeSpotFleetRequestHistory Text
dsfrhrqSpotFleetRequestId = lens _dsfrhrqSpotFleetRequestId (\ s a -> s{_dsfrhrqSpotFleetRequestId = a});

-- | The starting date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
dsfrhrqStartTime :: Lens' DescribeSpotFleetRequestHistory UTCTime
dsfrhrqStartTime = lens _dsfrhrqStartTime (\ s a -> s{_dsfrhrqStartTime = a}) . _Time;

instance AWSRequest DescribeSpotFleetRequestHistory
         where
        type Sv DescribeSpotFleetRequestHistory = EC2
        type Rs DescribeSpotFleetRequestHistory =
             DescribeSpotFleetRequestHistoryResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotFleetRequestHistoryResponse' <$>
                   (x .@? "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .@ "spotFleetRequestId")
                     <*> (x .@ "startTime")
                     <*> (x .@ "lastEvaluatedTime")
                     <*>
                     (x .@? "historyRecordSet" .!@ mempty >>=
                        parseXMLList "item"))

instance ToHeaders DescribeSpotFleetRequestHistory
         where
        toHeaders = const mempty

instance ToPath DescribeSpotFleetRequestHistory where
        toPath = const "/"

instance ToQuery DescribeSpotFleetRequestHistory
         where
        toQuery DescribeSpotFleetRequestHistory'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSpotFleetRequestHistory" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "NextToken" =: _dsfrhrqNextToken,
               "EventType" =: _dsfrhrqEventType,
               "DryRun" =: _dsfrhrqDryRun,
               "MaxResults" =: _dsfrhrqMaxResults,
               "SpotFleetRequestId" =: _dsfrhrqSpotFleetRequestId,
               "StartTime" =: _dsfrhrqStartTime]

-- | Contains the output of DescribeSpotFleetRequestHistory.
--
-- /See:/ 'describeSpotFleetRequestHistoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfrhrsNextToken'
--
-- * 'dsfrhrsStatus'
--
-- * 'dsfrhrsSpotFleetRequestId'
--
-- * 'dsfrhrsStartTime'
--
-- * 'dsfrhrsLastEvaluatedTime'
--
-- * 'dsfrhrsHistoryRecords'
data DescribeSpotFleetRequestHistoryResponse = DescribeSpotFleetRequestHistoryResponse'
    { _dsfrhrsNextToken          :: !(Maybe Text)
    , _dsfrhrsStatus             :: !Int
    , _dsfrhrsSpotFleetRequestId :: !Text
    , _dsfrhrsStartTime          :: !ISO8601
    , _dsfrhrsLastEvaluatedTime  :: !ISO8601
    , _dsfrhrsHistoryRecords     :: ![HistoryRecord]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSpotFleetRequestHistoryResponse' smart constructor.
describeSpotFleetRequestHistoryResponse :: Int -> Text -> UTCTime -> UTCTime -> DescribeSpotFleetRequestHistoryResponse
describeSpotFleetRequestHistoryResponse pStatus pSpotFleetRequestId pStartTime pLastEvaluatedTime =
    DescribeSpotFleetRequestHistoryResponse'
    { _dsfrhrsNextToken = Nothing
    , _dsfrhrsStatus = pStatus
    , _dsfrhrsSpotFleetRequestId = pSpotFleetRequestId
    , _dsfrhrsStartTime = _Time # pStartTime
    , _dsfrhrsLastEvaluatedTime = _Time # pLastEvaluatedTime
    , _dsfrhrsHistoryRecords = mempty
    }

-- | The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
dsfrhrsNextToken :: Lens' DescribeSpotFleetRequestHistoryResponse (Maybe Text)
dsfrhrsNextToken = lens _dsfrhrsNextToken (\ s a -> s{_dsfrhrsNextToken = a});

-- | FIXME: Undocumented member.
dsfrhrsStatus :: Lens' DescribeSpotFleetRequestHistoryResponse Int
dsfrhrsStatus = lens _dsfrhrsStatus (\ s a -> s{_dsfrhrsStatus = a});

-- | The ID of the Spot fleet request.
dsfrhrsSpotFleetRequestId :: Lens' DescribeSpotFleetRequestHistoryResponse Text
dsfrhrsSpotFleetRequestId = lens _dsfrhrsSpotFleetRequestId (\ s a -> s{_dsfrhrsSpotFleetRequestId = a});

-- | The starting date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
dsfrhrsStartTime :: Lens' DescribeSpotFleetRequestHistoryResponse UTCTime
dsfrhrsStartTime = lens _dsfrhrsStartTime (\ s a -> s{_dsfrhrsStartTime = a}) . _Time;

-- | The last date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). All records up to this time were
-- retrieved.
--
-- If @nextToken@ indicates that there are more results, this value is not
-- present.
dsfrhrsLastEvaluatedTime :: Lens' DescribeSpotFleetRequestHistoryResponse UTCTime
dsfrhrsLastEvaluatedTime = lens _dsfrhrsLastEvaluatedTime (\ s a -> s{_dsfrhrsLastEvaluatedTime = a}) . _Time;

-- | Information about the events in the history of the Spot fleet request.
dsfrhrsHistoryRecords :: Lens' DescribeSpotFleetRequestHistoryResponse [HistoryRecord]
dsfrhrsHistoryRecords = lens _dsfrhrsHistoryRecords (\ s a -> s{_dsfrhrsHistoryRecords = a});
