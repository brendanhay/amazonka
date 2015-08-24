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
-- Module      : Network.AWS.EC2.DescribeSpotFleetRequestHistory
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the events for the specified Spot fleet request during the
-- specified time.
--
-- Spot fleet events are delayed by up to 30 seconds before they can be
-- described. This ensures that you can query by the last evaluated time
-- and not miss a recorded event.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotFleetRequestHistory.html AWS API Reference> for DescribeSpotFleetRequestHistory.
module Network.AWS.EC2.DescribeSpotFleetRequestHistory
    (
    -- * Creating a Request
      describeSpotFleetRequestHistory
    , DescribeSpotFleetRequestHistory
    -- * Request Lenses
    , dsfrhNextToken
    , dsfrhEventType
    , dsfrhDryRun
    , dsfrhMaxResults
    , dsfrhSpotFleetRequestId
    , dsfrhStartTime

    -- * Destructuring the Response
    , describeSpotFleetRequestHistoryResponse
    , DescribeSpotFleetRequestHistoryResponse
    -- * Response Lenses
    , dsfrhrsNextToken
    , dsfrhrsStatus
    , dsfrhrsSpotFleetRequestId
    , dsfrhrsStartTime
    , dsfrhrsLastEvaluatedTime
    , dsfrhrsHistoryRecords
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribeSpotFleetRequestHistory.
--
-- /See:/ 'describeSpotFleetRequestHistory' smart constructor.
data DescribeSpotFleetRequestHistory = DescribeSpotFleetRequestHistory'
    { _dsfrhNextToken          :: !(Maybe Text)
    , _dsfrhEventType          :: !(Maybe EventType)
    , _dsfrhDryRun             :: !(Maybe Bool)
    , _dsfrhMaxResults         :: !(Maybe Int)
    , _dsfrhSpotFleetRequestId :: !Text
    , _dsfrhStartTime          :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeSpotFleetRequestHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfrhNextToken'
--
-- * 'dsfrhEventType'
--
-- * 'dsfrhDryRun'
--
-- * 'dsfrhMaxResults'
--
-- * 'dsfrhSpotFleetRequestId'
--
-- * 'dsfrhStartTime'
describeSpotFleetRequestHistory
    :: Text -- ^ 'dsfrhSpotFleetRequestId'
    -> UTCTime -- ^ 'dsfrhStartTime'
    -> DescribeSpotFleetRequestHistory
describeSpotFleetRequestHistory pSpotFleetRequestId_ pStartTime_ =
    DescribeSpotFleetRequestHistory'
    { _dsfrhNextToken = Nothing
    , _dsfrhEventType = Nothing
    , _dsfrhDryRun = Nothing
    , _dsfrhMaxResults = Nothing
    , _dsfrhSpotFleetRequestId = pSpotFleetRequestId_
    , _dsfrhStartTime = _Time # pStartTime_
    }

-- | The token for the next set of results.
dsfrhNextToken :: Lens' DescribeSpotFleetRequestHistory (Maybe Text)
dsfrhNextToken = lens _dsfrhNextToken (\ s a -> s{_dsfrhNextToken = a});

-- | The type of events to describe. By default, all events are described.
dsfrhEventType :: Lens' DescribeSpotFleetRequestHistory (Maybe EventType)
dsfrhEventType = lens _dsfrhEventType (\ s a -> s{_dsfrhEventType = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dsfrhDryRun :: Lens' DescribeSpotFleetRequestHistory (Maybe Bool)
dsfrhDryRun = lens _dsfrhDryRun (\ s a -> s{_dsfrhDryRun = a});

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned 'NextToken'
-- value.
dsfrhMaxResults :: Lens' DescribeSpotFleetRequestHistory (Maybe Int)
dsfrhMaxResults = lens _dsfrhMaxResults (\ s a -> s{_dsfrhMaxResults = a});

-- | The ID of the Spot fleet request.
dsfrhSpotFleetRequestId :: Lens' DescribeSpotFleetRequestHistory Text
dsfrhSpotFleetRequestId = lens _dsfrhSpotFleetRequestId (\ s a -> s{_dsfrhSpotFleetRequestId = a});

-- | The starting date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
dsfrhStartTime :: Lens' DescribeSpotFleetRequestHistory UTCTime
dsfrhStartTime = lens _dsfrhStartTime (\ s a -> s{_dsfrhStartTime = a}) . _Time;

instance AWSRequest DescribeSpotFleetRequestHistory
         where
        type Rs DescribeSpotFleetRequestHistory =
             DescribeSpotFleetRequestHistoryResponse
        request = postQuery eC2
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
               "NextToken" =: _dsfrhNextToken,
               "EventType" =: _dsfrhEventType,
               "DryRun" =: _dsfrhDryRun,
               "MaxResults" =: _dsfrhMaxResults,
               "SpotFleetRequestId" =: _dsfrhSpotFleetRequestId,
               "StartTime" =: _dsfrhStartTime]

-- | Contains the output of DescribeSpotFleetRequestHistory.
--
-- /See:/ 'describeSpotFleetRequestHistoryResponse' smart constructor.
data DescribeSpotFleetRequestHistoryResponse = DescribeSpotFleetRequestHistoryResponse'
    { _dsfrhrsNextToken          :: !(Maybe Text)
    , _dsfrhrsStatus             :: !Int
    , _dsfrhrsSpotFleetRequestId :: !Text
    , _dsfrhrsStartTime          :: !ISO8601
    , _dsfrhrsLastEvaluatedTime  :: !ISO8601
    , _dsfrhrsHistoryRecords     :: ![HistoryRecord]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeSpotFleetRequestHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
describeSpotFleetRequestHistoryResponse
    :: Int -- ^ 'dsfrhrsStatus'
    -> Text -- ^ 'dsfrhrsSpotFleetRequestId'
    -> UTCTime -- ^ 'dsfrhrsStartTime'
    -> UTCTime -- ^ 'dsfrhrsLastEvaluatedTime'
    -> DescribeSpotFleetRequestHistoryResponse
describeSpotFleetRequestHistoryResponse pStatus_ pSpotFleetRequestId_ pStartTime_ pLastEvaluatedTime_ =
    DescribeSpotFleetRequestHistoryResponse'
    { _dsfrhrsNextToken = Nothing
    , _dsfrhrsStatus = pStatus_
    , _dsfrhrsSpotFleetRequestId = pSpotFleetRequestId_
    , _dsfrhrsStartTime = _Time # pStartTime_
    , _dsfrhrsLastEvaluatedTime = _Time # pLastEvaluatedTime_
    , _dsfrhrsHistoryRecords = mempty
    }

-- | The token required to retrieve the next set of results. This value is
-- 'null' when there are no more results to return.
dsfrhrsNextToken :: Lens' DescribeSpotFleetRequestHistoryResponse (Maybe Text)
dsfrhrsNextToken = lens _dsfrhrsNextToken (\ s a -> s{_dsfrhrsNextToken = a});

-- | The response status code.
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
-- If 'nextToken' indicates that there are more results, this value is not
-- present.
dsfrhrsLastEvaluatedTime :: Lens' DescribeSpotFleetRequestHistoryResponse UTCTime
dsfrhrsLastEvaluatedTime = lens _dsfrhrsLastEvaluatedTime (\ s a -> s{_dsfrhrsLastEvaluatedTime = a}) . _Time;

-- | Information about the events in the history of the Spot fleet request.
dsfrhrsHistoryRecords :: Lens' DescribeSpotFleetRequestHistoryResponse [HistoryRecord]
dsfrhrsHistoryRecords = lens _dsfrhrsHistoryRecords (\ s a -> s{_dsfrhrsHistoryRecords = a}) . _Coerce;
