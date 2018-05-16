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
-- Module      : Network.AWS.EC2.DescribeFleetHistory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the events for the specified EC2 Fleet during the specified time.
--
--
module Network.AWS.EC2.DescribeFleetHistory
    (
    -- * Creating a Request
      describeFleetHistory
    , DescribeFleetHistory
    -- * Request Lenses
    , dfhNextToken
    , dfhEventType
    , dfhDryRun
    , dfhMaxResults
    , dfhFleetId
    , dfhStartTime

    -- * Destructuring the Response
    , describeFleetHistoryResponse
    , DescribeFleetHistoryResponse
    -- * Response Lenses
    , dfhrsStartTime
    , dfhrsLastEvaluatedTime
    , dfhrsNextToken
    , dfhrsHistoryRecords
    , dfhrsFleetId
    , dfhrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFleetHistory' smart constructor.
data DescribeFleetHistory = DescribeFleetHistory'
  { _dfhNextToken  :: !(Maybe Text)
  , _dfhEventType  :: !(Maybe FleetEventType)
  , _dfhDryRun     :: !(Maybe Bool)
  , _dfhMaxResults :: !(Maybe Int)
  , _dfhFleetId    :: !Text
  , _dfhStartTime  :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFleetHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfhNextToken' - The token for the next set of results.
--
-- * 'dfhEventType' - The type of events to describe. By default, all events are described.
--
-- * 'dfhDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dfhMaxResults' - The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- * 'dfhFleetId' - The ID of the EC2 Fleet.
--
-- * 'dfhStartTime' - The start date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
describeFleetHistory
    :: Text -- ^ 'dfhFleetId'
    -> UTCTime -- ^ 'dfhStartTime'
    -> DescribeFleetHistory
describeFleetHistory pFleetId_ pStartTime_ =
  DescribeFleetHistory'
    { _dfhNextToken = Nothing
    , _dfhEventType = Nothing
    , _dfhDryRun = Nothing
    , _dfhMaxResults = Nothing
    , _dfhFleetId = pFleetId_
    , _dfhStartTime = _Time # pStartTime_
    }


-- | The token for the next set of results.
dfhNextToken :: Lens' DescribeFleetHistory (Maybe Text)
dfhNextToken = lens _dfhNextToken (\ s a -> s{_dfhNextToken = a})

-- | The type of events to describe. By default, all events are described.
dfhEventType :: Lens' DescribeFleetHistory (Maybe FleetEventType)
dfhEventType = lens _dfhEventType (\ s a -> s{_dfhEventType = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dfhDryRun :: Lens' DescribeFleetHistory (Maybe Bool)
dfhDryRun = lens _dfhDryRun (\ s a -> s{_dfhDryRun = a})

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
dfhMaxResults :: Lens' DescribeFleetHistory (Maybe Int)
dfhMaxResults = lens _dfhMaxResults (\ s a -> s{_dfhMaxResults = a})

-- | The ID of the EC2 Fleet.
dfhFleetId :: Lens' DescribeFleetHistory Text
dfhFleetId = lens _dfhFleetId (\ s a -> s{_dfhFleetId = a})

-- | The start date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
dfhStartTime :: Lens' DescribeFleetHistory UTCTime
dfhStartTime = lens _dfhStartTime (\ s a -> s{_dfhStartTime = a}) . _Time

instance AWSRequest DescribeFleetHistory where
        type Rs DescribeFleetHistory =
             DescribeFleetHistoryResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeFleetHistoryResponse' <$>
                   (x .@? "startTime") <*> (x .@? "lastEvaluatedTime")
                     <*> (x .@? "nextToken")
                     <*>
                     (x .@? "historyRecordSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (x .@? "fleetId")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeFleetHistory where

instance NFData DescribeFleetHistory where

instance ToHeaders DescribeFleetHistory where
        toHeaders = const mempty

instance ToPath DescribeFleetHistory where
        toPath = const "/"

instance ToQuery DescribeFleetHistory where
        toQuery DescribeFleetHistory'{..}
          = mconcat
              ["Action" =: ("DescribeFleetHistory" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "NextToken" =: _dfhNextToken,
               "EventType" =: _dfhEventType, "DryRun" =: _dfhDryRun,
               "MaxResults" =: _dfhMaxResults,
               "FleetId" =: _dfhFleetId,
               "StartTime" =: _dfhStartTime]

-- | /See:/ 'describeFleetHistoryResponse' smart constructor.
data DescribeFleetHistoryResponse = DescribeFleetHistoryResponse'
  { _dfhrsStartTime         :: !(Maybe ISO8601)
  , _dfhrsLastEvaluatedTime :: !(Maybe ISO8601)
  , _dfhrsNextToken         :: !(Maybe Text)
  , _dfhrsHistoryRecords    :: !(Maybe [HistoryRecordEntry])
  , _dfhrsFleetId           :: !(Maybe Text)
  , _dfhrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFleetHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfhrsStartTime' - The start date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- * 'dfhrsLastEvaluatedTime' - The last date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). All records up to this time were retrieved. If @nextToken@ indicates that there are more results, this value is not present.
--
-- * 'dfhrsNextToken' - The token for the next set of results.
--
-- * 'dfhrsHistoryRecords' - Information about the events in the history of the EC2 Fleet.
--
-- * 'dfhrsFleetId' - The ID of the EC Fleet.
--
-- * 'dfhrsResponseStatus' - -- | The response status code.
describeFleetHistoryResponse
    :: Int -- ^ 'dfhrsResponseStatus'
    -> DescribeFleetHistoryResponse
describeFleetHistoryResponse pResponseStatus_ =
  DescribeFleetHistoryResponse'
    { _dfhrsStartTime = Nothing
    , _dfhrsLastEvaluatedTime = Nothing
    , _dfhrsNextToken = Nothing
    , _dfhrsHistoryRecords = Nothing
    , _dfhrsFleetId = Nothing
    , _dfhrsResponseStatus = pResponseStatus_
    }


-- | The start date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
dfhrsStartTime :: Lens' DescribeFleetHistoryResponse (Maybe UTCTime)
dfhrsStartTime = lens _dfhrsStartTime (\ s a -> s{_dfhrsStartTime = a}) . mapping _Time

-- | The last date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). All records up to this time were retrieved. If @nextToken@ indicates that there are more results, this value is not present.
dfhrsLastEvaluatedTime :: Lens' DescribeFleetHistoryResponse (Maybe UTCTime)
dfhrsLastEvaluatedTime = lens _dfhrsLastEvaluatedTime (\ s a -> s{_dfhrsLastEvaluatedTime = a}) . mapping _Time

-- | The token for the next set of results.
dfhrsNextToken :: Lens' DescribeFleetHistoryResponse (Maybe Text)
dfhrsNextToken = lens _dfhrsNextToken (\ s a -> s{_dfhrsNextToken = a})

-- | Information about the events in the history of the EC2 Fleet.
dfhrsHistoryRecords :: Lens' DescribeFleetHistoryResponse [HistoryRecordEntry]
dfhrsHistoryRecords = lens _dfhrsHistoryRecords (\ s a -> s{_dfhrsHistoryRecords = a}) . _Default . _Coerce

-- | The ID of the EC Fleet.
dfhrsFleetId :: Lens' DescribeFleetHistoryResponse (Maybe Text)
dfhrsFleetId = lens _dfhrsFleetId (\ s a -> s{_dfhrsFleetId = a})

-- | -- | The response status code.
dfhrsResponseStatus :: Lens' DescribeFleetHistoryResponse Int
dfhrsResponseStatus = lens _dfhrsResponseStatus (\ s a -> s{_dfhrsResponseStatus = a})

instance NFData DescribeFleetHistoryResponse where
