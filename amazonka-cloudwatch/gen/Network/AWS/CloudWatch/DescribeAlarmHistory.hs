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
-- Module      : Network.AWS.CloudWatch.DescribeAlarmHistory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the history for the specified alarm. You can filter the results by date range or item type. If an alarm name is not specified, the histories for all alarms are returned.
--
--
-- CloudWatch retains the history of an alarm even if you delete the alarm.
--
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.DescribeAlarmHistory
    (
    -- * Creating a Request
      describeAlarmHistory
    , DescribeAlarmHistory
    -- * Request Lenses
    , dahAlarmName
    , dahHistoryItemType
    , dahEndDate
    , dahStartDate
    , dahNextToken
    , dahMaxRecords

    -- * Destructuring the Response
    , describeAlarmHistoryResponse
    , DescribeAlarmHistoryResponse
    -- * Response Lenses
    , dahrsAlarmHistoryItems
    , dahrsNextToken
    , dahrsResponseStatus
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAlarmHistory' smart constructor.
data DescribeAlarmHistory = DescribeAlarmHistory'
  { _dahAlarmName       :: !(Maybe Text)
  , _dahHistoryItemType :: !(Maybe HistoryItemType)
  , _dahEndDate         :: !(Maybe ISO8601)
  , _dahStartDate       :: !(Maybe ISO8601)
  , _dahNextToken       :: !(Maybe Text)
  , _dahMaxRecords      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAlarmHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dahAlarmName' - The name of the alarm.
--
-- * 'dahHistoryItemType' - The type of alarm histories to retrieve.
--
-- * 'dahEndDate' - The ending date to retrieve alarm history.
--
-- * 'dahStartDate' - The starting date to retrieve alarm history.
--
-- * 'dahNextToken' - The token returned by a previous call to indicate that there is more data available.
--
-- * 'dahMaxRecords' - The maximum number of alarm history records to retrieve.
describeAlarmHistory
    :: DescribeAlarmHistory
describeAlarmHistory =
  DescribeAlarmHistory'
    { _dahAlarmName = Nothing
    , _dahHistoryItemType = Nothing
    , _dahEndDate = Nothing
    , _dahStartDate = Nothing
    , _dahNextToken = Nothing
    , _dahMaxRecords = Nothing
    }


-- | The name of the alarm.
dahAlarmName :: Lens' DescribeAlarmHistory (Maybe Text)
dahAlarmName = lens _dahAlarmName (\ s a -> s{_dahAlarmName = a})

-- | The type of alarm histories to retrieve.
dahHistoryItemType :: Lens' DescribeAlarmHistory (Maybe HistoryItemType)
dahHistoryItemType = lens _dahHistoryItemType (\ s a -> s{_dahHistoryItemType = a})

-- | The ending date to retrieve alarm history.
dahEndDate :: Lens' DescribeAlarmHistory (Maybe UTCTime)
dahEndDate = lens _dahEndDate (\ s a -> s{_dahEndDate = a}) . mapping _Time

-- | The starting date to retrieve alarm history.
dahStartDate :: Lens' DescribeAlarmHistory (Maybe UTCTime)
dahStartDate = lens _dahStartDate (\ s a -> s{_dahStartDate = a}) . mapping _Time

-- | The token returned by a previous call to indicate that there is more data available.
dahNextToken :: Lens' DescribeAlarmHistory (Maybe Text)
dahNextToken = lens _dahNextToken (\ s a -> s{_dahNextToken = a})

-- | The maximum number of alarm history records to retrieve.
dahMaxRecords :: Lens' DescribeAlarmHistory (Maybe Natural)
dahMaxRecords = lens _dahMaxRecords (\ s a -> s{_dahMaxRecords = a}) . mapping _Nat

instance AWSPager DescribeAlarmHistory where
        page rq rs
          | stop (rs ^. dahrsNextToken) = Nothing
          | stop (rs ^. dahrsAlarmHistoryItems) = Nothing
          | otherwise =
            Just $ rq & dahNextToken .~ rs ^. dahrsNextToken

instance AWSRequest DescribeAlarmHistory where
        type Rs DescribeAlarmHistory =
             DescribeAlarmHistoryResponse
        request = postQuery cloudWatch
        response
          = receiveXMLWrapper "DescribeAlarmHistoryResult"
              (\ s h x ->
                 DescribeAlarmHistoryResponse' <$>
                   (x .@? "AlarmHistoryItems" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAlarmHistory where

instance NFData DescribeAlarmHistory where

instance ToHeaders DescribeAlarmHistory where
        toHeaders = const mempty

instance ToPath DescribeAlarmHistory where
        toPath = const "/"

instance ToQuery DescribeAlarmHistory where
        toQuery DescribeAlarmHistory'{..}
          = mconcat
              ["Action" =: ("DescribeAlarmHistory" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "AlarmName" =: _dahAlarmName,
               "HistoryItemType" =: _dahHistoryItemType,
               "EndDate" =: _dahEndDate,
               "StartDate" =: _dahStartDate,
               "NextToken" =: _dahNextToken,
               "MaxRecords" =: _dahMaxRecords]

-- | /See:/ 'describeAlarmHistoryResponse' smart constructor.
data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse'
  { _dahrsAlarmHistoryItems :: !(Maybe [AlarmHistoryItem])
  , _dahrsNextToken         :: !(Maybe Text)
  , _dahrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAlarmHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dahrsAlarmHistoryItems' - The alarm histories, in JSON format.
--
-- * 'dahrsNextToken' - The token that marks the start of the next batch of returned results.
--
-- * 'dahrsResponseStatus' - -- | The response status code.
describeAlarmHistoryResponse
    :: Int -- ^ 'dahrsResponseStatus'
    -> DescribeAlarmHistoryResponse
describeAlarmHistoryResponse pResponseStatus_ =
  DescribeAlarmHistoryResponse'
    { _dahrsAlarmHistoryItems = Nothing
    , _dahrsNextToken = Nothing
    , _dahrsResponseStatus = pResponseStatus_
    }


-- | The alarm histories, in JSON format.
dahrsAlarmHistoryItems :: Lens' DescribeAlarmHistoryResponse [AlarmHistoryItem]
dahrsAlarmHistoryItems = lens _dahrsAlarmHistoryItems (\ s a -> s{_dahrsAlarmHistoryItems = a}) . _Default . _Coerce

-- | The token that marks the start of the next batch of returned results.
dahrsNextToken :: Lens' DescribeAlarmHistoryResponse (Maybe Text)
dahrsNextToken = lens _dahrsNextToken (\ s a -> s{_dahrsNextToken = a})

-- | -- | The response status code.
dahrsResponseStatus :: Lens' DescribeAlarmHistoryResponse Int
dahrsResponseStatus = lens _dahrsResponseStatus (\ s a -> s{_dahrsResponseStatus = a})

instance NFData DescribeAlarmHistoryResponse where
