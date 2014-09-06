{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.DescribeAlarmHistory
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves history for the specified alarm. Filter alarms by date range or
-- item type. If an alarm name is not specified, Amazon CloudWatch returns
-- histories for all of the owner's alarms. Amazon CloudWatch retains the
-- history of an alarm for two weeks, whether or not you delete the alarm.
module Network.AWS.CloudWatch.V2010_08_01.DescribeAlarmHistory
    (
    -- * Request
      DescribeAlarmHistory
    -- ** Request constructor
    , mkDescribeAlarmHistory
    -- ** Request lenses
    , dahAlarmName
    , dahHistoryItemType
    , dahStartDate
    , dahEndDate
    , dahMaxRecords
    , dahNextToken

    -- * Response
    , DescribeAlarmHistoryResponse
    -- ** Response lenses
    , dahrsAlarmHistoryItems
    , dahrsNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | 
data DescribeAlarmHistory = DescribeAlarmHistory
    { _dahAlarmName :: Maybe Text
    , _dahHistoryItemType :: Maybe HistoryItemType
    , _dahStartDate :: Maybe ISO8601
    , _dahEndDate :: Maybe ISO8601
    , _dahMaxRecords :: Maybe Integer
    , _dahNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAlarmHistory' request.
mkDescribeAlarmHistory :: DescribeAlarmHistory
mkDescribeAlarmHistory = DescribeAlarmHistory
    { _dahAlarmName = Nothing
    , _dahHistoryItemType = Nothing
    , _dahStartDate = Nothing
    , _dahEndDate = Nothing
    , _dahMaxRecords = Nothing
    , _dahNextToken = Nothing
    }
{-# INLINE mkDescribeAlarmHistory #-}

-- | The name of the alarm.
dahAlarmName :: Lens' DescribeAlarmHistory (Maybe Text)
dahAlarmName = lens _dahAlarmName (\s a -> s { _dahAlarmName = a })
{-# INLINE dahAlarmName #-}

-- | The type of alarm histories to retrieve.
dahHistoryItemType :: Lens' DescribeAlarmHistory (Maybe HistoryItemType)
dahHistoryItemType =
    lens _dahHistoryItemType (\s a -> s { _dahHistoryItemType = a })
{-# INLINE dahHistoryItemType #-}

-- | The starting date to retrieve alarm history.
dahStartDate :: Lens' DescribeAlarmHistory (Maybe ISO8601)
dahStartDate = lens _dahStartDate (\s a -> s { _dahStartDate = a })
{-# INLINE dahStartDate #-}

-- | The ending date to retrieve alarm history.
dahEndDate :: Lens' DescribeAlarmHistory (Maybe ISO8601)
dahEndDate = lens _dahEndDate (\s a -> s { _dahEndDate = a })
{-# INLINE dahEndDate #-}

-- | The maximum number of alarm history records to retrieve.
dahMaxRecords :: Lens' DescribeAlarmHistory (Maybe Integer)
dahMaxRecords = lens _dahMaxRecords (\s a -> s { _dahMaxRecords = a })
{-# INLINE dahMaxRecords #-}

-- | The token returned by a previous call to indicate that there is more data
-- available.
dahNextToken :: Lens' DescribeAlarmHistory (Maybe Text)
dahNextToken = lens _dahNextToken (\s a -> s { _dahNextToken = a })
{-# INLINE dahNextToken #-}

instance ToQuery DescribeAlarmHistory where
    toQuery = genericQuery def

-- | The output for the DescribeAlarmHistory action.
data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse
    { _dahrsAlarmHistoryItems :: [AlarmHistoryItem]
    , _dahrsNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | A list of alarm histories in JSON format.
dahrsAlarmHistoryItems :: Lens' DescribeAlarmHistoryResponse [AlarmHistoryItem]
dahrsAlarmHistoryItems =
    lens _dahrsAlarmHistoryItems (\s a -> s { _dahrsAlarmHistoryItems = a })
{-# INLINE dahrsAlarmHistoryItems #-}

-- | A string that marks the start of the next batch of returned results.
dahrsNextToken :: Lens' DescribeAlarmHistoryResponse (Maybe Text)
dahrsNextToken = lens _dahrsNextToken (\s a -> s { _dahrsNextToken = a })
{-# INLINE dahrsNextToken #-}

instance FromXML DescribeAlarmHistoryResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAlarmHistory where
    type Sv DescribeAlarmHistory = CloudWatch
    type Rs DescribeAlarmHistory = DescribeAlarmHistoryResponse

    request = post "DescribeAlarmHistory"
    response _ = xmlResponse

instance AWSPager DescribeAlarmHistory where
    next rq rs = (\x -> rq { _dahNextToken = Just x })
        <$> (_dahrsNextToken rs)
