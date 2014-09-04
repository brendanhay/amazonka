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
    , mkDescribeAlarmHistoryInput
    -- ** Request lenses
    , dahiAlarmName
    , dahiHistoryItemType
    , dahiStartDate
    , dahiEndDate
    , dahiMaxRecords
    , dahiNextToken

    -- * Response
    , DescribeAlarmHistoryResponse
    -- ** Response lenses
    , dahoAlarmHistoryItems
    , dahoNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAlarmHistory' request.
mkDescribeAlarmHistoryInput :: DescribeAlarmHistory
mkDescribeAlarmHistoryInput = DescribeAlarmHistory
    { _dahiAlarmName = Nothing
    , _dahiHistoryItemType = Nothing
    , _dahiStartDate = Nothing
    , _dahiEndDate = Nothing
    , _dahiMaxRecords = Nothing
    , _dahiNextToken = Nothing
    }
{-# INLINE mkDescribeAlarmHistoryInput #-}

data DescribeAlarmHistory = DescribeAlarmHistory
    { _dahiAlarmName :: Maybe Text
      -- ^ The name of the alarm.
    , _dahiHistoryItemType :: Maybe HistoryItemType
      -- ^ The type of alarm histories to retrieve.
    , _dahiStartDate :: Maybe ISO8601
      -- ^ The starting date to retrieve alarm history.
    , _dahiEndDate :: Maybe ISO8601
      -- ^ The ending date to retrieve alarm history.
    , _dahiMaxRecords :: Maybe Integer
      -- ^ The maximum number of alarm history records to retrieve.
    , _dahiNextToken :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is
      -- more data available.
    } deriving (Show, Generic)

-- | The name of the alarm.
dahiAlarmName :: Lens' DescribeAlarmHistory (Maybe Text)
dahiAlarmName = lens _dahiAlarmName (\s a -> s { _dahiAlarmName = a })
{-# INLINE dahiAlarmName #-}

-- | The type of alarm histories to retrieve.
dahiHistoryItemType :: Lens' DescribeAlarmHistory (Maybe HistoryItemType)
dahiHistoryItemType = lens _dahiHistoryItemType (\s a -> s { _dahiHistoryItemType = a })
{-# INLINE dahiHistoryItemType #-}

-- | The starting date to retrieve alarm history.
dahiStartDate :: Lens' DescribeAlarmHistory (Maybe ISO8601)
dahiStartDate = lens _dahiStartDate (\s a -> s { _dahiStartDate = a })
{-# INLINE dahiStartDate #-}

-- | The ending date to retrieve alarm history.
dahiEndDate :: Lens' DescribeAlarmHistory (Maybe ISO8601)
dahiEndDate = lens _dahiEndDate (\s a -> s { _dahiEndDate = a })
{-# INLINE dahiEndDate #-}

-- | The maximum number of alarm history records to retrieve.
dahiMaxRecords :: Lens' DescribeAlarmHistory (Maybe Integer)
dahiMaxRecords = lens _dahiMaxRecords (\s a -> s { _dahiMaxRecords = a })
{-# INLINE dahiMaxRecords #-}

-- | The token returned by a previous call to indicate that there is more data
-- available.
dahiNextToken :: Lens' DescribeAlarmHistory (Maybe Text)
dahiNextToken = lens _dahiNextToken (\s a -> s { _dahiNextToken = a })
{-# INLINE dahiNextToken #-}

instance ToQuery DescribeAlarmHistory where
    toQuery = genericQuery def

data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse
    { _dahoAlarmHistoryItems :: [AlarmHistoryItem]
      -- ^ A list of alarm histories in JSON format.
    , _dahoNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

-- | A list of alarm histories in JSON format.
dahoAlarmHistoryItems :: Lens' DescribeAlarmHistoryResponse ([AlarmHistoryItem])
dahoAlarmHistoryItems = lens _dahoAlarmHistoryItems (\s a -> s { _dahoAlarmHistoryItems = a })
{-# INLINE dahoAlarmHistoryItems #-}

-- | A string that marks the start of the next batch of returned results.
dahoNextToken :: Lens' DescribeAlarmHistoryResponse (Maybe Text)
dahoNextToken = lens _dahoNextToken (\s a -> s { _dahoNextToken = a })
{-# INLINE dahoNextToken #-}

instance FromXML DescribeAlarmHistoryResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAlarmHistory where
    type Sv DescribeAlarmHistory = CloudWatch
    type Rs DescribeAlarmHistory = DescribeAlarmHistoryResponse

    request = post "DescribeAlarmHistory"
    response _ = xmlResponse

instance AWSPager DescribeAlarmHistory where
    next rq rs = (\x -> rq { _dahiNextToken = Just x })
        <$> (_dahoNextToken rs)
