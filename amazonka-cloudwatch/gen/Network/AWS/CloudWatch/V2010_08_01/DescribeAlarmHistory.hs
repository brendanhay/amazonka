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
    , describeAlarmHistory
    -- ** Request lenses
    , dahiAlarmName
    , dahiHistoryItemType
    , dahiMaxRecords
    , dahiNextToken
    , dahiStartDate
    , dahiEndDate

    -- * Response
    , DescribeAlarmHistoryResponse
    -- ** Response lenses
    , dahoAlarmHistoryItems
    , dahoNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeAlarmHistory' request.
describeAlarmHistory :: DescribeAlarmHistory
describeAlarmHistory = DescribeAlarmHistory
    { _dahiAlarmName = Nothing
    , _dahiHistoryItemType = Nothing
    , _dahiMaxRecords = Nothing
    , _dahiNextToken = Nothing
    , _dahiStartDate = Nothing
    , _dahiEndDate = Nothing
    }
{-# INLINE describeAlarmHistory #-}

data DescribeAlarmHistory = DescribeAlarmHistory
    { _dahiAlarmName :: Maybe Text
      -- ^ The name of the alarm.
    , _dahiHistoryItemType :: Maybe HistoryItemType
      -- ^ The type of alarm histories to retrieve.
    , _dahiMaxRecords :: Maybe Integer
      -- ^ The maximum number of alarm history records to retrieve.
    , _dahiNextToken :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is
      -- more data available.
    , _dahiStartDate :: Maybe ISO8601
      -- ^ The starting date to retrieve alarm history.
    , _dahiEndDate :: Maybe ISO8601
      -- ^ The ending date to retrieve alarm history.
    } deriving (Show, Generic)

-- | The name of the alarm.
dahiAlarmName :: Lens' DescribeAlarmHistory (Maybe Text)
dahiAlarmName f x =
    f (_dahiAlarmName x)
        <&> \y -> x { _dahiAlarmName = y }
{-# INLINE dahiAlarmName #-}

-- | The type of alarm histories to retrieve.
dahiHistoryItemType :: Lens' DescribeAlarmHistory (Maybe HistoryItemType)
dahiHistoryItemType f x =
    f (_dahiHistoryItemType x)
        <&> \y -> x { _dahiHistoryItemType = y }
{-# INLINE dahiHistoryItemType #-}

-- | The maximum number of alarm history records to retrieve.
dahiMaxRecords :: Lens' DescribeAlarmHistory (Maybe Integer)
dahiMaxRecords f x =
    f (_dahiMaxRecords x)
        <&> \y -> x { _dahiMaxRecords = y }
{-# INLINE dahiMaxRecords #-}

-- | The token returned by a previous call to indicate that there is more data
-- available.
dahiNextToken :: Lens' DescribeAlarmHistory (Maybe Text)
dahiNextToken f x =
    f (_dahiNextToken x)
        <&> \y -> x { _dahiNextToken = y }
{-# INLINE dahiNextToken #-}

-- | The starting date to retrieve alarm history.
dahiStartDate :: Lens' DescribeAlarmHistory (Maybe ISO8601)
dahiStartDate f x =
    f (_dahiStartDate x)
        <&> \y -> x { _dahiStartDate = y }
{-# INLINE dahiStartDate #-}

-- | The ending date to retrieve alarm history.
dahiEndDate :: Lens' DescribeAlarmHistory (Maybe ISO8601)
dahiEndDate f x =
    f (_dahiEndDate x)
        <&> \y -> x { _dahiEndDate = y }
{-# INLINE dahiEndDate #-}

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
dahoAlarmHistoryItems f x =
    f (_dahoAlarmHistoryItems x)
        <&> \y -> x { _dahoAlarmHistoryItems = y }
{-# INLINE dahoAlarmHistoryItems #-}

-- | A string that marks the start of the next batch of returned results.
dahoNextToken :: Lens' DescribeAlarmHistoryResponse (Maybe Text)
dahoNextToken f x =
    f (_dahoNextToken x)
        <&> \y -> x { _dahoNextToken = y }
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
