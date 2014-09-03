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
dahiAlarmName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeAlarmHistory
    -> f DescribeAlarmHistory
dahiAlarmName f x =
    (\y -> x { _dahiAlarmName = y })
       <$> f (_dahiAlarmName x)
{-# INLINE dahiAlarmName #-}

-- | The type of alarm histories to retrieve.
dahiHistoryItemType
    :: Functor f
    => (Maybe HistoryItemType
    -> f (Maybe HistoryItemType))
    -> DescribeAlarmHistory
    -> f DescribeAlarmHistory
dahiHistoryItemType f x =
    (\y -> x { _dahiHistoryItemType = y })
       <$> f (_dahiHistoryItemType x)
{-# INLINE dahiHistoryItemType #-}

-- | The maximum number of alarm history records to retrieve.
dahiMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeAlarmHistory
    -> f DescribeAlarmHistory
dahiMaxRecords f x =
    (\y -> x { _dahiMaxRecords = y })
       <$> f (_dahiMaxRecords x)
{-# INLINE dahiMaxRecords #-}

-- | The token returned by a previous call to indicate that there is more data
-- available.
dahiNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeAlarmHistory
    -> f DescribeAlarmHistory
dahiNextToken f x =
    (\y -> x { _dahiNextToken = y })
       <$> f (_dahiNextToken x)
{-# INLINE dahiNextToken #-}

-- | The starting date to retrieve alarm history.
dahiStartDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DescribeAlarmHistory
    -> f DescribeAlarmHistory
dahiStartDate f x =
    (\y -> x { _dahiStartDate = y })
       <$> f (_dahiStartDate x)
{-# INLINE dahiStartDate #-}

-- | The ending date to retrieve alarm history.
dahiEndDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DescribeAlarmHistory
    -> f DescribeAlarmHistory
dahiEndDate f x =
    (\y -> x { _dahiEndDate = y })
       <$> f (_dahiEndDate x)
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
dahoAlarmHistoryItems
    :: Functor f
    => ([AlarmHistoryItem]
    -> f ([AlarmHistoryItem]))
    -> DescribeAlarmHistoryResponse
    -> f DescribeAlarmHistoryResponse
dahoAlarmHistoryItems f x =
    (\y -> x { _dahoAlarmHistoryItems = y })
       <$> f (_dahoAlarmHistoryItems x)
{-# INLINE dahoAlarmHistoryItems #-}

-- | A string that marks the start of the next batch of returned results.
dahoNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeAlarmHistoryResponse
    -> f DescribeAlarmHistoryResponse
dahoNextToken f x =
    (\y -> x { _dahoNextToken = y })
       <$> f (_dahoNextToken x)
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
