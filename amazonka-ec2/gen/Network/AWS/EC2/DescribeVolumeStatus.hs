{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeVolumeStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the status of the specified volumes. Volume status provides the
-- result of the checks performed on your volumes to determine events that can
-- impair the performance of your volumes. The performance of a volume can be
-- affected if an issue occurs on the volume's underlying host. If the
-- volume's underlying host experiences a power outage or system issue, after
-- the system is restored, there could be data inconsistencies on the volume.
-- Volume events notify you if this occurs. Volume actions notify you if any
-- action needs to be taken in response to the event. The DescribeVolumeStatus
-- operation provides the following information about the specified volumes:
-- Status: Reflects the current status of the volume. The possible values are
-- ok, impaired , warning, or insufficient-data. If all checks pass, the
-- overall status of the volume is ok. If the check fails, the overall status
-- is impaired. If the status is insufficient-data, then the checks may still
-- be taking place on your volume at the time. We recommend that you retry the
-- request. For more information on volume status, see Monitoring the Status
-- of Your Volumes. Events: Reflect the cause of a volume status and may
-- require you to take action. For example, if your volume returns an impaired
-- status, then the volume event might be potential-data-inconsistency. This
-- means that your volume has been affected by an issue with the underlying
-- host, has all I/O operations disabled, and may have inconsistent data.
-- Actions: Reflect the actions you may have to take in response to an event.
-- For example, if the status of the volume is impaired and the volume event
-- shows potential-data-inconsistency, then the action shows enable-volume-io.
-- This means that you may want to enable the I/O operations for the volume by
-- calling the EnableVolumeIO action and then check the volume for data
-- consistency.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumeStatus.html>
module Network.AWS.EC2.DescribeVolumeStatus
    (
    -- * Request
      DescribeVolumeStatus
    -- ** Request constructor
    , describeVolumeStatus
    -- ** Request lenses
    , dvsDryRun
    , dvsFilters
    , dvsMaxResults
    , dvsNextToken
    , dvsVolumeIds

    -- * Response
    , DescribeVolumeStatusResponse
    -- ** Response constructor
    , describeVolumeStatusResponse
    -- ** Response lenses
    , dvsrNextToken
    , dvsrVolumeStatuses
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeVolumeStatus = DescribeVolumeStatus
    { _dvsDryRun     :: Maybe Bool
    , _dvsFilters    :: [Filter]
    , _dvsMaxResults :: Maybe Int
    , _dvsNextToken  :: Maybe Text
    , _dvsVolumeIds  :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeVolumeStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvsDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvsFilters' @::@ ['Filter']
--
-- * 'dvsMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dvsNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dvsVolumeIds' @::@ ['Text']
--
describeVolumeStatus :: DescribeVolumeStatus
describeVolumeStatus = DescribeVolumeStatus
    { _dvsDryRun     = Nothing
    , _dvsVolumeIds  = mempty
    , _dvsFilters    = mempty
    , _dvsNextToken  = Nothing
    , _dvsMaxResults = Nothing
    }

dvsDryRun :: Lens' DescribeVolumeStatus (Maybe Bool)
dvsDryRun = lens _dvsDryRun (\s a -> s { _dvsDryRun = a })

-- | One or more filters. action.code - The action code for the event (for
-- example, enable-volume-io). action.description - A description of the
-- action. action.event-id - The event ID associated with the action.
-- availability-zone - The Availability Zone of the instance.
-- event.description - A description of the event. event.event-id - The
-- event ID. event.event-type - The event type (for io-enabled: passed |
-- failed; for io-performance: io-performance:degraded |
-- io-performance:severely-degraded | io-performance:stalled).
-- event.not-after - The latest end time for the event. event.not-before -
-- The earliest start time for the event. volume-status.details-name - The
-- cause for volume-status.status (io-enabled | io-performance).
-- volume-status.details-status - The status of volume-status.details-name
-- (for io-enabled: passed | failed; for io-performance: normal | degraded |
-- severely-degraded | stalled). volume-status.status - The status of the
-- volume (ok | impaired | warning | insufficient-data).
dvsFilters :: Lens' DescribeVolumeStatus [Filter]
dvsFilters = lens _dvsFilters (\s a -> s { _dvsFilters = a })

-- | The maximum number of paginated volume items per response.
dvsMaxResults :: Lens' DescribeVolumeStatus (Maybe Int)
dvsMaxResults = lens _dvsMaxResults (\s a -> s { _dvsMaxResults = a })

-- | The next paginated set of results to return using the pagination token
-- returned by a previous call.
dvsNextToken :: Lens' DescribeVolumeStatus (Maybe Text)
dvsNextToken = lens _dvsNextToken (\s a -> s { _dvsNextToken = a })

-- | One or more volume IDs. Default: Describes all your volumes.
dvsVolumeIds :: Lens' DescribeVolumeStatus [Text]
dvsVolumeIds = lens _dvsVolumeIds (\s a -> s { _dvsVolumeIds = a })

data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse
    { _dvsrNextToken      :: Maybe Text
    , _dvsrVolumeStatuses :: [VolumeStatusItem]
    } deriving (Eq, Show, Generic)

-- | 'DescribeVolumeStatusResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvsrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dvsrVolumeStatuses' @::@ ['VolumeStatusItem']
--
describeVolumeStatusResponse :: DescribeVolumeStatusResponse
describeVolumeStatusResponse = DescribeVolumeStatusResponse
    { _dvsrVolumeStatuses = mempty
    , _dvsrNextToken      = Nothing
    }

-- | The next paginated set of results to return.
dvsrNextToken :: Lens' DescribeVolumeStatusResponse (Maybe Text)
dvsrNextToken = lens _dvsrNextToken (\s a -> s { _dvsrNextToken = a })

-- | A list of volumes.
dvsrVolumeStatuses :: Lens' DescribeVolumeStatusResponse [VolumeStatusItem]
dvsrVolumeStatuses =
    lens _dvsrVolumeStatuses (\s a -> s { _dvsrVolumeStatuses = a })

instance ToPath DescribeVolumeStatus where
    toPath = const "/"

instance ToQuery DescribeVolumeStatus

instance ToHeaders DescribeVolumeStatus

instance AWSRequest DescribeVolumeStatus where
    type Sv DescribeVolumeStatus = EC2
    type Rs DescribeVolumeStatus = DescribeVolumeStatusResponse

    request  = post "DescribeVolumeStatus"
    response = xmlResponse

instance FromXML DescribeVolumeStatusResponse where
    parseXML x = DescribeVolumeStatusResponse
        <$> x .@? "nextToken"
        <*> x .@ "volumeStatusSet"
