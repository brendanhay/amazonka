{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeVolumeStatus
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
-- consistency. Volume status is based on the volume status checks, and does
-- not reflect the volume state. Therefore, volume status does not indicate
-- volumes in the error state (for example, when a volume is incapable of
-- accepting I/O.) Example This example describes the status of all the
-- volumes associated with your account.
-- https://ec2.amazonaws.com/?Action=DescribeVolumeStatus &amp;AUTHPARAMS
-- 5jkdf074-37ed-4004-8671-a78ee82bf1cbEXAMPLE vol-11111111 us-east-1d ok
-- io-enabled passed vol-22222222 us-east-1d impaired io-enabled failed
-- evol-61a54008 potential-data-inconsistency THIS IS AN EXAMPLE
-- 2011-12-01T14:00:00.000Z 2011-12-01T15:00:00.000Z enable-volume-io
-- evol-61a54008 potential-data-inconsistency THIS IS AN EXAMPLE Example This
-- example describes all the volumes in the us-east-1d Availability Zone with
-- failed io-enabled status.
-- https://ec2.amazonaws.com/?Action=DescribeVolumeStatus
-- &amp;Filter.1.Name=availability-zone &amp;Filter.1.Value.1=us-east-1d
-- &amp;Filter.2.Name=volume-status.details-name
-- &amp;Filter.2.Value.1=io-enabled
-- &amp;Filter.3.Name=volume-status.details-status
-- &amp;Filter.3.Value.1=failed &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeVolumeStatus
    (
    -- * Request
      DescribeVolumeStatus
    -- ** Request constructor
    , describeVolumeStatus
    -- ** Request lenses
    , dvsrFilters
    , dvsrMaxResults
    , dvsrNextToken
    , dvsrVolumeIds

    -- * Response
    , DescribeVolumeStatusResponse
    -- ** Response lenses
    , dvssNextToken
    , dvssVolumeStatuses
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeVolumeStatus' request.
describeVolumeStatus :: DescribeVolumeStatus
describeVolumeStatus = DescribeVolumeStatus
    { _dvsrFilters = mempty
    , _dvsrMaxResults = Nothing
    , _dvsrNextToken = Nothing
    , _dvsrVolumeIds = mempty
    }
{-# INLINE describeVolumeStatus #-}

data DescribeVolumeStatus = DescribeVolumeStatus
    { _dvsrFilters :: [Filter]
      -- ^ One or more filters. action.code - The action code for the event
      -- (for example, enable-volume-io). action.description - A
      -- description of the action. action.event-id - The event ID
      -- associated with the action. availability-zone - The Availability
      -- Zone of the instance. event.description - A description of the
      -- event. event.event-id - The event ID. event.event-type - The
      -- event type (for io-enabled: passed | failed; for io-performance:
      -- io-performance:degraded | io-performance:severely-degraded |
      -- io-performance:stalled). event.not-after - The latest end time
      -- for the event. event.not-before - The earliest start time for the
      -- event. volume-status.details-name - The cause for
      -- volume-status.status (io-enabled | io-performance).
      -- volume-status.details-status - The status of
      -- volume-status.details-name (for io-enabled: passed | failed; for
      -- io-performance: normal | degraded | severely-degraded | stalled).
      -- volume-status.status - The status of the volume (ok | impaired |
      -- warning | insufficient-data).
    , _dvsrMaxResults :: Maybe Integer
      -- ^ The maximum number of paginated volume items per response.
    , _dvsrNextToken :: Maybe Text
      -- ^ The next paginated set of results to return using the pagination
      -- token returned by a previous call.
    , _dvsrVolumeIds :: [Text]
      -- ^ One or more volume IDs. Default: Describes all your volumes.
    } deriving (Show, Generic)

-- | One or more filters. action.code - The action code for the event (for
-- example, enable-volume-io). action.description - A description of the
-- action. action.event-id - The event ID associated with the action.
-- availability-zone - The Availability Zone of the instance.
-- event.description - A description of the event. event.event-id - The event
-- ID. event.event-type - The event type (for io-enabled: passed | failed; for
-- io-performance: io-performance:degraded | io-performance:severely-degraded
-- | io-performance:stalled). event.not-after - The latest end time for the
-- event. event.not-before - The earliest start time for the event.
-- volume-status.details-name - The cause for volume-status.status (io-enabled
-- | io-performance). volume-status.details-status - The status of
-- volume-status.details-name (for io-enabled: passed | failed; for
-- io-performance: normal | degraded | severely-degraded | stalled).
-- volume-status.status - The status of the volume (ok | impaired | warning |
-- insufficient-data).
dvsrFilters :: Lens' DescribeVolumeStatus ([Filter])
dvsrFilters f x =
    f (_dvsrFilters x)
        <&> \y -> x { _dvsrFilters = y }
{-# INLINE dvsrFilters #-}

-- | The maximum number of paginated volume items per response.
dvsrMaxResults :: Lens' DescribeVolumeStatus (Maybe Integer)
dvsrMaxResults f x =
    f (_dvsrMaxResults x)
        <&> \y -> x { _dvsrMaxResults = y }
{-# INLINE dvsrMaxResults #-}

-- | The next paginated set of results to return using the pagination token
-- returned by a previous call.
dvsrNextToken :: Lens' DescribeVolumeStatus (Maybe Text)
dvsrNextToken f x =
    f (_dvsrNextToken x)
        <&> \y -> x { _dvsrNextToken = y }
{-# INLINE dvsrNextToken #-}

-- | One or more volume IDs. Default: Describes all your volumes.
dvsrVolumeIds :: Lens' DescribeVolumeStatus ([Text])
dvsrVolumeIds f x =
    f (_dvsrVolumeIds x)
        <&> \y -> x { _dvsrVolumeIds = y }
{-# INLINE dvsrVolumeIds #-}

instance ToQuery DescribeVolumeStatus where
    toQuery = genericQuery def

data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse
    { _dvssNextToken :: Maybe Text
      -- ^ The next paginated set of results to return.
    , _dvssVolumeStatuses :: [VolumeStatusItem]
      -- ^ A list of volumes.
    } deriving (Show, Generic)

-- | The next paginated set of results to return.
dvssNextToken :: Lens' DescribeVolumeStatusResponse (Maybe Text)
dvssNextToken f x =
    f (_dvssNextToken x)
        <&> \y -> x { _dvssNextToken = y }
{-# INLINE dvssNextToken #-}

-- | A list of volumes.
dvssVolumeStatuses :: Lens' DescribeVolumeStatusResponse ([VolumeStatusItem])
dvssVolumeStatuses f x =
    f (_dvssVolumeStatuses x)
        <&> \y -> x { _dvssVolumeStatuses = y }
{-# INLINE dvssVolumeStatuses #-}

instance FromXML DescribeVolumeStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeVolumeStatus where
    type Sv DescribeVolumeStatus = EC2
    type Rs DescribeVolumeStatus = DescribeVolumeStatusResponse

    request = post "DescribeVolumeStatus"
    response _ = xmlResponse

instance AWSPager DescribeVolumeStatus where
    next rq rs = (\x -> rq { _dvsrNextToken = Just x })
        <$> (_dvssNextToken rs)
