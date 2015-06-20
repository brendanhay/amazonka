{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DescribeVolumeStatus
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the status of the specified volumes. Volume status provides
-- the result of the checks performed on your volumes to determine events
-- that can impair the performance of your volumes. The performance of a
-- volume can be affected if an issue occurs on the volume\'s underlying
-- host. If the volume\'s underlying host experiences a power outage or
-- system issue, after the system is restored, there could be data
-- inconsistencies on the volume. Volume events notify you if this occurs.
-- Volume actions notify you if any action needs to be taken in response to
-- the event.
--
-- The @DescribeVolumeStatus@ operation provides the following information
-- about the specified volumes:
--
-- /Status/: Reflects the current status of the volume. The possible values
-- are @ok@, @impaired@ , @warning@, or @insufficient-data@. If all checks
-- pass, the overall status of the volume is @ok@. If the check fails, the
-- overall status is @impaired@. If the status is @insufficient-data@, then
-- the checks may still be taking place on your volume at the time. We
-- recommend that you retry the request. For more information on volume
-- status, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-volume-status.html Monitoring the Status of Your Volumes>.
--
-- /Events/: Reflect the cause of a volume status and may require you to
-- take action. For example, if your volume returns an @impaired@ status,
-- then the volume event might be @potential-data-inconsistency@. This
-- means that your volume has been affected by an issue with the underlying
-- host, has all I\/O operations disabled, and may have inconsistent data.
--
-- /Actions/: Reflect the actions you may have to take in response to an
-- event. For example, if the status of the volume is @impaired@ and the
-- volume event shows @potential-data-inconsistency@, then the action shows
-- @enable-volume-io@. This means that you may want to enable the I\/O
-- operations for the volume by calling the EnableVolumeIO action and then
-- check the volume for data consistency.
--
-- Volume status is based on the volume status checks, and does not reflect
-- the volume state. Therefore, volume status does not indicate volumes in
-- the @error@ state (for example, when a volume is incapable of accepting
-- I\/O.)
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumeStatus.html>
module Network.AWS.EC2.DescribeVolumeStatus
    (
    -- * Request
      DescribeVolumeStatus
    -- ** Request constructor
    , describeVolumeStatus
    -- ** Request lenses
    , dvsFilters
    , dvsVolumeIds
    , dvsNextToken
    , dvsDryRun
    , dvsMaxResults

    -- * Response
    , DescribeVolumeStatusResponse
    -- ** Response constructor
    , describeVolumeStatusResponse
    -- ** Response lenses
    , dvsrNextToken
    , dvsrVolumeStatuses
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVolumeStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvsFilters'
--
-- * 'dvsVolumeIds'
--
-- * 'dvsNextToken'
--
-- * 'dvsDryRun'
--
-- * 'dvsMaxResults'
data DescribeVolumeStatus = DescribeVolumeStatus'{_dvsFilters :: Maybe [Filter], _dvsVolumeIds :: Maybe [Text], _dvsNextToken :: Maybe Text, _dvsDryRun :: Maybe Bool, _dvsMaxResults :: Maybe Int} deriving (Eq, Read, Show)

-- | 'DescribeVolumeStatus' smart constructor.
describeVolumeStatus :: DescribeVolumeStatus
describeVolumeStatus = DescribeVolumeStatus'{_dvsFilters = Nothing, _dvsVolumeIds = Nothing, _dvsNextToken = Nothing, _dvsDryRun = Nothing, _dvsMaxResults = Nothing};

-- | One or more filters.
--
-- -   @action.code@ - The action code for the event (for example,
--     @enable-volume-io@).
--
-- -   @action.description@ - A description of the action.
--
-- -   @action.event-id@ - The event ID associated with the action.
--
-- -   @availability-zone@ - The Availability Zone of the instance.
--
-- -   @event.description@ - A description of the event.
--
-- -   @event.event-id@ - The event ID.
--
-- -   @event.event-type@ - The event type (for @io-enabled@: @passed@ |
--     @failed@; for @io-performance@: @io-performance:degraded@ |
--     @io-performance:severely-degraded@ | @io-performance:stalled@).
--
-- -   @event.not-after@ - The latest end time for the event.
--
-- -   @event.not-before@ - The earliest start time for the event.
--
-- -   @volume-status.details-name@ - The cause for @volume-status.status@
--     (@io-enabled@ | @io-performance@).
--
-- -   @volume-status.details-status@ - The status of
--     @volume-status.details-name@ (for @io-enabled@: @passed@ | @failed@;
--     for @io-performance@: @normal@ | @degraded@ | @severely-degraded@ |
--     @stalled@).
--
-- -   @volume-status.status@ - The status of the volume (@ok@ | @impaired@
--     | @warning@ | @insufficient-data@).
--
dvsFilters :: Lens' DescribeVolumeStatus [Filter]
dvsFilters = lens _dvsFilters (\ s a -> s{_dvsFilters = a}) . _Default;

-- | One or more volume IDs.
--
-- Default: Describes all your volumes.
dvsVolumeIds :: Lens' DescribeVolumeStatus [Text]
dvsVolumeIds = lens _dvsVolumeIds (\ s a -> s{_dvsVolumeIds = a}) . _Default;

-- | The @NextToken@ value to include in a future @DescribeVolumeStatus@
-- request. When the results of the request exceed @MaxResults@, this value
-- can be used to retrieve the next page of results. This value is @null@
-- when there are no more results to return.
dvsNextToken :: Lens' DescribeVolumeStatus (Maybe Text)
dvsNextToken = lens _dvsNextToken (\ s a -> s{_dvsNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvsDryRun :: Lens' DescribeVolumeStatus (Maybe Bool)
dvsDryRun = lens _dvsDryRun (\ s a -> s{_dvsDryRun = a});

-- | The maximum number of volume results returned by @DescribeVolumeStatus@
-- in paginated output. When this parameter is used, the request only
-- returns @MaxResults@ results in a single page along with a @NextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another request with the returned @NextToken@ value.
-- This value can be between 5 and 1000; if @MaxResults@ is given a value
-- larger than 1000, only 1000 results are returned. If this parameter is
-- not used, then @DescribeVolumeStatus@ returns all results. You cannot
-- specify this parameter and the volume IDs parameter in the same request.
dvsMaxResults :: Lens' DescribeVolumeStatus (Maybe Int)
dvsMaxResults = lens _dvsMaxResults (\ s a -> s{_dvsMaxResults = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeVolumeStatus where
        type Sv DescribeVolumeStatus = EC2
        type Rs DescribeVolumeStatus =
             DescribeVolumeStatusResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeVolumeStatusResponse' <$>
                   (x .@? "nextToken") <*>
                     (may (parseXMLList "item") x))

instance ToHeaders DescribeVolumeStatus where
        toHeaders = const mempty

instance ToPath DescribeVolumeStatus where
        toPath = const "/"

instance ToQuery DescribeVolumeStatus where
        toQuery DescribeVolumeStatus'{..}
          = mconcat
              ["Action" =: ("DescribeVolumeStatus" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvsFilters),
               toQuery (toQueryList "VolumeId" <$> _dvsVolumeIds),
               "NextToken" =: _dvsNextToken, "DryRun" =: _dvsDryRun,
               "MaxResults" =: _dvsMaxResults]

-- | /See:/ 'describeVolumeStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvsrNextToken'
--
-- * 'dvsrVolumeStatuses'
data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse'{_dvsrNextToken :: Maybe Text, _dvsrVolumeStatuses :: Maybe [VolumeStatusItem]} deriving (Eq, Read, Show)

-- | 'DescribeVolumeStatusResponse' smart constructor.
describeVolumeStatusResponse :: DescribeVolumeStatusResponse
describeVolumeStatusResponse = DescribeVolumeStatusResponse'{_dvsrNextToken = Nothing, _dvsrVolumeStatuses = Nothing};

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
dvsrNextToken :: Lens' DescribeVolumeStatusResponse (Maybe Text)
dvsrNextToken = lens _dvsrNextToken (\ s a -> s{_dvsrNextToken = a});

-- | A list of volumes.
dvsrVolumeStatuses :: Lens' DescribeVolumeStatusResponse [VolumeStatusItem]
dvsrVolumeStatuses = lens _dvsrVolumeStatuses (\ s a -> s{_dvsrVolumeStatuses = a}) . _Default;
