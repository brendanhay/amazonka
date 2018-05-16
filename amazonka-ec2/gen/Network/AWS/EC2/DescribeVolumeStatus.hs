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
-- Module      : Network.AWS.EC2.DescribeVolumeStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the specified volumes. Volume status provides the result of the checks performed on your volumes to determine events that can impair the performance of your volumes. The performance of a volume can be affected if an issue occurs on the volume's underlying host. If the volume's underlying host experiences a power outage or system issue, after the system is restored, there could be data inconsistencies on the volume. Volume events notify you if this occurs. Volume actions notify you if any action needs to be taken in response to the event.
--
--
-- The @DescribeVolumeStatus@ operation provides the following information about the specified volumes:
--
-- /Status/ : Reflects the current status of the volume. The possible values are @ok@ , @impaired@ , @warning@ , or @insufficient-data@ . If all checks pass, the overall status of the volume is @ok@ . If the check fails, the overall status is @impaired@ . If the status is @insufficient-data@ , then the checks may still be taking place on your volume at the time. We recommend that you retry the request. For more information on volume status, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-volume-status.html Monitoring the Status of Your Volumes> .
--
-- /Events/ : Reflect the cause of a volume status and may require you to take action. For example, if your volume returns an @impaired@ status, then the volume event might be @potential-data-inconsistency@ . This means that your volume has been affected by an issue with the underlying host, has all I/O operations disabled, and may have inconsistent data.
--
-- /Actions/ : Reflect the actions you may have to take in response to an event. For example, if the status of the volume is @impaired@ and the volume event shows @potential-data-inconsistency@ , then the action shows @enable-volume-io@ . This means that you may want to enable the I/O operations for the volume by calling the 'EnableVolumeIO' action and then check the volume for data consistency.
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVolumeStatus
    (
    -- * Creating a Request
      describeVolumeStatus
    , DescribeVolumeStatus
    -- * Request Lenses
    , dvssFilters
    , dvssVolumeIds
    , dvssNextToken
    , dvssDryRun
    , dvssMaxResults

    -- * Destructuring the Response
    , describeVolumeStatusResponse
    , DescribeVolumeStatusResponse
    -- * Response Lenses
    , dvsrsNextToken
    , dvsrsVolumeStatuses
    , dvsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeVolumeStatus.
--
--
--
-- /See:/ 'describeVolumeStatus' smart constructor.
data DescribeVolumeStatus = DescribeVolumeStatus'
  { _dvssFilters    :: !(Maybe [Filter])
  , _dvssVolumeIds  :: !(Maybe [Text])
  , _dvssNextToken  :: !(Maybe Text)
  , _dvssDryRun     :: !(Maybe Bool)
  , _dvssMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVolumeStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvssFilters' - One or more filters.     * @action.code@ - The action code for the event (for example, @enable-volume-io@ ).     * @action.description@ - A description of the action.     * @action.event-id@ - The event ID associated with the action.     * @availability-zone@ - The Availability Zone of the instance.     * @event.description@ - A description of the event.     * @event.event-id@ - The event ID.     * @event.event-type@ - The event type (for @io-enabled@ : @passed@ | @failed@ ; for @io-performance@ : @io-performance:degraded@ | @io-performance:severely-degraded@ | @io-performance:stalled@ ).     * @event.not-after@ - The latest end time for the event.     * @event.not-before@ - The earliest start time for the event.     * @volume-status.details-name@ - The cause for @volume-status.status@ (@io-enabled@ | @io-performance@ ).     * @volume-status.details-status@ - The status of @volume-status.details-name@ (for @io-enabled@ : @passed@ | @failed@ ; for @io-performance@ : @normal@ | @degraded@ | @severely-degraded@ | @stalled@ ).     * @volume-status.status@ - The status of the volume (@ok@ | @impaired@ | @warning@ | @insufficient-data@ ).
--
-- * 'dvssVolumeIds' - One or more volume IDs. Default: Describes all your volumes.
--
-- * 'dvssNextToken' - The @NextToken@ value to include in a future @DescribeVolumeStatus@ request. When the results of the request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dvssDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvssMaxResults' - The maximum number of volume results returned by @DescribeVolumeStatus@ in paginated output. When this parameter is used, the request only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeVolumeStatus@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
describeVolumeStatus
    :: DescribeVolumeStatus
describeVolumeStatus =
  DescribeVolumeStatus'
    { _dvssFilters = Nothing
    , _dvssVolumeIds = Nothing
    , _dvssNextToken = Nothing
    , _dvssDryRun = Nothing
    , _dvssMaxResults = Nothing
    }


-- | One or more filters.     * @action.code@ - The action code for the event (for example, @enable-volume-io@ ).     * @action.description@ - A description of the action.     * @action.event-id@ - The event ID associated with the action.     * @availability-zone@ - The Availability Zone of the instance.     * @event.description@ - A description of the event.     * @event.event-id@ - The event ID.     * @event.event-type@ - The event type (for @io-enabled@ : @passed@ | @failed@ ; for @io-performance@ : @io-performance:degraded@ | @io-performance:severely-degraded@ | @io-performance:stalled@ ).     * @event.not-after@ - The latest end time for the event.     * @event.not-before@ - The earliest start time for the event.     * @volume-status.details-name@ - The cause for @volume-status.status@ (@io-enabled@ | @io-performance@ ).     * @volume-status.details-status@ - The status of @volume-status.details-name@ (for @io-enabled@ : @passed@ | @failed@ ; for @io-performance@ : @normal@ | @degraded@ | @severely-degraded@ | @stalled@ ).     * @volume-status.status@ - The status of the volume (@ok@ | @impaired@ | @warning@ | @insufficient-data@ ).
dvssFilters :: Lens' DescribeVolumeStatus [Filter]
dvssFilters = lens _dvssFilters (\ s a -> s{_dvssFilters = a}) . _Default . _Coerce

-- | One or more volume IDs. Default: Describes all your volumes.
dvssVolumeIds :: Lens' DescribeVolumeStatus [Text]
dvssVolumeIds = lens _dvssVolumeIds (\ s a -> s{_dvssVolumeIds = a}) . _Default . _Coerce

-- | The @NextToken@ value to include in a future @DescribeVolumeStatus@ request. When the results of the request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
dvssNextToken :: Lens' DescribeVolumeStatus (Maybe Text)
dvssNextToken = lens _dvssNextToken (\ s a -> s{_dvssNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvssDryRun :: Lens' DescribeVolumeStatus (Maybe Bool)
dvssDryRun = lens _dvssDryRun (\ s a -> s{_dvssDryRun = a})

-- | The maximum number of volume results returned by @DescribeVolumeStatus@ in paginated output. When this parameter is used, the request only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeVolumeStatus@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
dvssMaxResults :: Lens' DescribeVolumeStatus (Maybe Int)
dvssMaxResults = lens _dvssMaxResults (\ s a -> s{_dvssMaxResults = a})

instance AWSPager DescribeVolumeStatus where
        page rq rs
          | stop (rs ^. dvsrsNextToken) = Nothing
          | stop (rs ^. dvsrsVolumeStatuses) = Nothing
          | otherwise =
            Just $ rq & dvssNextToken .~ rs ^. dvsrsNextToken

instance AWSRequest DescribeVolumeStatus where
        type Rs DescribeVolumeStatus =
             DescribeVolumeStatusResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeVolumeStatusResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "volumeStatusSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeVolumeStatus where

instance NFData DescribeVolumeStatus where

instance ToHeaders DescribeVolumeStatus where
        toHeaders = const mempty

instance ToPath DescribeVolumeStatus where
        toPath = const "/"

instance ToQuery DescribeVolumeStatus where
        toQuery DescribeVolumeStatus'{..}
          = mconcat
              ["Action" =: ("DescribeVolumeStatus" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvssFilters),
               toQuery (toQueryList "VolumeId" <$> _dvssVolumeIds),
               "NextToken" =: _dvssNextToken,
               "DryRun" =: _dvssDryRun,
               "MaxResults" =: _dvssMaxResults]

-- | Contains the output of DescribeVolumeStatus.
--
--
--
-- /See:/ 'describeVolumeStatusResponse' smart constructor.
data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse'
  { _dvsrsNextToken      :: !(Maybe Text)
  , _dvsrsVolumeStatuses :: !(Maybe [VolumeStatusItem])
  , _dvsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVolumeStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvsrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dvsrsVolumeStatuses' - A list of volumes.
--
-- * 'dvsrsResponseStatus' - -- | The response status code.
describeVolumeStatusResponse
    :: Int -- ^ 'dvsrsResponseStatus'
    -> DescribeVolumeStatusResponse
describeVolumeStatusResponse pResponseStatus_ =
  DescribeVolumeStatusResponse'
    { _dvsrsNextToken = Nothing
    , _dvsrsVolumeStatuses = Nothing
    , _dvsrsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dvsrsNextToken :: Lens' DescribeVolumeStatusResponse (Maybe Text)
dvsrsNextToken = lens _dvsrsNextToken (\ s a -> s{_dvsrsNextToken = a})

-- | A list of volumes.
dvsrsVolumeStatuses :: Lens' DescribeVolumeStatusResponse [VolumeStatusItem]
dvsrsVolumeStatuses = lens _dvsrsVolumeStatuses (\ s a -> s{_dvsrsVolumeStatuses = a}) . _Default . _Coerce

-- | -- | The response status code.
dvsrsResponseStatus :: Lens' DescribeVolumeStatusResponse Int
dvsrsResponseStatus = lens _dvsrsResponseStatus (\ s a -> s{_dvsrsResponseStatus = a})

instance NFData DescribeVolumeStatusResponse where
