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
-- Module      : Network.AWS.EC2.DescribeVolumesModifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reports the current modification status of EBS volumes.
--
--
-- Current-generation EBS volumes support modification of attributes including type, size, and (for @io1@ volumes) IOPS provisioning while either attached to or detached from an instance. Following an action from the API or the console to modify a volume, the status of the modification may be @modifying@ , @optimizing@ , @completed@ , or @failed@ . If a volume has never been modified, then certain elements of the returned @VolumeModification@ objects are null.
--
-- You can also use CloudWatch Events to check the status of a modification to an EBS volume. For information about CloudWatch Events, see the <http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ Amazon CloudWatch Events User Guide> . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#monitoring_mods Monitoring Volume Modifications"> .
--
module Network.AWS.EC2.DescribeVolumesModifications
    (
    -- * Creating a Request
      describeVolumesModifications
    , DescribeVolumesModifications
    -- * Request Lenses
    , dvmFilters
    , dvmVolumeIds
    , dvmNextToken
    , dvmDryRun
    , dvmMaxResults

    -- * Destructuring the Response
    , describeVolumesModificationsResponse
    , DescribeVolumesModificationsResponse
    -- * Response Lenses
    , dvmrsVolumesModifications
    , dvmrsNextToken
    , dvmrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVolumesModifications' smart constructor.
data DescribeVolumesModifications = DescribeVolumesModifications'
  { _dvmFilters    :: !(Maybe [Filter])
  , _dvmVolumeIds  :: !(Maybe [Text])
  , _dvmNextToken  :: !(Maybe Text)
  , _dvmDryRun     :: !(Maybe Bool)
  , _dvmMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVolumesModifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvmFilters' - One or more filters. Supported filters: @volume-id@ , @modification-state@ , @target-size@ , @target-iops@ , @target-volume-type@ , @original-size@ , @original-iops@ , @original-volume-type@ , @start-time@ .
--
-- * 'dvmVolumeIds' - One or more volume IDs for which in-progress modifications will be described.
--
-- * 'dvmNextToken' - The @nextToken@ value returned by a previous paginated request.
--
-- * 'dvmDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvmMaxResults' - The maximum number of results (up to a limit of 500) to be returned in a paginated request.
describeVolumesModifications
    :: DescribeVolumesModifications
describeVolumesModifications =
  DescribeVolumesModifications'
    { _dvmFilters = Nothing
    , _dvmVolumeIds = Nothing
    , _dvmNextToken = Nothing
    , _dvmDryRun = Nothing
    , _dvmMaxResults = Nothing
    }


-- | One or more filters. Supported filters: @volume-id@ , @modification-state@ , @target-size@ , @target-iops@ , @target-volume-type@ , @original-size@ , @original-iops@ , @original-volume-type@ , @start-time@ .
dvmFilters :: Lens' DescribeVolumesModifications [Filter]
dvmFilters = lens _dvmFilters (\ s a -> s{_dvmFilters = a}) . _Default . _Coerce

-- | One or more volume IDs for which in-progress modifications will be described.
dvmVolumeIds :: Lens' DescribeVolumesModifications [Text]
dvmVolumeIds = lens _dvmVolumeIds (\ s a -> s{_dvmVolumeIds = a}) . _Default . _Coerce

-- | The @nextToken@ value returned by a previous paginated request.
dvmNextToken :: Lens' DescribeVolumesModifications (Maybe Text)
dvmNextToken = lens _dvmNextToken (\ s a -> s{_dvmNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvmDryRun :: Lens' DescribeVolumesModifications (Maybe Bool)
dvmDryRun = lens _dvmDryRun (\ s a -> s{_dvmDryRun = a})

-- | The maximum number of results (up to a limit of 500) to be returned in a paginated request.
dvmMaxResults :: Lens' DescribeVolumesModifications (Maybe Int)
dvmMaxResults = lens _dvmMaxResults (\ s a -> s{_dvmMaxResults = a})

instance AWSRequest DescribeVolumesModifications
         where
        type Rs DescribeVolumesModifications =
             DescribeVolumesModificationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeVolumesModificationsResponse' <$>
                   (x .@? "volumeModificationSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeVolumesModifications where

instance NFData DescribeVolumesModifications where

instance ToHeaders DescribeVolumesModifications where
        toHeaders = const mempty

instance ToPath DescribeVolumesModifications where
        toPath = const "/"

instance ToQuery DescribeVolumesModifications where
        toQuery DescribeVolumesModifications'{..}
          = mconcat
              ["Action" =:
                 ("DescribeVolumesModifications" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvmFilters),
               toQuery (toQueryList "VolumeId" <$> _dvmVolumeIds),
               "NextToken" =: _dvmNextToken, "DryRun" =: _dvmDryRun,
               "MaxResults" =: _dvmMaxResults]

-- | /See:/ 'describeVolumesModificationsResponse' smart constructor.
data DescribeVolumesModificationsResponse = DescribeVolumesModificationsResponse'
  { _dvmrsVolumesModifications :: !(Maybe [VolumeModification])
  , _dvmrsNextToken            :: !(Maybe Text)
  , _dvmrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVolumesModificationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvmrsVolumesModifications' - A list of returned 'VolumeModification' objects.
--
-- * 'dvmrsNextToken' - Token for pagination, null if there are no more results
--
-- * 'dvmrsResponseStatus' - -- | The response status code.
describeVolumesModificationsResponse
    :: Int -- ^ 'dvmrsResponseStatus'
    -> DescribeVolumesModificationsResponse
describeVolumesModificationsResponse pResponseStatus_ =
  DescribeVolumesModificationsResponse'
    { _dvmrsVolumesModifications = Nothing
    , _dvmrsNextToken = Nothing
    , _dvmrsResponseStatus = pResponseStatus_
    }


-- | A list of returned 'VolumeModification' objects.
dvmrsVolumesModifications :: Lens' DescribeVolumesModificationsResponse [VolumeModification]
dvmrsVolumesModifications = lens _dvmrsVolumesModifications (\ s a -> s{_dvmrsVolumesModifications = a}) . _Default . _Coerce

-- | Token for pagination, null if there are no more results
dvmrsNextToken :: Lens' DescribeVolumesModificationsResponse (Maybe Text)
dvmrsNextToken = lens _dvmrsNextToken (\ s a -> s{_dvmrsNextToken = a})

-- | -- | The response status code.
dvmrsResponseStatus :: Lens' DescribeVolumesModificationsResponse Int
dvmrsResponseStatus = lens _dvmrsResponseStatus (\ s a -> s{_dvmrsResponseStatus = a})

instance NFData DescribeVolumesModificationsResponse
         where
