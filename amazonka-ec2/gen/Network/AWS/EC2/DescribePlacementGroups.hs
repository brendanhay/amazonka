{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DescribePlacementGroups
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

-- | Describes one or more of your placement groups. For more information
-- about placement groups and cluster instances, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using_cluster_computing.html Cluster Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribePlacementGroups.html>
module Network.AWS.EC2.DescribePlacementGroups
    (
    -- * Request
      DescribePlacementGroups
    -- ** Request constructor
    , describePlacementGroups
    -- ** Request lenses
    , dpg1GroupNames
    , dpg1Filters
    , dpg1DryRun

    -- * Response
    , DescribePlacementGroupsResponse
    -- ** Response constructor
    , describePlacementGroupsResponse
    -- ** Response lenses
    , dpgrPlacementGroups
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePlacementGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpg1GroupNames'
--
-- * 'dpg1Filters'
--
-- * 'dpg1DryRun'
data DescribePlacementGroups = DescribePlacementGroups'{_dpg1GroupNames :: Maybe [Text], _dpg1Filters :: Maybe [Filter], _dpg1DryRun :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'DescribePlacementGroups' smart constructor.
describePlacementGroups :: DescribePlacementGroups
describePlacementGroups = DescribePlacementGroups'{_dpg1GroupNames = Nothing, _dpg1Filters = Nothing, _dpg1DryRun = Nothing};

-- | One or more placement group names.
--
-- Default: Describes all your placement groups, or only those otherwise
-- specified.
dpg1GroupNames :: Lens' DescribePlacementGroups [Text]
dpg1GroupNames = lens _dpg1GroupNames (\ s a -> s{_dpg1GroupNames = a}) . _Default;

-- | One or more filters.
--
-- -   @group-name@ - The name of the placement group.
--
-- -   @state@ - The state of the placement group (@pending@ | @available@
--     | @deleting@ | @deleted@).
--
-- -   @strategy@ - The strategy of the placement group (@cluster@).
--
dpg1Filters :: Lens' DescribePlacementGroups [Filter]
dpg1Filters = lens _dpg1Filters (\ s a -> s{_dpg1Filters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dpg1DryRun :: Lens' DescribePlacementGroups (Maybe Bool)
dpg1DryRun = lens _dpg1DryRun (\ s a -> s{_dpg1DryRun = a});

instance AWSRequest DescribePlacementGroups where
        type Sv DescribePlacementGroups = EC2
        type Rs DescribePlacementGroups =
             DescribePlacementGroupsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribePlacementGroupsResponse' <$>
                   (may (parseXMLList "item") x))

instance ToHeaders DescribePlacementGroups where
        toHeaders = const mempty

instance ToPath DescribePlacementGroups where
        toPath = const "/"

instance ToQuery DescribePlacementGroups where
        toQuery DescribePlacementGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribePlacementGroups" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery
                 (toQueryList "GroupName" <$> _dpg1GroupNames),
               toQuery (toQueryList "Filter" <$> _dpg1Filters),
               "DryRun" =: _dpg1DryRun]

-- | /See:/ 'describePlacementGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpgrPlacementGroups'
newtype DescribePlacementGroupsResponse = DescribePlacementGroupsResponse'{_dpgrPlacementGroups :: Maybe [PlacementGroup]} deriving (Eq, Read, Show)

-- | 'DescribePlacementGroupsResponse' smart constructor.
describePlacementGroupsResponse :: DescribePlacementGroupsResponse
describePlacementGroupsResponse = DescribePlacementGroupsResponse'{_dpgrPlacementGroups = Nothing};

-- | One or more placement groups.
dpgrPlacementGroups :: Lens' DescribePlacementGroupsResponse [PlacementGroup]
dpgrPlacementGroups = lens _dpgrPlacementGroups (\ s a -> s{_dpgrPlacementGroups = a}) . _Default;
