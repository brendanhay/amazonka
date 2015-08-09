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
-- Module      : Network.AWS.EC2.DescribePlacementGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your placement groups. For more information
-- about placement groups and cluster instances, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using_cluster_computing.html Cluster Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribePlacementGroups.html AWS API Reference> for DescribePlacementGroups.
module Network.AWS.EC2.DescribePlacementGroups
    (
    -- * Creating a Request
      DescribePlacementGroups
    , describePlacementGroups
    -- * Request Lenses
    , dpgsGroupNames
    , dpgsFilters
    , dpgsDryRun

    -- * Destructuring the Response
    , DescribePlacementGroupsResponse
    , describePlacementGroupsResponse
    -- * Response Lenses
    , dpgrsPlacementGroups
    , dpgrsStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePlacementGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpgsGroupNames'
--
-- * 'dpgsFilters'
--
-- * 'dpgsDryRun'
data DescribePlacementGroups = DescribePlacementGroups'
    { _dpgsGroupNames :: !(Maybe [Text])
    , _dpgsFilters :: !(Maybe [Filter])
    , _dpgsDryRun :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribePlacementGroups' smart constructor.
describePlacementGroups :: DescribePlacementGroups
describePlacementGroups = 
    DescribePlacementGroups'
    { _dpgsGroupNames = Nothing
    , _dpgsFilters = Nothing
    , _dpgsDryRun = Nothing
    }

-- | One or more placement group names.
--
-- Default: Describes all your placement groups, or only those otherwise
-- specified.
dpgsGroupNames :: Lens' DescribePlacementGroups [Text]
dpgsGroupNames = lens _dpgsGroupNames (\ s a -> s{_dpgsGroupNames = a}) . _Default . _Coerce;

-- | One or more filters.
--
-- -   @group-name@ - The name of the placement group.
--
-- -   @state@ - The state of the placement group (@pending@ | @available@
--     | @deleting@ | @deleted@).
--
-- -   @strategy@ - The strategy of the placement group (@cluster@).
--
dpgsFilters :: Lens' DescribePlacementGroups [Filter]
dpgsFilters = lens _dpgsFilters (\ s a -> s{_dpgsFilters = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dpgsDryRun :: Lens' DescribePlacementGroups (Maybe Bool)
dpgsDryRun = lens _dpgsDryRun (\ s a -> s{_dpgsDryRun = a});

instance AWSRequest DescribePlacementGroups where
        type Sv DescribePlacementGroups = EC2
        type Rs DescribePlacementGroups =
             DescribePlacementGroupsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribePlacementGroupsResponse' <$>
                   (x .@? "placementGroupSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

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
                 (toQueryList "GroupName" <$> _dpgsGroupNames),
               toQuery (toQueryList "Filter" <$> _dpgsFilters),
               "DryRun" =: _dpgsDryRun]

-- | /See:/ 'describePlacementGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpgrsPlacementGroups'
--
-- * 'dpgrsStatus'
data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse'
    { _dpgrsPlacementGroups :: !(Maybe [PlacementGroup])
    , _dpgrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribePlacementGroupsResponse' smart constructor.
describePlacementGroupsResponse :: Int -> DescribePlacementGroupsResponse
describePlacementGroupsResponse pStatus_ = 
    DescribePlacementGroupsResponse'
    { _dpgrsPlacementGroups = Nothing
    , _dpgrsStatus = pStatus_
    }

-- | One or more placement groups.
dpgrsPlacementGroups :: Lens' DescribePlacementGroupsResponse [PlacementGroup]
dpgrsPlacementGroups = lens _dpgrsPlacementGroups (\ s a -> s{_dpgrsPlacementGroups = a}) . _Default . _Coerce;

-- | Undocumented member.
dpgrsStatus :: Lens' DescribePlacementGroupsResponse Int
dpgrsStatus = lens _dpgrsStatus (\ s a -> s{_dpgrsStatus = a});
