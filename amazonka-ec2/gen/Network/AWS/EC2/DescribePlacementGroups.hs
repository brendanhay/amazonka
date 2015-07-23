{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribePlacementGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your placement groups. For more information
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
    , dpgsrqGroupNames
    , dpgsrqFilters
    , dpgsrqDryRun

    -- * Response
    , DescribePlacementGroupsResponse
    -- ** Response constructor
    , describePlacementGroupsResponse
    -- ** Response lenses
    , dpgrsPlacementGroups
    , dpgrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describePlacementGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpgsrqGroupNames'
--
-- * 'dpgsrqFilters'
--
-- * 'dpgsrqDryRun'
data DescribePlacementGroups = DescribePlacementGroups'
    { _dpgsrqGroupNames :: !(Maybe [Text])
    , _dpgsrqFilters    :: !(Maybe [Filter])
    , _dpgsrqDryRun     :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribePlacementGroups' smart constructor.
describePlacementGroups :: DescribePlacementGroups
describePlacementGroups =
    DescribePlacementGroups'
    { _dpgsrqGroupNames = Nothing
    , _dpgsrqFilters = Nothing
    , _dpgsrqDryRun = Nothing
    }

-- | One or more placement group names.
--
-- Default: Describes all your placement groups, or only those otherwise
-- specified.
dpgsrqGroupNames :: Lens' DescribePlacementGroups [Text]
dpgsrqGroupNames = lens _dpgsrqGroupNames (\ s a -> s{_dpgsrqGroupNames = a}) . _Default;

-- | One or more filters.
--
-- -   @group-name@ - The name of the placement group.
--
-- -   @state@ - The state of the placement group (@pending@ | @available@
--     | @deleting@ | @deleted@).
--
-- -   @strategy@ - The strategy of the placement group (@cluster@).
--
dpgsrqFilters :: Lens' DescribePlacementGroups [Filter]
dpgsrqFilters = lens _dpgsrqFilters (\ s a -> s{_dpgsrqFilters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dpgsrqDryRun :: Lens' DescribePlacementGroups (Maybe Bool)
dpgsrqDryRun = lens _dpgsrqDryRun (\ s a -> s{_dpgsrqDryRun = a});

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
                 (toQueryList "GroupName" <$> _dpgsrqGroupNames),
               toQuery (toQueryList "Filter" <$> _dpgsrqFilters),
               "DryRun" =: _dpgsrqDryRun]

-- | /See:/ 'describePlacementGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpgrsPlacementGroups'
--
-- * 'dpgrsStatus'
data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse'
    { _dpgrsPlacementGroups :: !(Maybe [PlacementGroup])
    , _dpgrsStatus          :: !Int
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
dpgrsPlacementGroups = lens _dpgrsPlacementGroups (\ s a -> s{_dpgrsPlacementGroups = a}) . _Default;

-- | FIXME: Undocumented member.
dpgrsStatus :: Lens' DescribePlacementGroupsResponse Int
dpgrsStatus = lens _dpgrsStatus (\ s a -> s{_dpgrsStatus = a});
