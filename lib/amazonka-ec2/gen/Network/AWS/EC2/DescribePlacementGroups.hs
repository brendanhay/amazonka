{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribePlacementGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified placement groups or all of your placement groups. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribePlacementGroups
  ( -- * Creating a Request
    describePlacementGroups,
    DescribePlacementGroups,

    -- * Request Lenses
    dpgsFilters,
    dpgsGroupNames,
    dpgsGroupIds,
    dpgsDryRun,

    -- * Destructuring the Response
    describePlacementGroupsResponse,
    DescribePlacementGroupsResponse,

    -- * Response Lenses
    dpgrsPlacementGroups,
    dpgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePlacementGroups' smart constructor.
data DescribePlacementGroups = DescribePlacementGroups'
  { _dpgsFilters ::
      !(Maybe [Filter]),
    _dpgsGroupNames :: !(Maybe [Text]),
    _dpgsGroupIds :: !(Maybe [Text]),
    _dpgsDryRun :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribePlacementGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgsFilters' - The filters.     * @group-name@ - The name of the placement group.     * @state@ - The state of the placement group (@pending@ | @available@ | @deleting@ | @deleted@ ).     * @strategy@ - The strategy of the placement group (@cluster@ | @spread@ | @partition@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources that have a tag with a specific key, regardless of the tag value.
--
-- * 'dpgsGroupNames' - The names of the placement groups. Default: Describes all your placement groups, or only those otherwise specified.
--
-- * 'dpgsGroupIds' - The IDs of the placement groups.
--
-- * 'dpgsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describePlacementGroups ::
  DescribePlacementGroups
describePlacementGroups =
  DescribePlacementGroups'
    { _dpgsFilters = Nothing,
      _dpgsGroupNames = Nothing,
      _dpgsGroupIds = Nothing,
      _dpgsDryRun = Nothing
    }

-- | The filters.     * @group-name@ - The name of the placement group.     * @state@ - The state of the placement group (@pending@ | @available@ | @deleting@ | @deleted@ ).     * @strategy@ - The strategy of the placement group (@cluster@ | @spread@ | @partition@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources that have a tag with a specific key, regardless of the tag value.
dpgsFilters :: Lens' DescribePlacementGroups [Filter]
dpgsFilters = lens _dpgsFilters (\s a -> s {_dpgsFilters = a}) . _Default . _Coerce

-- | The names of the placement groups. Default: Describes all your placement groups, or only those otherwise specified.
dpgsGroupNames :: Lens' DescribePlacementGroups [Text]
dpgsGroupNames = lens _dpgsGroupNames (\s a -> s {_dpgsGroupNames = a}) . _Default . _Coerce

-- | The IDs of the placement groups.
dpgsGroupIds :: Lens' DescribePlacementGroups [Text]
dpgsGroupIds = lens _dpgsGroupIds (\s a -> s {_dpgsGroupIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dpgsDryRun :: Lens' DescribePlacementGroups (Maybe Bool)
dpgsDryRun = lens _dpgsDryRun (\s a -> s {_dpgsDryRun = a})

instance AWSRequest DescribePlacementGroups where
  type Rs DescribePlacementGroups = DescribePlacementGroupsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribePlacementGroupsResponse'
            <$> ( x .@? "placementGroupSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribePlacementGroups

instance NFData DescribePlacementGroups

instance ToHeaders DescribePlacementGroups where
  toHeaders = const mempty

instance ToPath DescribePlacementGroups where
  toPath = const "/"

instance ToQuery DescribePlacementGroups where
  toQuery DescribePlacementGroups' {..} =
    mconcat
      [ "Action" =: ("DescribePlacementGroups" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dpgsFilters),
        toQuery (toQueryList "GroupName" <$> _dpgsGroupNames),
        toQuery (toQueryList "GroupId" <$> _dpgsGroupIds),
        "DryRun" =: _dpgsDryRun
      ]

-- | /See:/ 'describePlacementGroupsResponse' smart constructor.
data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse'
  { _dpgrsPlacementGroups ::
      !(Maybe [PlacementGroup]),
    _dpgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribePlacementGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgrsPlacementGroups' - Information about the placement groups.
--
-- * 'dpgrsResponseStatus' - -- | The response status code.
describePlacementGroupsResponse ::
  -- | 'dpgrsResponseStatus'
  Int ->
  DescribePlacementGroupsResponse
describePlacementGroupsResponse pResponseStatus_ =
  DescribePlacementGroupsResponse'
    { _dpgrsPlacementGroups = Nothing,
      _dpgrsResponseStatus = pResponseStatus_
    }

-- | Information about the placement groups.
dpgrsPlacementGroups :: Lens' DescribePlacementGroupsResponse [PlacementGroup]
dpgrsPlacementGroups = lens _dpgrsPlacementGroups (\s a -> s {_dpgrsPlacementGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
dpgrsResponseStatus :: Lens' DescribePlacementGroupsResponse Int
dpgrsResponseStatus = lens _dpgrsResponseStatus (\s a -> s {_dpgrsResponseStatus = a})

instance NFData DescribePlacementGroupsResponse
