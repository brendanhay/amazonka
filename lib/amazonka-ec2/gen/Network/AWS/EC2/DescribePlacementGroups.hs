{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    DescribePlacementGroups (..),
    mkDescribePlacementGroups,

    -- ** Request lenses
    dpgsFilters,
    dpgsGroupNames,
    dpgsGroupIds,
    dpgsDryRun,

    -- * Destructuring the response
    DescribePlacementGroupsResponse (..),
    mkDescribePlacementGroupsResponse,

    -- ** Response lenses
    dpgrsPlacementGroups,
    dpgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePlacementGroups' smart constructor.
data DescribePlacementGroups = DescribePlacementGroups'
  { filters ::
      Lude.Maybe [Filter],
    groupNames :: Lude.Maybe [Lude.Text],
    groupIds :: Lude.Maybe [Lude.Text],
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePlacementGroups' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - The filters.
--
--
--     * @group-name@ - The name of the placement group.
--
--
--     * @state@ - The state of the placement group (@pending@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @strategy@ - The strategy of the placement group (@cluster@ | @spread@ | @partition@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources that have a tag with a specific key, regardless of the tag value.
--
--
-- * 'groupIds' - The IDs of the placement groups.
-- * 'groupNames' - The names of the placement groups.
--
-- Default: Describes all your placement groups, or only those otherwise specified.
mkDescribePlacementGroups ::
  DescribePlacementGroups
mkDescribePlacementGroups =
  DescribePlacementGroups'
    { filters = Lude.Nothing,
      groupNames = Lude.Nothing,
      groupIds = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The filters.
--
--
--     * @group-name@ - The name of the placement group.
--
--
--     * @state@ - The state of the placement group (@pending@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @strategy@ - The strategy of the placement group (@cluster@ | @spread@ | @partition@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources that have a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsFilters :: Lens.Lens' DescribePlacementGroups (Lude.Maybe [Filter])
dpgsFilters = Lens.lens (filters :: DescribePlacementGroups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribePlacementGroups)
{-# DEPRECATED dpgsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The names of the placement groups.
--
-- Default: Describes all your placement groups, or only those otherwise specified.
--
-- /Note:/ Consider using 'groupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsGroupNames :: Lens.Lens' DescribePlacementGroups (Lude.Maybe [Lude.Text])
dpgsGroupNames = Lens.lens (groupNames :: DescribePlacementGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {groupNames = a} :: DescribePlacementGroups)
{-# DEPRECATED dpgsGroupNames "Use generic-lens or generic-optics with 'groupNames' instead." #-}

-- | The IDs of the placement groups.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsGroupIds :: Lens.Lens' DescribePlacementGroups (Lude.Maybe [Lude.Text])
dpgsGroupIds = Lens.lens (groupIds :: DescribePlacementGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {groupIds = a} :: DescribePlacementGroups)
{-# DEPRECATED dpgsGroupIds "Use generic-lens or generic-optics with 'groupIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsDryRun :: Lens.Lens' DescribePlacementGroups (Lude.Maybe Lude.Bool)
dpgsDryRun = Lens.lens (dryRun :: DescribePlacementGroups -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribePlacementGroups)
{-# DEPRECATED dpgsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribePlacementGroups where
  type Rs DescribePlacementGroups = DescribePlacementGroupsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribePlacementGroupsResponse'
            Lude.<$> ( x Lude..@? "placementGroupSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePlacementGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribePlacementGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePlacementGroups where
  toQuery DescribePlacementGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribePlacementGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery (Lude.toQueryList "GroupName" Lude.<$> groupNames),
        Lude.toQuery (Lude.toQueryList "GroupId" Lude.<$> groupIds),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribePlacementGroupsResponse' smart constructor.
data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse'
  { placementGroups ::
      Lude.Maybe [PlacementGroup],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePlacementGroupsResponse' with the minimum fields required to make a request.
--
-- * 'placementGroups' - Information about the placement groups.
-- * 'responseStatus' - The response status code.
mkDescribePlacementGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePlacementGroupsResponse
mkDescribePlacementGroupsResponse pResponseStatus_ =
  DescribePlacementGroupsResponse'
    { placementGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the placement groups.
--
-- /Note:/ Consider using 'placementGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrsPlacementGroups :: Lens.Lens' DescribePlacementGroupsResponse (Lude.Maybe [PlacementGroup])
dpgrsPlacementGroups = Lens.lens (placementGroups :: DescribePlacementGroupsResponse -> Lude.Maybe [PlacementGroup]) (\s a -> s {placementGroups = a} :: DescribePlacementGroupsResponse)
{-# DEPRECATED dpgrsPlacementGroups "Use generic-lens or generic-optics with 'placementGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrsResponseStatus :: Lens.Lens' DescribePlacementGroupsResponse Lude.Int
dpgrsResponseStatus = Lens.lens (responseStatus :: DescribePlacementGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePlacementGroupsResponse)
{-# DEPRECATED dpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
