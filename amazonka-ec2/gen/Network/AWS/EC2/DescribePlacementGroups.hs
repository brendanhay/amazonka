{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribePlacementGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified placement groups or all of your placement
-- groups. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.DescribePlacementGroups
  ( -- * Creating a Request
    DescribePlacementGroups (..),
    newDescribePlacementGroups,

    -- * Request Lenses
    describePlacementGroups_groupIds,
    describePlacementGroups_dryRun,
    describePlacementGroups_groupNames,
    describePlacementGroups_filters,

    -- * Destructuring the Response
    DescribePlacementGroupsResponse (..),
    newDescribePlacementGroupsResponse,

    -- * Response Lenses
    describePlacementGroupsResponse_placementGroups,
    describePlacementGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePlacementGroups' smart constructor.
data DescribePlacementGroups = DescribePlacementGroups'
  { -- | The IDs of the placement groups.
    groupIds :: Core.Maybe [Core.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The names of the placement groups.
    --
    -- Default: Describes all your placement groups, or only those otherwise
    -- specified.
    groupNames :: Core.Maybe [Core.Text],
    -- | The filters.
    --
    -- -   @group-name@ - The name of the placement group.
    --
    -- -   @state@ - The state of the placement group (@pending@ | @available@
    --     | @deleting@ | @deleted@).
    --
    -- -   @strategy@ - The strategy of the placement group (@cluster@ |
    --     @spread@ | @partition@).
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources that have a tag with a specific key,
    --     regardless of the tag value.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePlacementGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupIds', 'describePlacementGroups_groupIds' - The IDs of the placement groups.
--
-- 'dryRun', 'describePlacementGroups_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groupNames', 'describePlacementGroups_groupNames' - The names of the placement groups.
--
-- Default: Describes all your placement groups, or only those otherwise
-- specified.
--
-- 'filters', 'describePlacementGroups_filters' - The filters.
--
-- -   @group-name@ - The name of the placement group.
--
-- -   @state@ - The state of the placement group (@pending@ | @available@
--     | @deleting@ | @deleted@).
--
-- -   @strategy@ - The strategy of the placement group (@cluster@ |
--     @spread@ | @partition@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources that have a tag with a specific key,
--     regardless of the tag value.
newDescribePlacementGroups ::
  DescribePlacementGroups
newDescribePlacementGroups =
  DescribePlacementGroups'
    { groupIds = Core.Nothing,
      dryRun = Core.Nothing,
      groupNames = Core.Nothing,
      filters = Core.Nothing
    }

-- | The IDs of the placement groups.
describePlacementGroups_groupIds :: Lens.Lens' DescribePlacementGroups (Core.Maybe [Core.Text])
describePlacementGroups_groupIds = Lens.lens (\DescribePlacementGroups' {groupIds} -> groupIds) (\s@DescribePlacementGroups' {} a -> s {groupIds = a} :: DescribePlacementGroups) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describePlacementGroups_dryRun :: Lens.Lens' DescribePlacementGroups (Core.Maybe Core.Bool)
describePlacementGroups_dryRun = Lens.lens (\DescribePlacementGroups' {dryRun} -> dryRun) (\s@DescribePlacementGroups' {} a -> s {dryRun = a} :: DescribePlacementGroups)

-- | The names of the placement groups.
--
-- Default: Describes all your placement groups, or only those otherwise
-- specified.
describePlacementGroups_groupNames :: Lens.Lens' DescribePlacementGroups (Core.Maybe [Core.Text])
describePlacementGroups_groupNames = Lens.lens (\DescribePlacementGroups' {groupNames} -> groupNames) (\s@DescribePlacementGroups' {} a -> s {groupNames = a} :: DescribePlacementGroups) Core.. Lens.mapping Lens._Coerce

-- | The filters.
--
-- -   @group-name@ - The name of the placement group.
--
-- -   @state@ - The state of the placement group (@pending@ | @available@
--     | @deleting@ | @deleted@).
--
-- -   @strategy@ - The strategy of the placement group (@cluster@ |
--     @spread@ | @partition@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources that have a tag with a specific key,
--     regardless of the tag value.
describePlacementGroups_filters :: Lens.Lens' DescribePlacementGroups (Core.Maybe [Filter])
describePlacementGroups_filters = Lens.lens (\DescribePlacementGroups' {filters} -> filters) (\s@DescribePlacementGroups' {} a -> s {filters = a} :: DescribePlacementGroups) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribePlacementGroups where
  type
    AWSResponse DescribePlacementGroups =
      DescribePlacementGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribePlacementGroupsResponse'
            Core.<$> ( x Core..@? "placementGroupSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePlacementGroups

instance Core.NFData DescribePlacementGroups

instance Core.ToHeaders DescribePlacementGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribePlacementGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribePlacementGroups where
  toQuery DescribePlacementGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribePlacementGroups" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          (Core.toQueryList "GroupId" Core.<$> groupIds),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          (Core.toQueryList "GroupName" Core.<$> groupNames),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribePlacementGroupsResponse' smart constructor.
data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse'
  { -- | Information about the placement groups.
    placementGroups :: Core.Maybe [PlacementGroup],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePlacementGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placementGroups', 'describePlacementGroupsResponse_placementGroups' - Information about the placement groups.
--
-- 'httpStatus', 'describePlacementGroupsResponse_httpStatus' - The response's http status code.
newDescribePlacementGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePlacementGroupsResponse
newDescribePlacementGroupsResponse pHttpStatus_ =
  DescribePlacementGroupsResponse'
    { placementGroups =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the placement groups.
describePlacementGroupsResponse_placementGroups :: Lens.Lens' DescribePlacementGroupsResponse (Core.Maybe [PlacementGroup])
describePlacementGroupsResponse_placementGroups = Lens.lens (\DescribePlacementGroupsResponse' {placementGroups} -> placementGroups) (\s@DescribePlacementGroupsResponse' {} a -> s {placementGroups = a} :: DescribePlacementGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describePlacementGroupsResponse_httpStatus :: Lens.Lens' DescribePlacementGroupsResponse Core.Int
describePlacementGroupsResponse_httpStatus = Lens.lens (\DescribePlacementGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribePlacementGroupsResponse' {} a -> s {httpStatus = a} :: DescribePlacementGroupsResponse)

instance Core.NFData DescribePlacementGroupsResponse
