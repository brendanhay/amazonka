{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePlacementGroups' smart constructor.
data DescribePlacementGroups = DescribePlacementGroups'
  { -- | The IDs of the placement groups.
    groupIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The names of the placement groups.
    --
    -- Default: Describes all your placement groups, or only those otherwise
    -- specified.
    groupNames :: Prelude.Maybe [Prelude.Text],
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
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { groupIds =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      groupNames = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The IDs of the placement groups.
describePlacementGroups_groupIds :: Lens.Lens' DescribePlacementGroups (Prelude.Maybe [Prelude.Text])
describePlacementGroups_groupIds = Lens.lens (\DescribePlacementGroups' {groupIds} -> groupIds) (\s@DescribePlacementGroups' {} a -> s {groupIds = a} :: DescribePlacementGroups) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describePlacementGroups_dryRun :: Lens.Lens' DescribePlacementGroups (Prelude.Maybe Prelude.Bool)
describePlacementGroups_dryRun = Lens.lens (\DescribePlacementGroups' {dryRun} -> dryRun) (\s@DescribePlacementGroups' {} a -> s {dryRun = a} :: DescribePlacementGroups)

-- | The names of the placement groups.
--
-- Default: Describes all your placement groups, or only those otherwise
-- specified.
describePlacementGroups_groupNames :: Lens.Lens' DescribePlacementGroups (Prelude.Maybe [Prelude.Text])
describePlacementGroups_groupNames = Lens.lens (\DescribePlacementGroups' {groupNames} -> groupNames) (\s@DescribePlacementGroups' {} a -> s {groupNames = a} :: DescribePlacementGroups) Prelude.. Lens.mapping Prelude._Coerce

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
describePlacementGroups_filters :: Lens.Lens' DescribePlacementGroups (Prelude.Maybe [Filter])
describePlacementGroups_filters = Lens.lens (\DescribePlacementGroups' {filters} -> filters) (\s@DescribePlacementGroups' {} a -> s {filters = a} :: DescribePlacementGroups) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest DescribePlacementGroups where
  type
    Rs DescribePlacementGroups =
      DescribePlacementGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribePlacementGroupsResponse'
            Prelude.<$> ( x Prelude..@? "placementGroupSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePlacementGroups

instance Prelude.NFData DescribePlacementGroups

instance Prelude.ToHeaders DescribePlacementGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribePlacementGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribePlacementGroups where
  toQuery DescribePlacementGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribePlacementGroups" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          (Prelude.toQueryList "GroupId" Prelude.<$> groupIds),
        "DryRun" Prelude.=: dryRun,
        Prelude.toQuery
          ( Prelude.toQueryList "GroupName"
              Prelude.<$> groupNames
          ),
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribePlacementGroupsResponse' smart constructor.
data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse'
  { -- | Information about the placement groups.
    placementGroups :: Prelude.Maybe [PlacementGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribePlacementGroupsResponse
newDescribePlacementGroupsResponse pHttpStatus_ =
  DescribePlacementGroupsResponse'
    { placementGroups =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the placement groups.
describePlacementGroupsResponse_placementGroups :: Lens.Lens' DescribePlacementGroupsResponse (Prelude.Maybe [PlacementGroup])
describePlacementGroupsResponse_placementGroups = Lens.lens (\DescribePlacementGroupsResponse' {placementGroups} -> placementGroups) (\s@DescribePlacementGroupsResponse' {} a -> s {placementGroups = a} :: DescribePlacementGroupsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describePlacementGroupsResponse_httpStatus :: Lens.Lens' DescribePlacementGroupsResponse Prelude.Int
describePlacementGroupsResponse_httpStatus = Lens.lens (\DescribePlacementGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribePlacementGroupsResponse' {} a -> s {httpStatus = a} :: DescribePlacementGroupsResponse)

instance
  Prelude.NFData
    DescribePlacementGroupsResponse
