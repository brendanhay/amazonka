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
-- Module      : Amazonka.EC2.DescribePlacementGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified placement groups or all of your placement
-- groups. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.DescribePlacementGroups
  ( -- * Creating a Request
    DescribePlacementGroups (..),
    newDescribePlacementGroups,

    -- * Request Lenses
    describePlacementGroups_filters,
    describePlacementGroups_dryRun,
    describePlacementGroups_groupIds,
    describePlacementGroups_groupNames,

    -- * Destructuring the Response
    DescribePlacementGroupsResponse (..),
    newDescribePlacementGroupsResponse,

    -- * Response Lenses
    describePlacementGroupsResponse_placementGroups,
    describePlacementGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePlacementGroups' smart constructor.
data DescribePlacementGroups = DescribePlacementGroups'
  { -- | The filters.
    --
    -- -   @group-name@ - The name of the placement group.
    --
    -- -   @group-arn@ - The Amazon Resource Name (ARN) of the placement group.
    --
    -- -   @spread-level@ - The spread level for the placement group (@host@ |
    --     @rack@).
    --
    -- -   @state@ - The state of the placement group (@pending@ | @available@
    --     | @deleting@ | @deleted@).
    --
    -- -   @strategy@ - The strategy of the placement group (@cluster@ |
    --     @spread@ | @partition@).
    --
    -- -   @tag:\<key>@ - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources that have a tag with a specific key,
    --     regardless of the tag value.
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the placement groups.
    groupIds :: Prelude.Maybe [Prelude.Text],
    -- | The names of the placement groups.
    --
    -- Default: Describes all your placement groups, or only those otherwise
    -- specified.
    groupNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePlacementGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describePlacementGroups_filters' - The filters.
--
-- -   @group-name@ - The name of the placement group.
--
-- -   @group-arn@ - The Amazon Resource Name (ARN) of the placement group.
--
-- -   @spread-level@ - The spread level for the placement group (@host@ |
--     @rack@).
--
-- -   @state@ - The state of the placement group (@pending@ | @available@
--     | @deleting@ | @deleted@).
--
-- -   @strategy@ - The strategy of the placement group (@cluster@ |
--     @spread@ | @partition@).
--
-- -   @tag:\<key>@ - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources that have a tag with a specific key,
--     regardless of the tag value.
--
-- 'dryRun', 'describePlacementGroups_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groupIds', 'describePlacementGroups_groupIds' - The IDs of the placement groups.
--
-- 'groupNames', 'describePlacementGroups_groupNames' - The names of the placement groups.
--
-- Default: Describes all your placement groups, or only those otherwise
-- specified.
newDescribePlacementGroups ::
  DescribePlacementGroups
newDescribePlacementGroups =
  DescribePlacementGroups'
    { filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      groupIds = Prelude.Nothing,
      groupNames = Prelude.Nothing
    }

-- | The filters.
--
-- -   @group-name@ - The name of the placement group.
--
-- -   @group-arn@ - The Amazon Resource Name (ARN) of the placement group.
--
-- -   @spread-level@ - The spread level for the placement group (@host@ |
--     @rack@).
--
-- -   @state@ - The state of the placement group (@pending@ | @available@
--     | @deleting@ | @deleted@).
--
-- -   @strategy@ - The strategy of the placement group (@cluster@ |
--     @spread@ | @partition@).
--
-- -   @tag:\<key>@ - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources that have a tag with a specific key,
--     regardless of the tag value.
describePlacementGroups_filters :: Lens.Lens' DescribePlacementGroups (Prelude.Maybe [Filter])
describePlacementGroups_filters = Lens.lens (\DescribePlacementGroups' {filters} -> filters) (\s@DescribePlacementGroups' {} a -> s {filters = a} :: DescribePlacementGroups) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describePlacementGroups_dryRun :: Lens.Lens' DescribePlacementGroups (Prelude.Maybe Prelude.Bool)
describePlacementGroups_dryRun = Lens.lens (\DescribePlacementGroups' {dryRun} -> dryRun) (\s@DescribePlacementGroups' {} a -> s {dryRun = a} :: DescribePlacementGroups)

-- | The IDs of the placement groups.
describePlacementGroups_groupIds :: Lens.Lens' DescribePlacementGroups (Prelude.Maybe [Prelude.Text])
describePlacementGroups_groupIds = Lens.lens (\DescribePlacementGroups' {groupIds} -> groupIds) (\s@DescribePlacementGroups' {} a -> s {groupIds = a} :: DescribePlacementGroups) Prelude.. Lens.mapping Lens.coerced

-- | The names of the placement groups.
--
-- Default: Describes all your placement groups, or only those otherwise
-- specified.
describePlacementGroups_groupNames :: Lens.Lens' DescribePlacementGroups (Prelude.Maybe [Prelude.Text])
describePlacementGroups_groupNames = Lens.lens (\DescribePlacementGroups' {groupNames} -> groupNames) (\s@DescribePlacementGroups' {} a -> s {groupNames = a} :: DescribePlacementGroups) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribePlacementGroups where
  type
    AWSResponse DescribePlacementGroups =
      DescribePlacementGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribePlacementGroupsResponse'
            Prelude.<$> ( x Data..@? "placementGroupSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePlacementGroups where
  hashWithSalt _salt DescribePlacementGroups' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` groupIds
      `Prelude.hashWithSalt` groupNames

instance Prelude.NFData DescribePlacementGroups where
  rnf DescribePlacementGroups' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf groupIds
      `Prelude.seq` Prelude.rnf groupNames

instance Data.ToHeaders DescribePlacementGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribePlacementGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePlacementGroups where
  toQuery DescribePlacementGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribePlacementGroups" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "GroupId" Prelude.<$> groupIds),
        Data.toQuery
          ( Data.toQueryList "GroupName"
              Prelude.<$> groupNames
          )
      ]

-- | /See:/ 'newDescribePlacementGroupsResponse' smart constructor.
data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse'
  { -- | Information about the placement groups.
    placementGroups :: Prelude.Maybe [PlacementGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
describePlacementGroupsResponse_placementGroups = Lens.lens (\DescribePlacementGroupsResponse' {placementGroups} -> placementGroups) (\s@DescribePlacementGroupsResponse' {} a -> s {placementGroups = a} :: DescribePlacementGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePlacementGroupsResponse_httpStatus :: Lens.Lens' DescribePlacementGroupsResponse Prelude.Int
describePlacementGroupsResponse_httpStatus = Lens.lens (\DescribePlacementGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribePlacementGroupsResponse' {} a -> s {httpStatus = a} :: DescribePlacementGroupsResponse)

instance
  Prelude.NFData
    DescribePlacementGroupsResponse
  where
  rnf DescribePlacementGroupsResponse' {..} =
    Prelude.rnf placementGroups
      `Prelude.seq` Prelude.rnf httpStatus
