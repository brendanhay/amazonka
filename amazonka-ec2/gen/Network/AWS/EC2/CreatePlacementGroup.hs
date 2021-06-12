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
-- Module      : Network.AWS.EC2.CreatePlacementGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a placement group in which to launch instances. The strategy of
-- the placement group determines how the instances are organized within
-- the group.
--
-- A @cluster@ placement group is a logical grouping of instances within a
-- single Availability Zone that benefit from low network latency, high
-- network throughput. A @spread@ placement group places instances on
-- distinct hardware. A @partition@ placement group places groups of
-- instances in different partitions, where instances in one partition do
-- not share the same hardware with instances in another partition.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.CreatePlacementGroup
  ( -- * Creating a Request
    CreatePlacementGroup (..),
    newCreatePlacementGroup,

    -- * Request Lenses
    createPlacementGroup_tagSpecifications,
    createPlacementGroup_dryRun,
    createPlacementGroup_strategy,
    createPlacementGroup_groupName,
    createPlacementGroup_partitionCount,

    -- * Destructuring the Response
    CreatePlacementGroupResponse (..),
    newCreatePlacementGroupResponse,

    -- * Response Lenses
    createPlacementGroupResponse_placementGroup,
    createPlacementGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePlacementGroup' smart constructor.
data CreatePlacementGroup = CreatePlacementGroup'
  { -- | The tags to apply to the new placement group.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The placement strategy.
    strategy :: Core.Maybe PlacementStrategy,
    -- | A name for the placement group. Must be unique within the scope of your
    -- account for the Region.
    --
    -- Constraints: Up to 255 ASCII characters
    groupName :: Core.Maybe Core.Text,
    -- | The number of partitions. Valid only when __Strategy__ is set to
    -- @partition@.
    partitionCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePlacementGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createPlacementGroup_tagSpecifications' - The tags to apply to the new placement group.
--
-- 'dryRun', 'createPlacementGroup_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'strategy', 'createPlacementGroup_strategy' - The placement strategy.
--
-- 'groupName', 'createPlacementGroup_groupName' - A name for the placement group. Must be unique within the scope of your
-- account for the Region.
--
-- Constraints: Up to 255 ASCII characters
--
-- 'partitionCount', 'createPlacementGroup_partitionCount' - The number of partitions. Valid only when __Strategy__ is set to
-- @partition@.
newCreatePlacementGroup ::
  CreatePlacementGroup
newCreatePlacementGroup =
  CreatePlacementGroup'
    { tagSpecifications =
        Core.Nothing,
      dryRun = Core.Nothing,
      strategy = Core.Nothing,
      groupName = Core.Nothing,
      partitionCount = Core.Nothing
    }

-- | The tags to apply to the new placement group.
createPlacementGroup_tagSpecifications :: Lens.Lens' CreatePlacementGroup (Core.Maybe [TagSpecification])
createPlacementGroup_tagSpecifications = Lens.lens (\CreatePlacementGroup' {tagSpecifications} -> tagSpecifications) (\s@CreatePlacementGroup' {} a -> s {tagSpecifications = a} :: CreatePlacementGroup) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createPlacementGroup_dryRun :: Lens.Lens' CreatePlacementGroup (Core.Maybe Core.Bool)
createPlacementGroup_dryRun = Lens.lens (\CreatePlacementGroup' {dryRun} -> dryRun) (\s@CreatePlacementGroup' {} a -> s {dryRun = a} :: CreatePlacementGroup)

-- | The placement strategy.
createPlacementGroup_strategy :: Lens.Lens' CreatePlacementGroup (Core.Maybe PlacementStrategy)
createPlacementGroup_strategy = Lens.lens (\CreatePlacementGroup' {strategy} -> strategy) (\s@CreatePlacementGroup' {} a -> s {strategy = a} :: CreatePlacementGroup)

-- | A name for the placement group. Must be unique within the scope of your
-- account for the Region.
--
-- Constraints: Up to 255 ASCII characters
createPlacementGroup_groupName :: Lens.Lens' CreatePlacementGroup (Core.Maybe Core.Text)
createPlacementGroup_groupName = Lens.lens (\CreatePlacementGroup' {groupName} -> groupName) (\s@CreatePlacementGroup' {} a -> s {groupName = a} :: CreatePlacementGroup)

-- | The number of partitions. Valid only when __Strategy__ is set to
-- @partition@.
createPlacementGroup_partitionCount :: Lens.Lens' CreatePlacementGroup (Core.Maybe Core.Int)
createPlacementGroup_partitionCount = Lens.lens (\CreatePlacementGroup' {partitionCount} -> partitionCount) (\s@CreatePlacementGroup' {} a -> s {partitionCount = a} :: CreatePlacementGroup)

instance Core.AWSRequest CreatePlacementGroup where
  type
    AWSResponse CreatePlacementGroup =
      CreatePlacementGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreatePlacementGroupResponse'
            Core.<$> (x Core..@? "placementGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePlacementGroup

instance Core.NFData CreatePlacementGroup

instance Core.ToHeaders CreatePlacementGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreatePlacementGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreatePlacementGroup where
  toQuery CreatePlacementGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreatePlacementGroup" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "Strategy" Core.=: strategy,
        "GroupName" Core.=: groupName,
        "PartitionCount" Core.=: partitionCount
      ]

-- | /See:/ 'newCreatePlacementGroupResponse' smart constructor.
data CreatePlacementGroupResponse = CreatePlacementGroupResponse'
  { placementGroup :: Core.Maybe PlacementGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePlacementGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placementGroup', 'createPlacementGroupResponse_placementGroup' - Undocumented member.
--
-- 'httpStatus', 'createPlacementGroupResponse_httpStatus' - The response's http status code.
newCreatePlacementGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePlacementGroupResponse
newCreatePlacementGroupResponse pHttpStatus_ =
  CreatePlacementGroupResponse'
    { placementGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createPlacementGroupResponse_placementGroup :: Lens.Lens' CreatePlacementGroupResponse (Core.Maybe PlacementGroup)
createPlacementGroupResponse_placementGroup = Lens.lens (\CreatePlacementGroupResponse' {placementGroup} -> placementGroup) (\s@CreatePlacementGroupResponse' {} a -> s {placementGroup = a} :: CreatePlacementGroupResponse)

-- | The response's http status code.
createPlacementGroupResponse_httpStatus :: Lens.Lens' CreatePlacementGroupResponse Core.Int
createPlacementGroupResponse_httpStatus = Lens.lens (\CreatePlacementGroupResponse' {httpStatus} -> httpStatus) (\s@CreatePlacementGroupResponse' {} a -> s {httpStatus = a} :: CreatePlacementGroupResponse)

instance Core.NFData CreatePlacementGroupResponse
