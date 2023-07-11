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
-- Module      : Amazonka.EC2.CreatePlacementGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.EC2.CreatePlacementGroup
  ( -- * Creating a Request
    CreatePlacementGroup (..),
    newCreatePlacementGroup,

    -- * Request Lenses
    createPlacementGroup_dryRun,
    createPlacementGroup_groupName,
    createPlacementGroup_partitionCount,
    createPlacementGroup_spreadLevel,
    createPlacementGroup_strategy,
    createPlacementGroup_tagSpecifications,

    -- * Destructuring the Response
    CreatePlacementGroupResponse (..),
    newCreatePlacementGroupResponse,

    -- * Response Lenses
    createPlacementGroupResponse_placementGroup,
    createPlacementGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePlacementGroup' smart constructor.
data CreatePlacementGroup = CreatePlacementGroup'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A name for the placement group. Must be unique within the scope of your
    -- account for the Region.
    --
    -- Constraints: Up to 255 ASCII characters
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The number of partitions. Valid only when __Strategy__ is set to
    -- @partition@.
    partitionCount :: Prelude.Maybe Prelude.Int,
    -- | Determines how placement groups spread instances.
    --
    -- -   Host – You can use @host@ only with Outpost placement groups.
    --
    -- -   Rack – No usage restrictions.
    spreadLevel :: Prelude.Maybe SpreadLevel,
    -- | The placement strategy.
    strategy :: Prelude.Maybe PlacementStrategy,
    -- | The tags to apply to the new placement group.
    tagSpecifications :: Prelude.Maybe [TagSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePlacementGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createPlacementGroup_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groupName', 'createPlacementGroup_groupName' - A name for the placement group. Must be unique within the scope of your
-- account for the Region.
--
-- Constraints: Up to 255 ASCII characters
--
-- 'partitionCount', 'createPlacementGroup_partitionCount' - The number of partitions. Valid only when __Strategy__ is set to
-- @partition@.
--
-- 'spreadLevel', 'createPlacementGroup_spreadLevel' - Determines how placement groups spread instances.
--
-- -   Host – You can use @host@ only with Outpost placement groups.
--
-- -   Rack – No usage restrictions.
--
-- 'strategy', 'createPlacementGroup_strategy' - The placement strategy.
--
-- 'tagSpecifications', 'createPlacementGroup_tagSpecifications' - The tags to apply to the new placement group.
newCreatePlacementGroup ::
  CreatePlacementGroup
newCreatePlacementGroup =
  CreatePlacementGroup'
    { dryRun = Prelude.Nothing,
      groupName = Prelude.Nothing,
      partitionCount = Prelude.Nothing,
      spreadLevel = Prelude.Nothing,
      strategy = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createPlacementGroup_dryRun :: Lens.Lens' CreatePlacementGroup (Prelude.Maybe Prelude.Bool)
createPlacementGroup_dryRun = Lens.lens (\CreatePlacementGroup' {dryRun} -> dryRun) (\s@CreatePlacementGroup' {} a -> s {dryRun = a} :: CreatePlacementGroup)

-- | A name for the placement group. Must be unique within the scope of your
-- account for the Region.
--
-- Constraints: Up to 255 ASCII characters
createPlacementGroup_groupName :: Lens.Lens' CreatePlacementGroup (Prelude.Maybe Prelude.Text)
createPlacementGroup_groupName = Lens.lens (\CreatePlacementGroup' {groupName} -> groupName) (\s@CreatePlacementGroup' {} a -> s {groupName = a} :: CreatePlacementGroup)

-- | The number of partitions. Valid only when __Strategy__ is set to
-- @partition@.
createPlacementGroup_partitionCount :: Lens.Lens' CreatePlacementGroup (Prelude.Maybe Prelude.Int)
createPlacementGroup_partitionCount = Lens.lens (\CreatePlacementGroup' {partitionCount} -> partitionCount) (\s@CreatePlacementGroup' {} a -> s {partitionCount = a} :: CreatePlacementGroup)

-- | Determines how placement groups spread instances.
--
-- -   Host – You can use @host@ only with Outpost placement groups.
--
-- -   Rack – No usage restrictions.
createPlacementGroup_spreadLevel :: Lens.Lens' CreatePlacementGroup (Prelude.Maybe SpreadLevel)
createPlacementGroup_spreadLevel = Lens.lens (\CreatePlacementGroup' {spreadLevel} -> spreadLevel) (\s@CreatePlacementGroup' {} a -> s {spreadLevel = a} :: CreatePlacementGroup)

-- | The placement strategy.
createPlacementGroup_strategy :: Lens.Lens' CreatePlacementGroup (Prelude.Maybe PlacementStrategy)
createPlacementGroup_strategy = Lens.lens (\CreatePlacementGroup' {strategy} -> strategy) (\s@CreatePlacementGroup' {} a -> s {strategy = a} :: CreatePlacementGroup)

-- | The tags to apply to the new placement group.
createPlacementGroup_tagSpecifications :: Lens.Lens' CreatePlacementGroup (Prelude.Maybe [TagSpecification])
createPlacementGroup_tagSpecifications = Lens.lens (\CreatePlacementGroup' {tagSpecifications} -> tagSpecifications) (\s@CreatePlacementGroup' {} a -> s {tagSpecifications = a} :: CreatePlacementGroup) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreatePlacementGroup where
  type
    AWSResponse CreatePlacementGroup =
      CreatePlacementGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreatePlacementGroupResponse'
            Prelude.<$> (x Data..@? "placementGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePlacementGroup where
  hashWithSalt _salt CreatePlacementGroup' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` partitionCount
      `Prelude.hashWithSalt` spreadLevel
      `Prelude.hashWithSalt` strategy
      `Prelude.hashWithSalt` tagSpecifications

instance Prelude.NFData CreatePlacementGroup where
  rnf CreatePlacementGroup' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf partitionCount
      `Prelude.seq` Prelude.rnf spreadLevel
      `Prelude.seq` Prelude.rnf strategy
      `Prelude.seq` Prelude.rnf tagSpecifications

instance Data.ToHeaders CreatePlacementGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreatePlacementGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePlacementGroup where
  toQuery CreatePlacementGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreatePlacementGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "GroupName" Data.=: groupName,
        "PartitionCount" Data.=: partitionCount,
        "SpreadLevel" Data.=: spreadLevel,
        "Strategy" Data.=: strategy,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          )
      ]

-- | /See:/ 'newCreatePlacementGroupResponse' smart constructor.
data CreatePlacementGroupResponse = CreatePlacementGroupResponse'
  { -- | Information about the placement group.
    placementGroup :: Prelude.Maybe PlacementGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePlacementGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placementGroup', 'createPlacementGroupResponse_placementGroup' - Information about the placement group.
--
-- 'httpStatus', 'createPlacementGroupResponse_httpStatus' - The response's http status code.
newCreatePlacementGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePlacementGroupResponse
newCreatePlacementGroupResponse pHttpStatus_ =
  CreatePlacementGroupResponse'
    { placementGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the placement group.
createPlacementGroupResponse_placementGroup :: Lens.Lens' CreatePlacementGroupResponse (Prelude.Maybe PlacementGroup)
createPlacementGroupResponse_placementGroup = Lens.lens (\CreatePlacementGroupResponse' {placementGroup} -> placementGroup) (\s@CreatePlacementGroupResponse' {} a -> s {placementGroup = a} :: CreatePlacementGroupResponse)

-- | The response's http status code.
createPlacementGroupResponse_httpStatus :: Lens.Lens' CreatePlacementGroupResponse Prelude.Int
createPlacementGroupResponse_httpStatus = Lens.lens (\CreatePlacementGroupResponse' {httpStatus} -> httpStatus) (\s@CreatePlacementGroupResponse' {} a -> s {httpStatus = a} :: CreatePlacementGroupResponse)

instance Prelude.NFData CreatePlacementGroupResponse where
  rnf CreatePlacementGroupResponse' {..} =
    Prelude.rnf placementGroup
      `Prelude.seq` Prelude.rnf httpStatus
