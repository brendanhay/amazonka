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
-- Module      : Network.AWS.EC2.CreateRouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route table for the specified VPC. After you create a route
-- table, you can add routes and associate the table with a subnet.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.CreateRouteTable
  ( -- * Creating a Request
    CreateRouteTable (..),
    newCreateRouteTable,

    -- * Request Lenses
    createRouteTable_tagSpecifications,
    createRouteTable_dryRun,
    createRouteTable_vpcId,

    -- * Destructuring the Response
    CreateRouteTableResponse (..),
    newCreateRouteTableResponse,

    -- * Response Lenses
    createRouteTableResponse_routeTable,
    createRouteTableResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRouteTable' smart constructor.
data CreateRouteTable = CreateRouteTable'
  { -- | The tags to assign to the route table.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the VPC.
    vpcId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createRouteTable_tagSpecifications' - The tags to assign to the route table.
--
-- 'dryRun', 'createRouteTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcId', 'createRouteTable_vpcId' - The ID of the VPC.
newCreateRouteTable ::
  -- | 'vpcId'
  Core.Text ->
  CreateRouteTable
newCreateRouteTable pVpcId_ =
  CreateRouteTable'
    { tagSpecifications = Core.Nothing,
      dryRun = Core.Nothing,
      vpcId = pVpcId_
    }

-- | The tags to assign to the route table.
createRouteTable_tagSpecifications :: Lens.Lens' CreateRouteTable (Core.Maybe [TagSpecification])
createRouteTable_tagSpecifications = Lens.lens (\CreateRouteTable' {tagSpecifications} -> tagSpecifications) (\s@CreateRouteTable' {} a -> s {tagSpecifications = a} :: CreateRouteTable) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createRouteTable_dryRun :: Lens.Lens' CreateRouteTable (Core.Maybe Core.Bool)
createRouteTable_dryRun = Lens.lens (\CreateRouteTable' {dryRun} -> dryRun) (\s@CreateRouteTable' {} a -> s {dryRun = a} :: CreateRouteTable)

-- | The ID of the VPC.
createRouteTable_vpcId :: Lens.Lens' CreateRouteTable Core.Text
createRouteTable_vpcId = Lens.lens (\CreateRouteTable' {vpcId} -> vpcId) (\s@CreateRouteTable' {} a -> s {vpcId = a} :: CreateRouteTable)

instance Core.AWSRequest CreateRouteTable where
  type
    AWSResponse CreateRouteTable =
      CreateRouteTableResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateRouteTableResponse'
            Core.<$> (x Core..@? "routeTable")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateRouteTable

instance Core.NFData CreateRouteTable

instance Core.ToHeaders CreateRouteTable where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateRouteTable where
  toPath = Core.const "/"

instance Core.ToQuery CreateRouteTable where
  toQuery CreateRouteTable' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateRouteTable" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newCreateRouteTableResponse' smart constructor.
data CreateRouteTableResponse = CreateRouteTableResponse'
  { -- | Information about the route table.
    routeTable :: Core.Maybe RouteTable,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRouteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routeTable', 'createRouteTableResponse_routeTable' - Information about the route table.
--
-- 'httpStatus', 'createRouteTableResponse_httpStatus' - The response's http status code.
newCreateRouteTableResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateRouteTableResponse
newCreateRouteTableResponse pHttpStatus_ =
  CreateRouteTableResponse'
    { routeTable =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the route table.
createRouteTableResponse_routeTable :: Lens.Lens' CreateRouteTableResponse (Core.Maybe RouteTable)
createRouteTableResponse_routeTable = Lens.lens (\CreateRouteTableResponse' {routeTable} -> routeTable) (\s@CreateRouteTableResponse' {} a -> s {routeTable = a} :: CreateRouteTableResponse)

-- | The response's http status code.
createRouteTableResponse_httpStatus :: Lens.Lens' CreateRouteTableResponse Core.Int
createRouteTableResponse_httpStatus = Lens.lens (\CreateRouteTableResponse' {httpStatus} -> httpStatus) (\s@CreateRouteTableResponse' {} a -> s {httpStatus = a} :: CreateRouteTableResponse)

instance Core.NFData CreateRouteTableResponse
