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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRouteTable' smart constructor.
data CreateRouteTable = CreateRouteTable'
  { -- | The tags to assign to the route table.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateRouteTable
newCreateRouteTable pVpcId_ =
  CreateRouteTable'
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | The tags to assign to the route table.
createRouteTable_tagSpecifications :: Lens.Lens' CreateRouteTable (Prelude.Maybe [TagSpecification])
createRouteTable_tagSpecifications = Lens.lens (\CreateRouteTable' {tagSpecifications} -> tagSpecifications) (\s@CreateRouteTable' {} a -> s {tagSpecifications = a} :: CreateRouteTable) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createRouteTable_dryRun :: Lens.Lens' CreateRouteTable (Prelude.Maybe Prelude.Bool)
createRouteTable_dryRun = Lens.lens (\CreateRouteTable' {dryRun} -> dryRun) (\s@CreateRouteTable' {} a -> s {dryRun = a} :: CreateRouteTable)

-- | The ID of the VPC.
createRouteTable_vpcId :: Lens.Lens' CreateRouteTable Prelude.Text
createRouteTable_vpcId = Lens.lens (\CreateRouteTable' {vpcId} -> vpcId) (\s@CreateRouteTable' {} a -> s {vpcId = a} :: CreateRouteTable)

instance Prelude.AWSRequest CreateRouteTable where
  type Rs CreateRouteTable = CreateRouteTableResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateRouteTableResponse'
            Prelude.<$> (x Prelude..@? "routeTable")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRouteTable

instance Prelude.NFData CreateRouteTable

instance Prelude.ToHeaders CreateRouteTable where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateRouteTable where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateRouteTable where
  toQuery CreateRouteTable' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateRouteTable" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Prelude.=: dryRun,
        "VpcId" Prelude.=: vpcId
      ]

-- | /See:/ 'newCreateRouteTableResponse' smart constructor.
data CreateRouteTableResponse = CreateRouteTableResponse'
  { -- | Information about the route table.
    routeTable :: Prelude.Maybe RouteTable,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateRouteTableResponse
newCreateRouteTableResponse pHttpStatus_ =
  CreateRouteTableResponse'
    { routeTable =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the route table.
createRouteTableResponse_routeTable :: Lens.Lens' CreateRouteTableResponse (Prelude.Maybe RouteTable)
createRouteTableResponse_routeTable = Lens.lens (\CreateRouteTableResponse' {routeTable} -> routeTable) (\s@CreateRouteTableResponse' {} a -> s {routeTable = a} :: CreateRouteTableResponse)

-- | The response's http status code.
createRouteTableResponse_httpStatus :: Lens.Lens' CreateRouteTableResponse Prelude.Int
createRouteTableResponse_httpStatus = Lens.lens (\CreateRouteTableResponse' {httpStatus} -> httpStatus) (\s@CreateRouteTableResponse' {} a -> s {httpStatus = a} :: CreateRouteTableResponse)

instance Prelude.NFData CreateRouteTableResponse
