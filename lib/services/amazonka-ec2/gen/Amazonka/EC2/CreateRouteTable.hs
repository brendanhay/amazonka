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
-- Module      : Amazonka.EC2.CreateRouteTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route table for the specified VPC. After you create a route
-- table, you can add routes and associate the table with a subnet.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.CreateRouteTable
  ( -- * Creating a Request
    CreateRouteTable (..),
    newCreateRouteTable,

    -- * Request Lenses
    createRouteTable_dryRun,
    createRouteTable_tagSpecifications,
    createRouteTable_vpcId,

    -- * Destructuring the Response
    CreateRouteTableResponse (..),
    newCreateRouteTableResponse,

    -- * Response Lenses
    createRouteTableResponse_routeTable,
    createRouteTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRouteTable' smart constructor.
data CreateRouteTable = CreateRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to assign to the route table.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createRouteTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createRouteTable_tagSpecifications' - The tags to assign to the route table.
--
-- 'vpcId', 'createRouteTable_vpcId' - The ID of the VPC.
newCreateRouteTable ::
  -- | 'vpcId'
  Prelude.Text ->
  CreateRouteTable
newCreateRouteTable pVpcId_ =
  CreateRouteTable'
    { dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createRouteTable_dryRun :: Lens.Lens' CreateRouteTable (Prelude.Maybe Prelude.Bool)
createRouteTable_dryRun = Lens.lens (\CreateRouteTable' {dryRun} -> dryRun) (\s@CreateRouteTable' {} a -> s {dryRun = a} :: CreateRouteTable)

-- | The tags to assign to the route table.
createRouteTable_tagSpecifications :: Lens.Lens' CreateRouteTable (Prelude.Maybe [TagSpecification])
createRouteTable_tagSpecifications = Lens.lens (\CreateRouteTable' {tagSpecifications} -> tagSpecifications) (\s@CreateRouteTable' {} a -> s {tagSpecifications = a} :: CreateRouteTable) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
createRouteTable_vpcId :: Lens.Lens' CreateRouteTable Prelude.Text
createRouteTable_vpcId = Lens.lens (\CreateRouteTable' {vpcId} -> vpcId) (\s@CreateRouteTable' {} a -> s {vpcId = a} :: CreateRouteTable)

instance Core.AWSRequest CreateRouteTable where
  type
    AWSResponse CreateRouteTable =
      CreateRouteTableResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateRouteTableResponse'
            Prelude.<$> (x Data..@? "routeTable")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRouteTable where
  hashWithSalt _salt CreateRouteTable' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData CreateRouteTable where
  rnf CreateRouteTable' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf tagSpecifications `Prelude.seq`
        Prelude.rnf vpcId

instance Data.ToHeaders CreateRouteTable where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateRouteTable where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateRouteTable where
  toQuery CreateRouteTable' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateRouteTable" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "VpcId" Data.=: vpcId
      ]

-- | /See:/ 'newCreateRouteTableResponse' smart constructor.
data CreateRouteTableResponse = CreateRouteTableResponse'
  { -- | Information about the route table.
    routeTable :: Prelude.Maybe RouteTable,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateRouteTableResponse where
  rnf CreateRouteTableResponse' {..} =
    Prelude.rnf routeTable `Prelude.seq`
      Prelude.rnf httpStatus
