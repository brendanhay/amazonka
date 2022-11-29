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
-- Module      : Amazonka.EC2.CreateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route table for the specified transit gateway.
module Amazonka.EC2.CreateTransitGatewayRouteTable
  ( -- * Creating a Request
    CreateTransitGatewayRouteTable (..),
    newCreateTransitGatewayRouteTable,

    -- * Request Lenses
    createTransitGatewayRouteTable_dryRun,
    createTransitGatewayRouteTable_tagSpecifications,
    createTransitGatewayRouteTable_transitGatewayId,

    -- * Destructuring the Response
    CreateTransitGatewayRouteTableResponse (..),
    newCreateTransitGatewayRouteTableResponse,

    -- * Response Lenses
    createTransitGatewayRouteTableResponse_transitGatewayRouteTable,
    createTransitGatewayRouteTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTransitGatewayRouteTable' smart constructor.
data CreateTransitGatewayRouteTable = CreateTransitGatewayRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the transit gateway route table.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createTransitGatewayRouteTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createTransitGatewayRouteTable_tagSpecifications' - The tags to apply to the transit gateway route table.
--
-- 'transitGatewayId', 'createTransitGatewayRouteTable_transitGatewayId' - The ID of the transit gateway.
newCreateTransitGatewayRouteTable ::
  -- | 'transitGatewayId'
  Prelude.Text ->
  CreateTransitGatewayRouteTable
newCreateTransitGatewayRouteTable pTransitGatewayId_ =
  CreateTransitGatewayRouteTable'
    { dryRun =
        Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      transitGatewayId = pTransitGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayRouteTable_dryRun :: Lens.Lens' CreateTransitGatewayRouteTable (Prelude.Maybe Prelude.Bool)
createTransitGatewayRouteTable_dryRun = Lens.lens (\CreateTransitGatewayRouteTable' {dryRun} -> dryRun) (\s@CreateTransitGatewayRouteTable' {} a -> s {dryRun = a} :: CreateTransitGatewayRouteTable)

-- | The tags to apply to the transit gateway route table.
createTransitGatewayRouteTable_tagSpecifications :: Lens.Lens' CreateTransitGatewayRouteTable (Prelude.Maybe [TagSpecification])
createTransitGatewayRouteTable_tagSpecifications = Lens.lens (\CreateTransitGatewayRouteTable' {tagSpecifications} -> tagSpecifications) (\s@CreateTransitGatewayRouteTable' {} a -> s {tagSpecifications = a} :: CreateTransitGatewayRouteTable) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway.
createTransitGatewayRouteTable_transitGatewayId :: Lens.Lens' CreateTransitGatewayRouteTable Prelude.Text
createTransitGatewayRouteTable_transitGatewayId = Lens.lens (\CreateTransitGatewayRouteTable' {transitGatewayId} -> transitGatewayId) (\s@CreateTransitGatewayRouteTable' {} a -> s {transitGatewayId = a} :: CreateTransitGatewayRouteTable)

instance
  Core.AWSRequest
    CreateTransitGatewayRouteTable
  where
  type
    AWSResponse CreateTransitGatewayRouteTable =
      CreateTransitGatewayRouteTableResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayRouteTableResponse'
            Prelude.<$> (x Core..@? "transitGatewayRouteTable")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTransitGatewayRouteTable
  where
  hashWithSalt
    _salt
    CreateTransitGatewayRouteTable' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` tagSpecifications
        `Prelude.hashWithSalt` transitGatewayId

instance
  Prelude.NFData
    CreateTransitGatewayRouteTable
  where
  rnf CreateTransitGatewayRouteTable' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf transitGatewayId

instance
  Core.ToHeaders
    CreateTransitGatewayRouteTable
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateTransitGatewayRouteTable where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTransitGatewayRouteTable where
  toQuery CreateTransitGatewayRouteTable' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateTransitGatewayRouteTable" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "TagSpecifications"
              Prelude.<$> tagSpecifications
          ),
        "TransitGatewayId" Core.=: transitGatewayId
      ]

-- | /See:/ 'newCreateTransitGatewayRouteTableResponse' smart constructor.
data CreateTransitGatewayRouteTableResponse = CreateTransitGatewayRouteTableResponse'
  { -- | Information about the transit gateway route table.
    transitGatewayRouteTable :: Prelude.Maybe TransitGatewayRouteTable,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayRouteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayRouteTable', 'createTransitGatewayRouteTableResponse_transitGatewayRouteTable' - Information about the transit gateway route table.
--
-- 'httpStatus', 'createTransitGatewayRouteTableResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayRouteTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitGatewayRouteTableResponse
newCreateTransitGatewayRouteTableResponse
  pHttpStatus_ =
    CreateTransitGatewayRouteTableResponse'
      { transitGatewayRouteTable =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the transit gateway route table.
createTransitGatewayRouteTableResponse_transitGatewayRouteTable :: Lens.Lens' CreateTransitGatewayRouteTableResponse (Prelude.Maybe TransitGatewayRouteTable)
createTransitGatewayRouteTableResponse_transitGatewayRouteTable = Lens.lens (\CreateTransitGatewayRouteTableResponse' {transitGatewayRouteTable} -> transitGatewayRouteTable) (\s@CreateTransitGatewayRouteTableResponse' {} a -> s {transitGatewayRouteTable = a} :: CreateTransitGatewayRouteTableResponse)

-- | The response's http status code.
createTransitGatewayRouteTableResponse_httpStatus :: Lens.Lens' CreateTransitGatewayRouteTableResponse Prelude.Int
createTransitGatewayRouteTableResponse_httpStatus = Lens.lens (\CreateTransitGatewayRouteTableResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayRouteTableResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayRouteTableResponse)

instance
  Prelude.NFData
    CreateTransitGatewayRouteTableResponse
  where
  rnf CreateTransitGatewayRouteTableResponse' {..} =
    Prelude.rnf transitGatewayRouteTable
      `Prelude.seq` Prelude.rnf httpStatus
