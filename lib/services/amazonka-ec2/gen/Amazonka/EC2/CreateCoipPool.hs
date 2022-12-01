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
-- Module      : Amazonka.EC2.CreateCoipPool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pool of customer-owned IP (CoIP) addresses.
module Amazonka.EC2.CreateCoipPool
  ( -- * Creating a Request
    CreateCoipPool (..),
    newCreateCoipPool,

    -- * Request Lenses
    createCoipPool_dryRun,
    createCoipPool_tagSpecifications,
    createCoipPool_localGatewayRouteTableId,

    -- * Destructuring the Response
    CreateCoipPoolResponse (..),
    newCreateCoipPoolResponse,

    -- * Response Lenses
    createCoipPoolResponse_coipPool,
    createCoipPoolResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCoipPool' smart constructor.
data CreateCoipPool = CreateCoipPool'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to assign to the CoIP address pool.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCoipPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createCoipPool_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createCoipPool_tagSpecifications' - The tags to assign to the CoIP address pool.
--
-- 'localGatewayRouteTableId', 'createCoipPool_localGatewayRouteTableId' - The ID of the local gateway route table.
newCreateCoipPool ::
  -- | 'localGatewayRouteTableId'
  Prelude.Text ->
  CreateCoipPool
newCreateCoipPool pLocalGatewayRouteTableId_ =
  CreateCoipPool'
    { dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      localGatewayRouteTableId =
        pLocalGatewayRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createCoipPool_dryRun :: Lens.Lens' CreateCoipPool (Prelude.Maybe Prelude.Bool)
createCoipPool_dryRun = Lens.lens (\CreateCoipPool' {dryRun} -> dryRun) (\s@CreateCoipPool' {} a -> s {dryRun = a} :: CreateCoipPool)

-- | The tags to assign to the CoIP address pool.
createCoipPool_tagSpecifications :: Lens.Lens' CreateCoipPool (Prelude.Maybe [TagSpecification])
createCoipPool_tagSpecifications = Lens.lens (\CreateCoipPool' {tagSpecifications} -> tagSpecifications) (\s@CreateCoipPool' {} a -> s {tagSpecifications = a} :: CreateCoipPool) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the local gateway route table.
createCoipPool_localGatewayRouteTableId :: Lens.Lens' CreateCoipPool Prelude.Text
createCoipPool_localGatewayRouteTableId = Lens.lens (\CreateCoipPool' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@CreateCoipPool' {} a -> s {localGatewayRouteTableId = a} :: CreateCoipPool)

instance Core.AWSRequest CreateCoipPool where
  type
    AWSResponse CreateCoipPool =
      CreateCoipPoolResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCoipPoolResponse'
            Prelude.<$> (x Core..@? "coipPool")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCoipPool where
  hashWithSalt _salt CreateCoipPool' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` localGatewayRouteTableId

instance Prelude.NFData CreateCoipPool where
  rnf CreateCoipPool' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf localGatewayRouteTableId

instance Core.ToHeaders CreateCoipPool where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateCoipPool where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCoipPool where
  toQuery CreateCoipPool' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateCoipPool" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "LocalGatewayRouteTableId"
          Core.=: localGatewayRouteTableId
      ]

-- | /See:/ 'newCreateCoipPoolResponse' smart constructor.
data CreateCoipPoolResponse = CreateCoipPoolResponse'
  { coipPool :: Prelude.Maybe CoipPool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCoipPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coipPool', 'createCoipPoolResponse_coipPool' - Undocumented member.
--
-- 'httpStatus', 'createCoipPoolResponse_httpStatus' - The response's http status code.
newCreateCoipPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCoipPoolResponse
newCreateCoipPoolResponse pHttpStatus_ =
  CreateCoipPoolResponse'
    { coipPool = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createCoipPoolResponse_coipPool :: Lens.Lens' CreateCoipPoolResponse (Prelude.Maybe CoipPool)
createCoipPoolResponse_coipPool = Lens.lens (\CreateCoipPoolResponse' {coipPool} -> coipPool) (\s@CreateCoipPoolResponse' {} a -> s {coipPool = a} :: CreateCoipPoolResponse)

-- | The response's http status code.
createCoipPoolResponse_httpStatus :: Lens.Lens' CreateCoipPoolResponse Prelude.Int
createCoipPoolResponse_httpStatus = Lens.lens (\CreateCoipPoolResponse' {httpStatus} -> httpStatus) (\s@CreateCoipPoolResponse' {} a -> s {httpStatus = a} :: CreateCoipPoolResponse)

instance Prelude.NFData CreateCoipPoolResponse where
  rnf CreateCoipPoolResponse' {..} =
    Prelude.rnf coipPool
      `Prelude.seq` Prelude.rnf httpStatus
