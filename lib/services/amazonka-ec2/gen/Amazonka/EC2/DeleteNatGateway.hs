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
-- Module      : Amazonka.EC2.DeleteNatGateway
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified NAT gateway. Deleting a public NAT gateway
-- disassociates its Elastic IP address, but does not release the address
-- from your account. Deleting a NAT gateway does not delete any NAT
-- gateway routes in your route tables.
module Amazonka.EC2.DeleteNatGateway
  ( -- * Creating a Request
    DeleteNatGateway (..),
    newDeleteNatGateway,

    -- * Request Lenses
    deleteNatGateway_dryRun,
    deleteNatGateway_natGatewayId,

    -- * Destructuring the Response
    DeleteNatGatewayResponse (..),
    newDeleteNatGatewayResponse,

    -- * Response Lenses
    deleteNatGatewayResponse_natGatewayId,
    deleteNatGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNatGateway' smart constructor.
data DeleteNatGateway = DeleteNatGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the NAT gateway.
    natGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNatGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteNatGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'natGatewayId', 'deleteNatGateway_natGatewayId' - The ID of the NAT gateway.
newDeleteNatGateway ::
  -- | 'natGatewayId'
  Prelude.Text ->
  DeleteNatGateway
newDeleteNatGateway pNatGatewayId_ =
  DeleteNatGateway'
    { dryRun = Prelude.Nothing,
      natGatewayId = pNatGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNatGateway_dryRun :: Lens.Lens' DeleteNatGateway (Prelude.Maybe Prelude.Bool)
deleteNatGateway_dryRun = Lens.lens (\DeleteNatGateway' {dryRun} -> dryRun) (\s@DeleteNatGateway' {} a -> s {dryRun = a} :: DeleteNatGateway)

-- | The ID of the NAT gateway.
deleteNatGateway_natGatewayId :: Lens.Lens' DeleteNatGateway Prelude.Text
deleteNatGateway_natGatewayId = Lens.lens (\DeleteNatGateway' {natGatewayId} -> natGatewayId) (\s@DeleteNatGateway' {} a -> s {natGatewayId = a} :: DeleteNatGateway)

instance Core.AWSRequest DeleteNatGateway where
  type
    AWSResponse DeleteNatGateway =
      DeleteNatGatewayResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteNatGatewayResponse'
            Prelude.<$> (x Core..@? "natGatewayId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNatGateway where
  hashWithSalt _salt DeleteNatGateway' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` natGatewayId

instance Prelude.NFData DeleteNatGateway where
  rnf DeleteNatGateway' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf natGatewayId

instance Core.ToHeaders DeleteNatGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteNatGateway where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteNatGateway where
  toQuery DeleteNatGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteNatGateway" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "NatGatewayId" Core.=: natGatewayId
      ]

-- | /See:/ 'newDeleteNatGatewayResponse' smart constructor.
data DeleteNatGatewayResponse = DeleteNatGatewayResponse'
  { -- | The ID of the NAT gateway.
    natGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNatGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'natGatewayId', 'deleteNatGatewayResponse_natGatewayId' - The ID of the NAT gateway.
--
-- 'httpStatus', 'deleteNatGatewayResponse_httpStatus' - The response's http status code.
newDeleteNatGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNatGatewayResponse
newDeleteNatGatewayResponse pHttpStatus_ =
  DeleteNatGatewayResponse'
    { natGatewayId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the NAT gateway.
deleteNatGatewayResponse_natGatewayId :: Lens.Lens' DeleteNatGatewayResponse (Prelude.Maybe Prelude.Text)
deleteNatGatewayResponse_natGatewayId = Lens.lens (\DeleteNatGatewayResponse' {natGatewayId} -> natGatewayId) (\s@DeleteNatGatewayResponse' {} a -> s {natGatewayId = a} :: DeleteNatGatewayResponse)

-- | The response's http status code.
deleteNatGatewayResponse_httpStatus :: Lens.Lens' DeleteNatGatewayResponse Prelude.Int
deleteNatGatewayResponse_httpStatus = Lens.lens (\DeleteNatGatewayResponse' {httpStatus} -> httpStatus) (\s@DeleteNatGatewayResponse' {} a -> s {httpStatus = a} :: DeleteNatGatewayResponse)

instance Prelude.NFData DeleteNatGatewayResponse where
  rnf DeleteNatGatewayResponse' {..} =
    Prelude.rnf natGatewayId
      `Prelude.seq` Prelude.rnf httpStatus
