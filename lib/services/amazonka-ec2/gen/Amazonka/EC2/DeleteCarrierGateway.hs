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
-- Module      : Amazonka.EC2.DeleteCarrierGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a carrier gateway.
--
-- If you do not delete the route that contains the carrier gateway as the
-- Target, the route is a blackhole route. For information about how to
-- delete a route, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteRoute.html DeleteRoute>.
module Amazonka.EC2.DeleteCarrierGateway
  ( -- * Creating a Request
    DeleteCarrierGateway (..),
    newDeleteCarrierGateway,

    -- * Request Lenses
    deleteCarrierGateway_dryRun,
    deleteCarrierGateway_carrierGatewayId,

    -- * Destructuring the Response
    DeleteCarrierGatewayResponse (..),
    newDeleteCarrierGatewayResponse,

    -- * Response Lenses
    deleteCarrierGatewayResponse_carrierGateway,
    deleteCarrierGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCarrierGateway' smart constructor.
data DeleteCarrierGateway = DeleteCarrierGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the carrier gateway.
    carrierGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCarrierGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteCarrierGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'carrierGatewayId', 'deleteCarrierGateway_carrierGatewayId' - The ID of the carrier gateway.
newDeleteCarrierGateway ::
  -- | 'carrierGatewayId'
  Prelude.Text ->
  DeleteCarrierGateway
newDeleteCarrierGateway pCarrierGatewayId_ =
  DeleteCarrierGateway'
    { dryRun = Prelude.Nothing,
      carrierGatewayId = pCarrierGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteCarrierGateway_dryRun :: Lens.Lens' DeleteCarrierGateway (Prelude.Maybe Prelude.Bool)
deleteCarrierGateway_dryRun = Lens.lens (\DeleteCarrierGateway' {dryRun} -> dryRun) (\s@DeleteCarrierGateway' {} a -> s {dryRun = a} :: DeleteCarrierGateway)

-- | The ID of the carrier gateway.
deleteCarrierGateway_carrierGatewayId :: Lens.Lens' DeleteCarrierGateway Prelude.Text
deleteCarrierGateway_carrierGatewayId = Lens.lens (\DeleteCarrierGateway' {carrierGatewayId} -> carrierGatewayId) (\s@DeleteCarrierGateway' {} a -> s {carrierGatewayId = a} :: DeleteCarrierGateway)

instance Core.AWSRequest DeleteCarrierGateway where
  type
    AWSResponse DeleteCarrierGateway =
      DeleteCarrierGatewayResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteCarrierGatewayResponse'
            Prelude.<$> (x Data..@? "carrierGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCarrierGateway where
  hashWithSalt _salt DeleteCarrierGateway' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` carrierGatewayId

instance Prelude.NFData DeleteCarrierGateway where
  rnf DeleteCarrierGateway' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf carrierGatewayId

instance Data.ToHeaders DeleteCarrierGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteCarrierGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCarrierGateway where
  toQuery DeleteCarrierGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteCarrierGateway" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "CarrierGatewayId" Data.=: carrierGatewayId
      ]

-- | /See:/ 'newDeleteCarrierGatewayResponse' smart constructor.
data DeleteCarrierGatewayResponse = DeleteCarrierGatewayResponse'
  { -- | Information about the carrier gateway.
    carrierGateway :: Prelude.Maybe CarrierGateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCarrierGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carrierGateway', 'deleteCarrierGatewayResponse_carrierGateway' - Information about the carrier gateway.
--
-- 'httpStatus', 'deleteCarrierGatewayResponse_httpStatus' - The response's http status code.
newDeleteCarrierGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCarrierGatewayResponse
newDeleteCarrierGatewayResponse pHttpStatus_ =
  DeleteCarrierGatewayResponse'
    { carrierGateway =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the carrier gateway.
deleteCarrierGatewayResponse_carrierGateway :: Lens.Lens' DeleteCarrierGatewayResponse (Prelude.Maybe CarrierGateway)
deleteCarrierGatewayResponse_carrierGateway = Lens.lens (\DeleteCarrierGatewayResponse' {carrierGateway} -> carrierGateway) (\s@DeleteCarrierGatewayResponse' {} a -> s {carrierGateway = a} :: DeleteCarrierGatewayResponse)

-- | The response's http status code.
deleteCarrierGatewayResponse_httpStatus :: Lens.Lens' DeleteCarrierGatewayResponse Prelude.Int
deleteCarrierGatewayResponse_httpStatus = Lens.lens (\DeleteCarrierGatewayResponse' {httpStatus} -> httpStatus) (\s@DeleteCarrierGatewayResponse' {} a -> s {httpStatus = a} :: DeleteCarrierGatewayResponse)

instance Prelude.NFData DeleteCarrierGatewayResponse where
  rnf DeleteCarrierGatewayResponse' {..} =
    Prelude.rnf carrierGateway
      `Prelude.seq` Prelude.rnf httpStatus
