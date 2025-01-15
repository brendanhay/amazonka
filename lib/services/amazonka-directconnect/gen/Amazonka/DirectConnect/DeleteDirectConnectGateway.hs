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
-- Module      : Amazonka.DirectConnect.DeleteDirectConnectGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Direct Connect gateway. You must first delete all
-- virtual interfaces that are attached to the Direct Connect gateway and
-- disassociate all virtual private gateways associated with the Direct
-- Connect gateway.
module Amazonka.DirectConnect.DeleteDirectConnectGateway
  ( -- * Creating a Request
    DeleteDirectConnectGateway (..),
    newDeleteDirectConnectGateway,

    -- * Request Lenses
    deleteDirectConnectGateway_directConnectGatewayId,

    -- * Destructuring the Response
    DeleteDirectConnectGatewayResponse (..),
    newDeleteDirectConnectGatewayResponse,

    -- * Response Lenses
    deleteDirectConnectGatewayResponse_directConnectGateway,
    deleteDirectConnectGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDirectConnectGateway' smart constructor.
data DeleteDirectConnectGateway = DeleteDirectConnectGateway'
  { -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDirectConnectGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayId', 'deleteDirectConnectGateway_directConnectGatewayId' - The ID of the Direct Connect gateway.
newDeleteDirectConnectGateway ::
  -- | 'directConnectGatewayId'
  Prelude.Text ->
  DeleteDirectConnectGateway
newDeleteDirectConnectGateway
  pDirectConnectGatewayId_ =
    DeleteDirectConnectGateway'
      { directConnectGatewayId =
          pDirectConnectGatewayId_
      }

-- | The ID of the Direct Connect gateway.
deleteDirectConnectGateway_directConnectGatewayId :: Lens.Lens' DeleteDirectConnectGateway Prelude.Text
deleteDirectConnectGateway_directConnectGatewayId = Lens.lens (\DeleteDirectConnectGateway' {directConnectGatewayId} -> directConnectGatewayId) (\s@DeleteDirectConnectGateway' {} a -> s {directConnectGatewayId = a} :: DeleteDirectConnectGateway)

instance Core.AWSRequest DeleteDirectConnectGateway where
  type
    AWSResponse DeleteDirectConnectGateway =
      DeleteDirectConnectGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDirectConnectGatewayResponse'
            Prelude.<$> (x Data..?> "directConnectGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDirectConnectGateway where
  hashWithSalt _salt DeleteDirectConnectGateway' {..} =
    _salt `Prelude.hashWithSalt` directConnectGatewayId

instance Prelude.NFData DeleteDirectConnectGateway where
  rnf DeleteDirectConnectGateway' {..} =
    Prelude.rnf directConnectGatewayId

instance Data.ToHeaders DeleteDirectConnectGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DeleteDirectConnectGateway" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDirectConnectGateway where
  toJSON DeleteDirectConnectGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "directConnectGatewayId"
                  Data..= directConnectGatewayId
              )
          ]
      )

instance Data.ToPath DeleteDirectConnectGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDirectConnectGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDirectConnectGatewayResponse' smart constructor.
data DeleteDirectConnectGatewayResponse = DeleteDirectConnectGatewayResponse'
  { -- | The Direct Connect gateway.
    directConnectGateway :: Prelude.Maybe DirectConnectGateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDirectConnectGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGateway', 'deleteDirectConnectGatewayResponse_directConnectGateway' - The Direct Connect gateway.
--
-- 'httpStatus', 'deleteDirectConnectGatewayResponse_httpStatus' - The response's http status code.
newDeleteDirectConnectGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDirectConnectGatewayResponse
newDeleteDirectConnectGatewayResponse pHttpStatus_ =
  DeleteDirectConnectGatewayResponse'
    { directConnectGateway =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Direct Connect gateway.
deleteDirectConnectGatewayResponse_directConnectGateway :: Lens.Lens' DeleteDirectConnectGatewayResponse (Prelude.Maybe DirectConnectGateway)
deleteDirectConnectGatewayResponse_directConnectGateway = Lens.lens (\DeleteDirectConnectGatewayResponse' {directConnectGateway} -> directConnectGateway) (\s@DeleteDirectConnectGatewayResponse' {} a -> s {directConnectGateway = a} :: DeleteDirectConnectGatewayResponse)

-- | The response's http status code.
deleteDirectConnectGatewayResponse_httpStatus :: Lens.Lens' DeleteDirectConnectGatewayResponse Prelude.Int
deleteDirectConnectGatewayResponse_httpStatus = Lens.lens (\DeleteDirectConnectGatewayResponse' {httpStatus} -> httpStatus) (\s@DeleteDirectConnectGatewayResponse' {} a -> s {httpStatus = a} :: DeleteDirectConnectGatewayResponse)

instance
  Prelude.NFData
    DeleteDirectConnectGatewayResponse
  where
  rnf DeleteDirectConnectGatewayResponse' {..} =
    Prelude.rnf directConnectGateway `Prelude.seq`
      Prelude.rnf httpStatus
