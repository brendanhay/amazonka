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
-- Module      : Amazonka.DirectConnect.UpdateDirectConnectGateway
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of a current Direct Connect gateway.
module Amazonka.DirectConnect.UpdateDirectConnectGateway
  ( -- * Creating a Request
    UpdateDirectConnectGateway (..),
    newUpdateDirectConnectGateway,

    -- * Request Lenses
    updateDirectConnectGateway_directConnectGatewayId,
    updateDirectConnectGateway_newDirectConnectGatewayName,

    -- * Destructuring the Response
    UpdateDirectConnectGatewayResponse (..),
    newUpdateDirectConnectGatewayResponse,

    -- * Response Lenses
    updateDirectConnectGatewayResponse_directConnectGateway,
    updateDirectConnectGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDirectConnectGateway' smart constructor.
data UpdateDirectConnectGateway = UpdateDirectConnectGateway'
  { -- | The ID of the Direct Connect gateway to update.
    directConnectGatewayId :: Prelude.Text,
    -- | The new name for the Direct Connect gateway.
    newDirectConnectGatewayName' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDirectConnectGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayId', 'updateDirectConnectGateway_directConnectGatewayId' - The ID of the Direct Connect gateway to update.
--
-- 'newDirectConnectGatewayName'', 'updateDirectConnectGateway_newDirectConnectGatewayName' - The new name for the Direct Connect gateway.
newUpdateDirectConnectGateway ::
  -- | 'directConnectGatewayId'
  Prelude.Text ->
  -- | 'newDirectConnectGatewayName''
  Prelude.Text ->
  UpdateDirectConnectGateway
newUpdateDirectConnectGateway
  pDirectConnectGatewayId_
  pNewDirectConnectGatewayName_ =
    UpdateDirectConnectGateway'
      { directConnectGatewayId =
          pDirectConnectGatewayId_,
        newDirectConnectGatewayName' =
          pNewDirectConnectGatewayName_
      }

-- | The ID of the Direct Connect gateway to update.
updateDirectConnectGateway_directConnectGatewayId :: Lens.Lens' UpdateDirectConnectGateway Prelude.Text
updateDirectConnectGateway_directConnectGatewayId = Lens.lens (\UpdateDirectConnectGateway' {directConnectGatewayId} -> directConnectGatewayId) (\s@UpdateDirectConnectGateway' {} a -> s {directConnectGatewayId = a} :: UpdateDirectConnectGateway)

-- | The new name for the Direct Connect gateway.
updateDirectConnectGateway_newDirectConnectGatewayName :: Lens.Lens' UpdateDirectConnectGateway Prelude.Text
updateDirectConnectGateway_newDirectConnectGatewayName = Lens.lens (\UpdateDirectConnectGateway' {newDirectConnectGatewayName'} -> newDirectConnectGatewayName') (\s@UpdateDirectConnectGateway' {} a -> s {newDirectConnectGatewayName' = a} :: UpdateDirectConnectGateway)

instance Core.AWSRequest UpdateDirectConnectGateway where
  type
    AWSResponse UpdateDirectConnectGateway =
      UpdateDirectConnectGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDirectConnectGatewayResponse'
            Prelude.<$> (x Core..?> "directConnectGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDirectConnectGateway where
  hashWithSalt _salt UpdateDirectConnectGateway' {..} =
    _salt `Prelude.hashWithSalt` directConnectGatewayId
      `Prelude.hashWithSalt` newDirectConnectGatewayName'

instance Prelude.NFData UpdateDirectConnectGateway where
  rnf UpdateDirectConnectGateway' {..} =
    Prelude.rnf directConnectGatewayId
      `Prelude.seq` Prelude.rnf newDirectConnectGatewayName'

instance Core.ToHeaders UpdateDirectConnectGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.UpdateDirectConnectGateway" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDirectConnectGateway where
  toJSON UpdateDirectConnectGateway' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "directConnectGatewayId"
                  Core..= directConnectGatewayId
              ),
            Prelude.Just
              ( "newDirectConnectGatewayName"
                  Core..= newDirectConnectGatewayName'
              )
          ]
      )

instance Core.ToPath UpdateDirectConnectGateway where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDirectConnectGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDirectConnectGatewayResponse' smart constructor.
data UpdateDirectConnectGatewayResponse = UpdateDirectConnectGatewayResponse'
  { directConnectGateway :: Prelude.Maybe DirectConnectGateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDirectConnectGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGateway', 'updateDirectConnectGatewayResponse_directConnectGateway' - Undocumented member.
--
-- 'httpStatus', 'updateDirectConnectGatewayResponse_httpStatus' - The response's http status code.
newUpdateDirectConnectGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDirectConnectGatewayResponse
newUpdateDirectConnectGatewayResponse pHttpStatus_ =
  UpdateDirectConnectGatewayResponse'
    { directConnectGateway =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateDirectConnectGatewayResponse_directConnectGateway :: Lens.Lens' UpdateDirectConnectGatewayResponse (Prelude.Maybe DirectConnectGateway)
updateDirectConnectGatewayResponse_directConnectGateway = Lens.lens (\UpdateDirectConnectGatewayResponse' {directConnectGateway} -> directConnectGateway) (\s@UpdateDirectConnectGatewayResponse' {} a -> s {directConnectGateway = a} :: UpdateDirectConnectGatewayResponse)

-- | The response's http status code.
updateDirectConnectGatewayResponse_httpStatus :: Lens.Lens' UpdateDirectConnectGatewayResponse Prelude.Int
updateDirectConnectGatewayResponse_httpStatus = Lens.lens (\UpdateDirectConnectGatewayResponse' {httpStatus} -> httpStatus) (\s@UpdateDirectConnectGatewayResponse' {} a -> s {httpStatus = a} :: UpdateDirectConnectGatewayResponse)

instance
  Prelude.NFData
    UpdateDirectConnectGatewayResponse
  where
  rnf UpdateDirectConnectGatewayResponse' {..} =
    Prelude.rnf directConnectGateway
      `Prelude.seq` Prelude.rnf httpStatus
