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
-- Module      : Amazonka.DirectConnect.UpdateDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the Direct Connect gateway
-- association.
--
-- Add or remove prefixes from the association.
module Amazonka.DirectConnect.UpdateDirectConnectGatewayAssociation
  ( -- * Creating a Request
    UpdateDirectConnectGatewayAssociation (..),
    newUpdateDirectConnectGatewayAssociation,

    -- * Request Lenses
    updateDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway,
    updateDirectConnectGatewayAssociation_associationId,
    updateDirectConnectGatewayAssociation_removeAllowedPrefixesToDirectConnectGateway,

    -- * Destructuring the Response
    UpdateDirectConnectGatewayAssociationResponse (..),
    newUpdateDirectConnectGatewayAssociationResponse,

    -- * Response Lenses
    updateDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    updateDirectConnectGatewayAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDirectConnectGatewayAssociation' smart constructor.
data UpdateDirectConnectGatewayAssociation = UpdateDirectConnectGatewayAssociation'
  { -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    addAllowedPrefixesToDirectConnectGateway :: Prelude.Maybe [RouteFilterPrefix],
    -- | The ID of the Direct Connect gateway association.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon VPC prefixes to no longer advertise to the Direct Connect
    -- gateway.
    removeAllowedPrefixesToDirectConnectGateway :: Prelude.Maybe [RouteFilterPrefix]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDirectConnectGatewayAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addAllowedPrefixesToDirectConnectGateway', 'updateDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- 'associationId', 'updateDirectConnectGatewayAssociation_associationId' - The ID of the Direct Connect gateway association.
--
-- 'removeAllowedPrefixesToDirectConnectGateway', 'updateDirectConnectGatewayAssociation_removeAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to no longer advertise to the Direct Connect
-- gateway.
newUpdateDirectConnectGatewayAssociation ::
  UpdateDirectConnectGatewayAssociation
newUpdateDirectConnectGatewayAssociation =
  UpdateDirectConnectGatewayAssociation'
    { addAllowedPrefixesToDirectConnectGateway =
        Prelude.Nothing,
      associationId = Prelude.Nothing,
      removeAllowedPrefixesToDirectConnectGateway =
        Prelude.Nothing
    }

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
updateDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Prelude.Maybe [RouteFilterPrefix])
updateDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway = Lens.lens (\UpdateDirectConnectGatewayAssociation' {addAllowedPrefixesToDirectConnectGateway} -> addAllowedPrefixesToDirectConnectGateway) (\s@UpdateDirectConnectGatewayAssociation' {} a -> s {addAllowedPrefixesToDirectConnectGateway = a} :: UpdateDirectConnectGatewayAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Direct Connect gateway association.
updateDirectConnectGatewayAssociation_associationId :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
updateDirectConnectGatewayAssociation_associationId = Lens.lens (\UpdateDirectConnectGatewayAssociation' {associationId} -> associationId) (\s@UpdateDirectConnectGatewayAssociation' {} a -> s {associationId = a} :: UpdateDirectConnectGatewayAssociation)

-- | The Amazon VPC prefixes to no longer advertise to the Direct Connect
-- gateway.
updateDirectConnectGatewayAssociation_removeAllowedPrefixesToDirectConnectGateway :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Prelude.Maybe [RouteFilterPrefix])
updateDirectConnectGatewayAssociation_removeAllowedPrefixesToDirectConnectGateway = Lens.lens (\UpdateDirectConnectGatewayAssociation' {removeAllowedPrefixesToDirectConnectGateway} -> removeAllowedPrefixesToDirectConnectGateway) (\s@UpdateDirectConnectGatewayAssociation' {} a -> s {removeAllowedPrefixesToDirectConnectGateway = a} :: UpdateDirectConnectGatewayAssociation) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    UpdateDirectConnectGatewayAssociation
  where
  type
    AWSResponse
      UpdateDirectConnectGatewayAssociation =
      UpdateDirectConnectGatewayAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDirectConnectGatewayAssociationResponse'
            Prelude.<$> (x Data..?> "directConnectGatewayAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateDirectConnectGatewayAssociation
  where
  hashWithSalt
    _salt
    UpdateDirectConnectGatewayAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` addAllowedPrefixesToDirectConnectGateway
        `Prelude.hashWithSalt` associationId
        `Prelude.hashWithSalt` removeAllowedPrefixesToDirectConnectGateway

instance
  Prelude.NFData
    UpdateDirectConnectGatewayAssociation
  where
  rnf UpdateDirectConnectGatewayAssociation' {..} =
    Prelude.rnf
      addAllowedPrefixesToDirectConnectGateway
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf
        removeAllowedPrefixesToDirectConnectGateway

instance
  Data.ToHeaders
    UpdateDirectConnectGatewayAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.UpdateDirectConnectGatewayAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateDirectConnectGatewayAssociation
  where
  toJSON UpdateDirectConnectGatewayAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addAllowedPrefixesToDirectConnectGateway" Data..=)
              Prelude.<$> addAllowedPrefixesToDirectConnectGateway,
            ("associationId" Data..=) Prelude.<$> associationId,
            ( "removeAllowedPrefixesToDirectConnectGateway"
                Data..=
            )
              Prelude.<$> removeAllowedPrefixesToDirectConnectGateway
          ]
      )

instance
  Data.ToPath
    UpdateDirectConnectGatewayAssociation
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateDirectConnectGatewayAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDirectConnectGatewayAssociationResponse' smart constructor.
data UpdateDirectConnectGatewayAssociationResponse = UpdateDirectConnectGatewayAssociationResponse'
  { directConnectGatewayAssociation :: Prelude.Maybe DirectConnectGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDirectConnectGatewayAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayAssociation', 'updateDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation' - Undocumented member.
--
-- 'httpStatus', 'updateDirectConnectGatewayAssociationResponse_httpStatus' - The response's http status code.
newUpdateDirectConnectGatewayAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDirectConnectGatewayAssociationResponse
newUpdateDirectConnectGatewayAssociationResponse
  pHttpStatus_ =
    UpdateDirectConnectGatewayAssociationResponse'
      { directConnectGatewayAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
updateDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation :: Lens.Lens' UpdateDirectConnectGatewayAssociationResponse (Prelude.Maybe DirectConnectGatewayAssociation)
updateDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation = Lens.lens (\UpdateDirectConnectGatewayAssociationResponse' {directConnectGatewayAssociation} -> directConnectGatewayAssociation) (\s@UpdateDirectConnectGatewayAssociationResponse' {} a -> s {directConnectGatewayAssociation = a} :: UpdateDirectConnectGatewayAssociationResponse)

-- | The response's http status code.
updateDirectConnectGatewayAssociationResponse_httpStatus :: Lens.Lens' UpdateDirectConnectGatewayAssociationResponse Prelude.Int
updateDirectConnectGatewayAssociationResponse_httpStatus = Lens.lens (\UpdateDirectConnectGatewayAssociationResponse' {httpStatus} -> httpStatus) (\s@UpdateDirectConnectGatewayAssociationResponse' {} a -> s {httpStatus = a} :: UpdateDirectConnectGatewayAssociationResponse)

instance
  Prelude.NFData
    UpdateDirectConnectGatewayAssociationResponse
  where
  rnf
    UpdateDirectConnectGatewayAssociationResponse' {..} =
      Prelude.rnf directConnectGatewayAssociation
        `Prelude.seq` Prelude.rnf httpStatus
