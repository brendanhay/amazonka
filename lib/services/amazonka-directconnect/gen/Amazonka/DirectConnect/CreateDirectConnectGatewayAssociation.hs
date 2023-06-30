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
-- Module      : Amazonka.DirectConnect.CreateDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between a Direct Connect gateway and a virtual
-- private gateway. The virtual private gateway must be attached to a VPC
-- and must not be associated with another Direct Connect gateway.
module Amazonka.DirectConnect.CreateDirectConnectGatewayAssociation
  ( -- * Creating a Request
    CreateDirectConnectGatewayAssociation (..),
    newCreateDirectConnectGatewayAssociation,

    -- * Request Lenses
    createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociation_gatewayId,
    createDirectConnectGatewayAssociation_virtualGatewayId,
    createDirectConnectGatewayAssociation_directConnectGatewayId,

    -- * Destructuring the Response
    CreateDirectConnectGatewayAssociationResponse (..),
    newCreateDirectConnectGatewayAssociationResponse,

    -- * Response Lenses
    createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    createDirectConnectGatewayAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDirectConnectGatewayAssociation' smart constructor.
data CreateDirectConnectGatewayAssociation = CreateDirectConnectGatewayAssociation'
  { -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway
    --
    -- This parameter is required when you create an association to a transit
    -- gateway.
    --
    -- For information about how to set the prefixes, see
    -- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes>
    -- in the /Direct Connect User Guide/.
    addAllowedPrefixesToDirectConnectGateway :: Prelude.Maybe [RouteFilterPrefix],
    -- | The ID of the virtual private gateway or transit gateway.
    gatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual private gateway.
    virtualGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectConnectGatewayAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addAllowedPrefixesToDirectConnectGateway', 'createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway
--
-- This parameter is required when you create an association to a transit
-- gateway.
--
-- For information about how to set the prefixes, see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes>
-- in the /Direct Connect User Guide/.
--
-- 'gatewayId', 'createDirectConnectGatewayAssociation_gatewayId' - The ID of the virtual private gateway or transit gateway.
--
-- 'virtualGatewayId', 'createDirectConnectGatewayAssociation_virtualGatewayId' - The ID of the virtual private gateway.
--
-- 'directConnectGatewayId', 'createDirectConnectGatewayAssociation_directConnectGatewayId' - The ID of the Direct Connect gateway.
newCreateDirectConnectGatewayAssociation ::
  -- | 'directConnectGatewayId'
  Prelude.Text ->
  CreateDirectConnectGatewayAssociation
newCreateDirectConnectGatewayAssociation
  pDirectConnectGatewayId_ =
    CreateDirectConnectGatewayAssociation'
      { addAllowedPrefixesToDirectConnectGateway =
          Prelude.Nothing,
        gatewayId = Prelude.Nothing,
        virtualGatewayId = Prelude.Nothing,
        directConnectGatewayId =
          pDirectConnectGatewayId_
      }

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway
--
-- This parameter is required when you create an association to a transit
-- gateway.
--
-- For information about how to set the prefixes, see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes>
-- in the /Direct Connect User Guide/.
createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayAssociation (Prelude.Maybe [RouteFilterPrefix])
createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway = Lens.lens (\CreateDirectConnectGatewayAssociation' {addAllowedPrefixesToDirectConnectGateway} -> addAllowedPrefixesToDirectConnectGateway) (\s@CreateDirectConnectGatewayAssociation' {} a -> s {addAllowedPrefixesToDirectConnectGateway = a} :: CreateDirectConnectGatewayAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the virtual private gateway or transit gateway.
createDirectConnectGatewayAssociation_gatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
createDirectConnectGatewayAssociation_gatewayId = Lens.lens (\CreateDirectConnectGatewayAssociation' {gatewayId} -> gatewayId) (\s@CreateDirectConnectGatewayAssociation' {} a -> s {gatewayId = a} :: CreateDirectConnectGatewayAssociation)

-- | The ID of the virtual private gateway.
createDirectConnectGatewayAssociation_virtualGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
createDirectConnectGatewayAssociation_virtualGatewayId = Lens.lens (\CreateDirectConnectGatewayAssociation' {virtualGatewayId} -> virtualGatewayId) (\s@CreateDirectConnectGatewayAssociation' {} a -> s {virtualGatewayId = a} :: CreateDirectConnectGatewayAssociation)

-- | The ID of the Direct Connect gateway.
createDirectConnectGatewayAssociation_directConnectGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation Prelude.Text
createDirectConnectGatewayAssociation_directConnectGatewayId = Lens.lens (\CreateDirectConnectGatewayAssociation' {directConnectGatewayId} -> directConnectGatewayId) (\s@CreateDirectConnectGatewayAssociation' {} a -> s {directConnectGatewayId = a} :: CreateDirectConnectGatewayAssociation)

instance
  Core.AWSRequest
    CreateDirectConnectGatewayAssociation
  where
  type
    AWSResponse
      CreateDirectConnectGatewayAssociation =
      CreateDirectConnectGatewayAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDirectConnectGatewayAssociationResponse'
            Prelude.<$> (x Data..?> "directConnectGatewayAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateDirectConnectGatewayAssociation
  where
  hashWithSalt
    _salt
    CreateDirectConnectGatewayAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` addAllowedPrefixesToDirectConnectGateway
        `Prelude.hashWithSalt` gatewayId
        `Prelude.hashWithSalt` virtualGatewayId
        `Prelude.hashWithSalt` directConnectGatewayId

instance
  Prelude.NFData
    CreateDirectConnectGatewayAssociation
  where
  rnf CreateDirectConnectGatewayAssociation' {..} =
    Prelude.rnf
      addAllowedPrefixesToDirectConnectGateway
      `Prelude.seq` Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf virtualGatewayId
      `Prelude.seq` Prelude.rnf directConnectGatewayId

instance
  Data.ToHeaders
    CreateDirectConnectGatewayAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.CreateDirectConnectGatewayAssociation" ::
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
    CreateDirectConnectGatewayAssociation
  where
  toJSON CreateDirectConnectGatewayAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addAllowedPrefixesToDirectConnectGateway" Data..=)
              Prelude.<$> addAllowedPrefixesToDirectConnectGateway,
            ("gatewayId" Data..=) Prelude.<$> gatewayId,
            ("virtualGatewayId" Data..=)
              Prelude.<$> virtualGatewayId,
            Prelude.Just
              ( "directConnectGatewayId"
                  Data..= directConnectGatewayId
              )
          ]
      )

instance
  Data.ToPath
    CreateDirectConnectGatewayAssociation
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateDirectConnectGatewayAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDirectConnectGatewayAssociationResponse' smart constructor.
data CreateDirectConnectGatewayAssociationResponse = CreateDirectConnectGatewayAssociationResponse'
  { -- | The association to be created.
    directConnectGatewayAssociation :: Prelude.Maybe DirectConnectGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectConnectGatewayAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayAssociation', 'createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation' - The association to be created.
--
-- 'httpStatus', 'createDirectConnectGatewayAssociationResponse_httpStatus' - The response's http status code.
newCreateDirectConnectGatewayAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDirectConnectGatewayAssociationResponse
newCreateDirectConnectGatewayAssociationResponse
  pHttpStatus_ =
    CreateDirectConnectGatewayAssociationResponse'
      { directConnectGatewayAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The association to be created.
createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation :: Lens.Lens' CreateDirectConnectGatewayAssociationResponse (Prelude.Maybe DirectConnectGatewayAssociation)
createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation = Lens.lens (\CreateDirectConnectGatewayAssociationResponse' {directConnectGatewayAssociation} -> directConnectGatewayAssociation) (\s@CreateDirectConnectGatewayAssociationResponse' {} a -> s {directConnectGatewayAssociation = a} :: CreateDirectConnectGatewayAssociationResponse)

-- | The response's http status code.
createDirectConnectGatewayAssociationResponse_httpStatus :: Lens.Lens' CreateDirectConnectGatewayAssociationResponse Prelude.Int
createDirectConnectGatewayAssociationResponse_httpStatus = Lens.lens (\CreateDirectConnectGatewayAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateDirectConnectGatewayAssociationResponse' {} a -> s {httpStatus = a} :: CreateDirectConnectGatewayAssociationResponse)

instance
  Prelude.NFData
    CreateDirectConnectGatewayAssociationResponse
  where
  rnf
    CreateDirectConnectGatewayAssociationResponse' {..} =
      Prelude.rnf directConnectGatewayAssociation
        `Prelude.seq` Prelude.rnf httpStatus
