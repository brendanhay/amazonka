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
-- Module      : Amazonka.DirectConnect.CreateDirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a proposal to associate the specified virtual private gateway or
-- transit gateway with the specified Direct Connect gateway.
--
-- You can associate a Direct Connect gateway and virtual private gateway
-- or transit gateway that is owned by any Amazon Web Services account.
module Amazonka.DirectConnect.CreateDirectConnectGatewayAssociationProposal
  ( -- * Creating a Request
    CreateDirectConnectGatewayAssociationProposal (..),
    newCreateDirectConnectGatewayAssociationProposal,

    -- * Request Lenses
    createDirectConnectGatewayAssociationProposal_addAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociationProposal_removeAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociationProposal_directConnectGatewayId,
    createDirectConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount,
    createDirectConnectGatewayAssociationProposal_gatewayId,

    -- * Destructuring the Response
    CreateDirectConnectGatewayAssociationProposalResponse (..),
    newCreateDirectConnectGatewayAssociationProposalResponse,

    -- * Response Lenses
    createDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal,
    createDirectConnectGatewayAssociationProposalResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDirectConnectGatewayAssociationProposal' smart constructor.
data CreateDirectConnectGatewayAssociationProposal = CreateDirectConnectGatewayAssociationProposal'
  { -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    addAllowedPrefixesToDirectConnectGateway :: Prelude.Maybe [RouteFilterPrefix],
    -- | The Amazon VPC prefixes to no longer advertise to the Direct Connect
    -- gateway.
    removeAllowedPrefixesToDirectConnectGateway :: Prelude.Maybe [RouteFilterPrefix],
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the Direct Connect
    -- gateway.
    directConnectGatewayOwnerAccount :: Prelude.Text,
    -- | The ID of the virtual private gateway or transit gateway.
    gatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectConnectGatewayAssociationProposal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addAllowedPrefixesToDirectConnectGateway', 'createDirectConnectGatewayAssociationProposal_addAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- 'removeAllowedPrefixesToDirectConnectGateway', 'createDirectConnectGatewayAssociationProposal_removeAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to no longer advertise to the Direct Connect
-- gateway.
--
-- 'directConnectGatewayId', 'createDirectConnectGatewayAssociationProposal_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'directConnectGatewayOwnerAccount', 'createDirectConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount' - The ID of the Amazon Web Services account that owns the Direct Connect
-- gateway.
--
-- 'gatewayId', 'createDirectConnectGatewayAssociationProposal_gatewayId' - The ID of the virtual private gateway or transit gateway.
newCreateDirectConnectGatewayAssociationProposal ::
  -- | 'directConnectGatewayId'
  Prelude.Text ->
  -- | 'directConnectGatewayOwnerAccount'
  Prelude.Text ->
  -- | 'gatewayId'
  Prelude.Text ->
  CreateDirectConnectGatewayAssociationProposal
newCreateDirectConnectGatewayAssociationProposal
  pDirectConnectGatewayId_
  pDirectConnectGatewayOwnerAccount_
  pGatewayId_ =
    CreateDirectConnectGatewayAssociationProposal'
      { addAllowedPrefixesToDirectConnectGateway =
          Prelude.Nothing,
        removeAllowedPrefixesToDirectConnectGateway =
          Prelude.Nothing,
        directConnectGatewayId =
          pDirectConnectGatewayId_,
        directConnectGatewayOwnerAccount =
          pDirectConnectGatewayOwnerAccount_,
        gatewayId = pGatewayId_
      }

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
createDirectConnectGatewayAssociationProposal_addAllowedPrefixesToDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal (Prelude.Maybe [RouteFilterPrefix])
createDirectConnectGatewayAssociationProposal_addAllowedPrefixesToDirectConnectGateway = Lens.lens (\CreateDirectConnectGatewayAssociationProposal' {addAllowedPrefixesToDirectConnectGateway} -> addAllowedPrefixesToDirectConnectGateway) (\s@CreateDirectConnectGatewayAssociationProposal' {} a -> s {addAllowedPrefixesToDirectConnectGateway = a} :: CreateDirectConnectGatewayAssociationProposal) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon VPC prefixes to no longer advertise to the Direct Connect
-- gateway.
createDirectConnectGatewayAssociationProposal_removeAllowedPrefixesToDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal (Prelude.Maybe [RouteFilterPrefix])
createDirectConnectGatewayAssociationProposal_removeAllowedPrefixesToDirectConnectGateway = Lens.lens (\CreateDirectConnectGatewayAssociationProposal' {removeAllowedPrefixesToDirectConnectGateway} -> removeAllowedPrefixesToDirectConnectGateway) (\s@CreateDirectConnectGatewayAssociationProposal' {} a -> s {removeAllowedPrefixesToDirectConnectGateway = a} :: CreateDirectConnectGatewayAssociationProposal) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Direct Connect gateway.
createDirectConnectGatewayAssociationProposal_directConnectGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal Prelude.Text
createDirectConnectGatewayAssociationProposal_directConnectGatewayId = Lens.lens (\CreateDirectConnectGatewayAssociationProposal' {directConnectGatewayId} -> directConnectGatewayId) (\s@CreateDirectConnectGatewayAssociationProposal' {} a -> s {directConnectGatewayId = a} :: CreateDirectConnectGatewayAssociationProposal)

-- | The ID of the Amazon Web Services account that owns the Direct Connect
-- gateway.
createDirectConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal Prelude.Text
createDirectConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount = Lens.lens (\CreateDirectConnectGatewayAssociationProposal' {directConnectGatewayOwnerAccount} -> directConnectGatewayOwnerAccount) (\s@CreateDirectConnectGatewayAssociationProposal' {} a -> s {directConnectGatewayOwnerAccount = a} :: CreateDirectConnectGatewayAssociationProposal)

-- | The ID of the virtual private gateway or transit gateway.
createDirectConnectGatewayAssociationProposal_gatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociationProposal Prelude.Text
createDirectConnectGatewayAssociationProposal_gatewayId = Lens.lens (\CreateDirectConnectGatewayAssociationProposal' {gatewayId} -> gatewayId) (\s@CreateDirectConnectGatewayAssociationProposal' {} a -> s {gatewayId = a} :: CreateDirectConnectGatewayAssociationProposal)

instance
  Core.AWSRequest
    CreateDirectConnectGatewayAssociationProposal
  where
  type
    AWSResponse
      CreateDirectConnectGatewayAssociationProposal =
      CreateDirectConnectGatewayAssociationProposalResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDirectConnectGatewayAssociationProposalResponse'
            Prelude.<$> ( x
                            Data..?> "directConnectGatewayAssociationProposal"
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateDirectConnectGatewayAssociationProposal
  where
  hashWithSalt
    _salt
    CreateDirectConnectGatewayAssociationProposal' {..} =
      _salt
        `Prelude.hashWithSalt` addAllowedPrefixesToDirectConnectGateway
        `Prelude.hashWithSalt` removeAllowedPrefixesToDirectConnectGateway
        `Prelude.hashWithSalt` directConnectGatewayId
        `Prelude.hashWithSalt` directConnectGatewayOwnerAccount
        `Prelude.hashWithSalt` gatewayId

instance
  Prelude.NFData
    CreateDirectConnectGatewayAssociationProposal
  where
  rnf
    CreateDirectConnectGatewayAssociationProposal' {..} =
      Prelude.rnf
        addAllowedPrefixesToDirectConnectGateway
        `Prelude.seq` Prelude.rnf
          removeAllowedPrefixesToDirectConnectGateway
        `Prelude.seq` Prelude.rnf directConnectGatewayId
        `Prelude.seq` Prelude.rnf directConnectGatewayOwnerAccount
        `Prelude.seq` Prelude.rnf gatewayId

instance
  Data.ToHeaders
    CreateDirectConnectGatewayAssociationProposal
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.CreateDirectConnectGatewayAssociationProposal" ::
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
    CreateDirectConnectGatewayAssociationProposal
  where
  toJSON
    CreateDirectConnectGatewayAssociationProposal' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("addAllowedPrefixesToDirectConnectGateway" Data..=)
                Prelude.<$> addAllowedPrefixesToDirectConnectGateway,
              ( "removeAllowedPrefixesToDirectConnectGateway"
                  Data..=
              )
                Prelude.<$> removeAllowedPrefixesToDirectConnectGateway,
              Prelude.Just
                ( "directConnectGatewayId"
                    Data..= directConnectGatewayId
                ),
              Prelude.Just
                ( "directConnectGatewayOwnerAccount"
                    Data..= directConnectGatewayOwnerAccount
                ),
              Prelude.Just ("gatewayId" Data..= gatewayId)
            ]
        )

instance
  Data.ToPath
    CreateDirectConnectGatewayAssociationProposal
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateDirectConnectGatewayAssociationProposal
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDirectConnectGatewayAssociationProposalResponse' smart constructor.
data CreateDirectConnectGatewayAssociationProposalResponse = CreateDirectConnectGatewayAssociationProposalResponse'
  { -- | Information about the Direct Connect gateway proposal.
    directConnectGatewayAssociationProposal :: Prelude.Maybe DirectConnectGatewayAssociationProposal,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectConnectGatewayAssociationProposalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayAssociationProposal', 'createDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal' - Information about the Direct Connect gateway proposal.
--
-- 'httpStatus', 'createDirectConnectGatewayAssociationProposalResponse_httpStatus' - The response's http status code.
newCreateDirectConnectGatewayAssociationProposalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDirectConnectGatewayAssociationProposalResponse
newCreateDirectConnectGatewayAssociationProposalResponse
  pHttpStatus_ =
    CreateDirectConnectGatewayAssociationProposalResponse'
      { directConnectGatewayAssociationProposal =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the Direct Connect gateway proposal.
createDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal :: Lens.Lens' CreateDirectConnectGatewayAssociationProposalResponse (Prelude.Maybe DirectConnectGatewayAssociationProposal)
createDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal = Lens.lens (\CreateDirectConnectGatewayAssociationProposalResponse' {directConnectGatewayAssociationProposal} -> directConnectGatewayAssociationProposal) (\s@CreateDirectConnectGatewayAssociationProposalResponse' {} a -> s {directConnectGatewayAssociationProposal = a} :: CreateDirectConnectGatewayAssociationProposalResponse)

-- | The response's http status code.
createDirectConnectGatewayAssociationProposalResponse_httpStatus :: Lens.Lens' CreateDirectConnectGatewayAssociationProposalResponse Prelude.Int
createDirectConnectGatewayAssociationProposalResponse_httpStatus = Lens.lens (\CreateDirectConnectGatewayAssociationProposalResponse' {httpStatus} -> httpStatus) (\s@CreateDirectConnectGatewayAssociationProposalResponse' {} a -> s {httpStatus = a} :: CreateDirectConnectGatewayAssociationProposalResponse)

instance
  Prelude.NFData
    CreateDirectConnectGatewayAssociationProposalResponse
  where
  rnf
    CreateDirectConnectGatewayAssociationProposalResponse' {..} =
      Prelude.rnf directConnectGatewayAssociationProposal
        `Prelude.seq` Prelude.rnf httpStatus
