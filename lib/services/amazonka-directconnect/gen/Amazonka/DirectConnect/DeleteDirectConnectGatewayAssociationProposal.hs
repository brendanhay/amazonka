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
-- Module      : Amazonka.DirectConnect.DeleteDirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association proposal request between the specified Direct
-- Connect gateway and virtual private gateway or transit gateway.
module Amazonka.DirectConnect.DeleteDirectConnectGatewayAssociationProposal
  ( -- * Creating a Request
    DeleteDirectConnectGatewayAssociationProposal (..),
    newDeleteDirectConnectGatewayAssociationProposal,

    -- * Request Lenses
    deleteDirectConnectGatewayAssociationProposal_proposalId,

    -- * Destructuring the Response
    DeleteDirectConnectGatewayAssociationProposalResponse (..),
    newDeleteDirectConnectGatewayAssociationProposalResponse,

    -- * Response Lenses
    deleteDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal,
    deleteDirectConnectGatewayAssociationProposalResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDirectConnectGatewayAssociationProposal' smart constructor.
data DeleteDirectConnectGatewayAssociationProposal = DeleteDirectConnectGatewayAssociationProposal'
  { -- | The ID of the proposal.
    proposalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDirectConnectGatewayAssociationProposal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proposalId', 'deleteDirectConnectGatewayAssociationProposal_proposalId' - The ID of the proposal.
newDeleteDirectConnectGatewayAssociationProposal ::
  -- | 'proposalId'
  Prelude.Text ->
  DeleteDirectConnectGatewayAssociationProposal
newDeleteDirectConnectGatewayAssociationProposal
  pProposalId_ =
    DeleteDirectConnectGatewayAssociationProposal'
      { proposalId =
          pProposalId_
      }

-- | The ID of the proposal.
deleteDirectConnectGatewayAssociationProposal_proposalId :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposal Prelude.Text
deleteDirectConnectGatewayAssociationProposal_proposalId = Lens.lens (\DeleteDirectConnectGatewayAssociationProposal' {proposalId} -> proposalId) (\s@DeleteDirectConnectGatewayAssociationProposal' {} a -> s {proposalId = a} :: DeleteDirectConnectGatewayAssociationProposal)

instance
  Core.AWSRequest
    DeleteDirectConnectGatewayAssociationProposal
  where
  type
    AWSResponse
      DeleteDirectConnectGatewayAssociationProposal =
      DeleteDirectConnectGatewayAssociationProposalResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDirectConnectGatewayAssociationProposalResponse'
            Prelude.<$> ( x
                            Data..?> "directConnectGatewayAssociationProposal"
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteDirectConnectGatewayAssociationProposal
  where
  hashWithSalt
    _salt
    DeleteDirectConnectGatewayAssociationProposal' {..} =
      _salt `Prelude.hashWithSalt` proposalId

instance
  Prelude.NFData
    DeleteDirectConnectGatewayAssociationProposal
  where
  rnf
    DeleteDirectConnectGatewayAssociationProposal' {..} =
      Prelude.rnf proposalId

instance
  Data.ToHeaders
    DeleteDirectConnectGatewayAssociationProposal
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DeleteDirectConnectGatewayAssociationProposal" ::
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
    DeleteDirectConnectGatewayAssociationProposal
  where
  toJSON
    DeleteDirectConnectGatewayAssociationProposal' {..} =
      Data.object
        ( Prelude.catMaybes
            [Prelude.Just ("proposalId" Data..= proposalId)]
        )

instance
  Data.ToPath
    DeleteDirectConnectGatewayAssociationProposal
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteDirectConnectGatewayAssociationProposal
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDirectConnectGatewayAssociationProposalResponse' smart constructor.
data DeleteDirectConnectGatewayAssociationProposalResponse = DeleteDirectConnectGatewayAssociationProposalResponse'
  { -- | The ID of the associated gateway.
    directConnectGatewayAssociationProposal :: Prelude.Maybe DirectConnectGatewayAssociationProposal,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDirectConnectGatewayAssociationProposalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayAssociationProposal', 'deleteDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal' - The ID of the associated gateway.
--
-- 'httpStatus', 'deleteDirectConnectGatewayAssociationProposalResponse_httpStatus' - The response's http status code.
newDeleteDirectConnectGatewayAssociationProposalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDirectConnectGatewayAssociationProposalResponse
newDeleteDirectConnectGatewayAssociationProposalResponse
  pHttpStatus_ =
    DeleteDirectConnectGatewayAssociationProposalResponse'
      { directConnectGatewayAssociationProposal =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The ID of the associated gateway.
deleteDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposalResponse (Prelude.Maybe DirectConnectGatewayAssociationProposal)
deleteDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal = Lens.lens (\DeleteDirectConnectGatewayAssociationProposalResponse' {directConnectGatewayAssociationProposal} -> directConnectGatewayAssociationProposal) (\s@DeleteDirectConnectGatewayAssociationProposalResponse' {} a -> s {directConnectGatewayAssociationProposal = a} :: DeleteDirectConnectGatewayAssociationProposalResponse)

-- | The response's http status code.
deleteDirectConnectGatewayAssociationProposalResponse_httpStatus :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposalResponse Prelude.Int
deleteDirectConnectGatewayAssociationProposalResponse_httpStatus = Lens.lens (\DeleteDirectConnectGatewayAssociationProposalResponse' {httpStatus} -> httpStatus) (\s@DeleteDirectConnectGatewayAssociationProposalResponse' {} a -> s {httpStatus = a} :: DeleteDirectConnectGatewayAssociationProposalResponse)

instance
  Prelude.NFData
    DeleteDirectConnectGatewayAssociationProposalResponse
  where
  rnf
    DeleteDirectConnectGatewayAssociationProposalResponse' {..} =
      Prelude.rnf directConnectGatewayAssociationProposal
        `Prelude.seq` Prelude.rnf httpStatus
