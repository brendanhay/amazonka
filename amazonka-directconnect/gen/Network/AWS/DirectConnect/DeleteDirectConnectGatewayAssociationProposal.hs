{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association proposal request between the specified Direct
-- Connect gateway and virtual private gateway or transit gateway.
module Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociationProposal
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

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDirectConnectGatewayAssociationProposal' smart constructor.
data DeleteDirectConnectGatewayAssociationProposal = DeleteDirectConnectGatewayAssociationProposal'
  { -- | The ID of the proposal.
    proposalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeleteDirectConnectGatewayAssociationProposal
  where
  type
    Rs DeleteDirectConnectGatewayAssociationProposal =
      DeleteDirectConnectGatewayAssociationProposalResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDirectConnectGatewayAssociationProposalResponse'
            Prelude.<$> ( x
                            Prelude..?> "directConnectGatewayAssociationProposal"
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteDirectConnectGatewayAssociationProposal

instance
  Prelude.NFData
    DeleteDirectConnectGatewayAssociationProposal

instance
  Prelude.ToHeaders
    DeleteDirectConnectGatewayAssociationProposal
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OvertureService.DeleteDirectConnectGatewayAssociationProposal" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DeleteDirectConnectGatewayAssociationProposal
  where
  toJSON
    DeleteDirectConnectGatewayAssociationProposal' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [Prelude.Just ("proposalId" Prelude..= proposalId)]
        )

instance
  Prelude.ToPath
    DeleteDirectConnectGatewayAssociationProposal
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
