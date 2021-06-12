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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDirectConnectGatewayAssociationProposal' smart constructor.
data DeleteDirectConnectGatewayAssociationProposal = DeleteDirectConnectGatewayAssociationProposal'
  { -- | The ID of the proposal.
    proposalId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteDirectConnectGatewayAssociationProposal
newDeleteDirectConnectGatewayAssociationProposal
  pProposalId_ =
    DeleteDirectConnectGatewayAssociationProposal'
      { proposalId =
          pProposalId_
      }

-- | The ID of the proposal.
deleteDirectConnectGatewayAssociationProposal_proposalId :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposal Core.Text
deleteDirectConnectGatewayAssociationProposal_proposalId = Lens.lens (\DeleteDirectConnectGatewayAssociationProposal' {proposalId} -> proposalId) (\s@DeleteDirectConnectGatewayAssociationProposal' {} a -> s {proposalId = a} :: DeleteDirectConnectGatewayAssociationProposal)

instance
  Core.AWSRequest
    DeleteDirectConnectGatewayAssociationProposal
  where
  type
    AWSResponse
      DeleteDirectConnectGatewayAssociationProposal =
      DeleteDirectConnectGatewayAssociationProposalResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDirectConnectGatewayAssociationProposalResponse'
            Core.<$> ( x
                         Core..?> "directConnectGatewayAssociationProposal"
                     )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteDirectConnectGatewayAssociationProposal

instance
  Core.NFData
    DeleteDirectConnectGatewayAssociationProposal

instance
  Core.ToHeaders
    DeleteDirectConnectGatewayAssociationProposal
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DeleteDirectConnectGatewayAssociationProposal" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DeleteDirectConnectGatewayAssociationProposal
  where
  toJSON
    DeleteDirectConnectGatewayAssociationProposal' {..} =
      Core.object
        ( Core.catMaybes
            [Core.Just ("proposalId" Core..= proposalId)]
        )

instance
  Core.ToPath
    DeleteDirectConnectGatewayAssociationProposal
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeleteDirectConnectGatewayAssociationProposal
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDirectConnectGatewayAssociationProposalResponse' smart constructor.
data DeleteDirectConnectGatewayAssociationProposalResponse = DeleteDirectConnectGatewayAssociationProposalResponse'
  { -- | The ID of the associated gateway.
    directConnectGatewayAssociationProposal :: Core.Maybe DirectConnectGatewayAssociationProposal,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteDirectConnectGatewayAssociationProposalResponse
newDeleteDirectConnectGatewayAssociationProposalResponse
  pHttpStatus_ =
    DeleteDirectConnectGatewayAssociationProposalResponse'
      { directConnectGatewayAssociationProposal =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The ID of the associated gateway.
deleteDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposalResponse (Core.Maybe DirectConnectGatewayAssociationProposal)
deleteDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal = Lens.lens (\DeleteDirectConnectGatewayAssociationProposalResponse' {directConnectGatewayAssociationProposal} -> directConnectGatewayAssociationProposal) (\s@DeleteDirectConnectGatewayAssociationProposalResponse' {} a -> s {directConnectGatewayAssociationProposal = a} :: DeleteDirectConnectGatewayAssociationProposalResponse)

-- | The response's http status code.
deleteDirectConnectGatewayAssociationProposalResponse_httpStatus :: Lens.Lens' DeleteDirectConnectGatewayAssociationProposalResponse Core.Int
deleteDirectConnectGatewayAssociationProposalResponse_httpStatus = Lens.lens (\DeleteDirectConnectGatewayAssociationProposalResponse' {httpStatus} -> httpStatus) (\s@DeleteDirectConnectGatewayAssociationProposalResponse' {} a -> s {httpStatus = a} :: DeleteDirectConnectGatewayAssociationProposalResponse)

instance
  Core.NFData
    DeleteDirectConnectGatewayAssociationProposalResponse
