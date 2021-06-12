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
-- Module      : Network.AWS.DirectConnect.AssociateHostedConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a hosted connection and its virtual interfaces with a link
-- aggregation group (LAG) or interconnect. If the target interconnect or
-- LAG has an existing hosted connection with a conflicting VLAN number or
-- IP address, the operation fails. This action temporarily interrupts the
-- hosted connection\'s connectivity to AWS as it is being migrated.
--
-- Intended for use by AWS Direct Connect Partners only.
module Network.AWS.DirectConnect.AssociateHostedConnection
  ( -- * Creating a Request
    AssociateHostedConnection (..),
    newAssociateHostedConnection,

    -- * Request Lenses
    associateHostedConnection_connectionId,
    associateHostedConnection_parentConnectionId,

    -- * Destructuring the Response
    Connection (..),
    newConnection,

    -- * Response Lenses
    connection_bandwidth,
    connection_connectionState,
    connection_awsDeviceV2,
    connection_connectionName,
    connection_providerName,
    connection_connectionId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_location,
    connection_vlan,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateHostedConnection' smart constructor.
data AssociateHostedConnection = AssociateHostedConnection'
  { -- | The ID of the hosted connection.
    connectionId :: Core.Text,
    -- | The ID of the interconnect or the LAG.
    parentConnectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateHostedConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'associateHostedConnection_connectionId' - The ID of the hosted connection.
--
-- 'parentConnectionId', 'associateHostedConnection_parentConnectionId' - The ID of the interconnect or the LAG.
newAssociateHostedConnection ::
  -- | 'connectionId'
  Core.Text ->
  -- | 'parentConnectionId'
  Core.Text ->
  AssociateHostedConnection
newAssociateHostedConnection
  pConnectionId_
  pParentConnectionId_ =
    AssociateHostedConnection'
      { connectionId =
          pConnectionId_,
        parentConnectionId = pParentConnectionId_
      }

-- | The ID of the hosted connection.
associateHostedConnection_connectionId :: Lens.Lens' AssociateHostedConnection Core.Text
associateHostedConnection_connectionId = Lens.lens (\AssociateHostedConnection' {connectionId} -> connectionId) (\s@AssociateHostedConnection' {} a -> s {connectionId = a} :: AssociateHostedConnection)

-- | The ID of the interconnect or the LAG.
associateHostedConnection_parentConnectionId :: Lens.Lens' AssociateHostedConnection Core.Text
associateHostedConnection_parentConnectionId = Lens.lens (\AssociateHostedConnection' {parentConnectionId} -> parentConnectionId) (\s@AssociateHostedConnection' {} a -> s {parentConnectionId = a} :: AssociateHostedConnection)

instance Core.AWSRequest AssociateHostedConnection where
  type
    AWSResponse AssociateHostedConnection =
      Connection
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable AssociateHostedConnection

instance Core.NFData AssociateHostedConnection

instance Core.ToHeaders AssociateHostedConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.AssociateHostedConnection" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateHostedConnection where
  toJSON AssociateHostedConnection' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("connectionId" Core..= connectionId),
            Core.Just
              ("parentConnectionId" Core..= parentConnectionId)
          ]
      )

instance Core.ToPath AssociateHostedConnection where
  toPath = Core.const "/"

instance Core.ToQuery AssociateHostedConnection where
  toQuery = Core.const Core.mempty
