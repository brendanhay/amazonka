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
-- Module      : Amazonka.DirectConnect.AssociateHostedConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a hosted connection and its virtual interfaces with a link
-- aggregation group (LAG) or interconnect. If the target interconnect or
-- LAG has an existing hosted connection with a conflicting VLAN number or
-- IP address, the operation fails. This action temporarily interrupts the
-- hosted connection\'s connectivity to Amazon Web Services as it is being
-- migrated.
--
-- Intended for use by Direct Connect Partners only.
module Amazonka.DirectConnect.AssociateHostedConnection
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
    connection_awsDevice,
    connection_awsDeviceV2,
    connection_awsLogicalDeviceId,
    connection_bandwidth,
    connection_connectionId,
    connection_connectionName,
    connection_connectionState,
    connection_encryptionMode,
    connection_hasLogicalRedundancy,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_loaIssueTime,
    connection_location,
    connection_macSecCapable,
    connection_macSecKeys,
    connection_ownerAccount,
    connection_partnerName,
    connection_portEncryptionStatus,
    connection_providerName,
    connection_region,
    connection_tags,
    connection_vlan,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateHostedConnection' smart constructor.
data AssociateHostedConnection = AssociateHostedConnection'
  { -- | The ID of the hosted connection.
    connectionId :: Prelude.Text,
    -- | The ID of the interconnect or the LAG.
    parentConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'parentConnectionId'
  Prelude.Text ->
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
associateHostedConnection_connectionId :: Lens.Lens' AssociateHostedConnection Prelude.Text
associateHostedConnection_connectionId = Lens.lens (\AssociateHostedConnection' {connectionId} -> connectionId) (\s@AssociateHostedConnection' {} a -> s {connectionId = a} :: AssociateHostedConnection)

-- | The ID of the interconnect or the LAG.
associateHostedConnection_parentConnectionId :: Lens.Lens' AssociateHostedConnection Prelude.Text
associateHostedConnection_parentConnectionId = Lens.lens (\AssociateHostedConnection' {parentConnectionId} -> parentConnectionId) (\s@AssociateHostedConnection' {} a -> s {parentConnectionId = a} :: AssociateHostedConnection)

instance Core.AWSRequest AssociateHostedConnection where
  type
    AWSResponse AssociateHostedConnection =
      Connection
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable AssociateHostedConnection where
  hashWithSalt _salt AssociateHostedConnection' {..} =
    _salt
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` parentConnectionId

instance Prelude.NFData AssociateHostedConnection where
  rnf AssociateHostedConnection' {..} =
    Prelude.rnf connectionId `Prelude.seq`
      Prelude.rnf parentConnectionId

instance Data.ToHeaders AssociateHostedConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.AssociateHostedConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateHostedConnection where
  toJSON AssociateHostedConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("connectionId" Data..= connectionId),
            Prelude.Just
              ("parentConnectionId" Data..= parentConnectionId)
          ]
      )

instance Data.ToPath AssociateHostedConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateHostedConnection where
  toQuery = Prelude.const Prelude.mempty
