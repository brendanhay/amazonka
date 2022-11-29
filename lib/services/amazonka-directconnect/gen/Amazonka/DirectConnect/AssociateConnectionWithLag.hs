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
-- Module      : Amazonka.DirectConnect.AssociateConnectionWithLag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an existing connection with a link aggregation group (LAG).
-- The connection is interrupted and re-established as a member of the LAG
-- (connectivity to Amazon Web Services is interrupted). The connection
-- must be hosted on the same Direct Connect endpoint as the LAG, and its
-- bandwidth must match the bandwidth for the LAG. You can re-associate a
-- connection that\'s currently associated with a different LAG; however,
-- if removing the connection would cause the original LAG to fall below
-- its setting for minimum number of operational connections, the request
-- fails.
--
-- Any virtual interfaces that are directly associated with the connection
-- are automatically re-associated with the LAG. If the connection was
-- originally associated with a different LAG, the virtual interfaces
-- remain associated with the original LAG.
--
-- For interconnects, any hosted connections are automatically
-- re-associated with the LAG. If the interconnect was originally
-- associated with a different LAG, the hosted connections remain
-- associated with the original LAG.
module Amazonka.DirectConnect.AssociateConnectionWithLag
  ( -- * Creating a Request
    AssociateConnectionWithLag (..),
    newAssociateConnectionWithLag,

    -- * Request Lenses
    associateConnectionWithLag_connectionId,
    associateConnectionWithLag_lagId,

    -- * Destructuring the Response
    Connection (..),
    newConnection,

    -- * Response Lenses
    connection_tags,
    connection_macSecKeys,
    connection_macSecCapable,
    connection_providerName,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_connectionState,
    connection_hasLogicalRedundancy,
    connection_vlan,
    connection_loaIssueTime,
    connection_awsDevice,
    connection_connectionId,
    connection_location,
    connection_region,
    connection_partnerName,
    connection_ownerAccount,
    connection_awsLogicalDeviceId,
    connection_encryptionMode,
    connection_connectionName,
    connection_awsDeviceV2,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateConnectionWithLag' smart constructor.
data AssociateConnectionWithLag = AssociateConnectionWithLag'
  { -- | The ID of the connection.
    connectionId :: Prelude.Text,
    -- | The ID of the LAG with which to associate the connection.
    lagId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateConnectionWithLag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'associateConnectionWithLag_connectionId' - The ID of the connection.
--
-- 'lagId', 'associateConnectionWithLag_lagId' - The ID of the LAG with which to associate the connection.
newAssociateConnectionWithLag ::
  -- | 'connectionId'
  Prelude.Text ->
  -- | 'lagId'
  Prelude.Text ->
  AssociateConnectionWithLag
newAssociateConnectionWithLag pConnectionId_ pLagId_ =
  AssociateConnectionWithLag'
    { connectionId =
        pConnectionId_,
      lagId = pLagId_
    }

-- | The ID of the connection.
associateConnectionWithLag_connectionId :: Lens.Lens' AssociateConnectionWithLag Prelude.Text
associateConnectionWithLag_connectionId = Lens.lens (\AssociateConnectionWithLag' {connectionId} -> connectionId) (\s@AssociateConnectionWithLag' {} a -> s {connectionId = a} :: AssociateConnectionWithLag)

-- | The ID of the LAG with which to associate the connection.
associateConnectionWithLag_lagId :: Lens.Lens' AssociateConnectionWithLag Prelude.Text
associateConnectionWithLag_lagId = Lens.lens (\AssociateConnectionWithLag' {lagId} -> lagId) (\s@AssociateConnectionWithLag' {} a -> s {lagId = a} :: AssociateConnectionWithLag)

instance Core.AWSRequest AssociateConnectionWithLag where
  type
    AWSResponse AssociateConnectionWithLag =
      Connection
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable AssociateConnectionWithLag where
  hashWithSalt _salt AssociateConnectionWithLag' {..} =
    _salt `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` lagId

instance Prelude.NFData AssociateConnectionWithLag where
  rnf AssociateConnectionWithLag' {..} =
    Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf lagId

instance Core.ToHeaders AssociateConnectionWithLag where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.AssociateConnectionWithLag" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateConnectionWithLag where
  toJSON AssociateConnectionWithLag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("connectionId" Core..= connectionId),
            Prelude.Just ("lagId" Core..= lagId)
          ]
      )

instance Core.ToPath AssociateConnectionWithLag where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateConnectionWithLag where
  toQuery = Prelude.const Prelude.mempty
