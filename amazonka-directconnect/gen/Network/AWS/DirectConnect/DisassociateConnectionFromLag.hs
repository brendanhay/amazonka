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
-- Module      : Network.AWS.DirectConnect.DisassociateConnectionFromLag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a connection from a link aggregation group (LAG). The
-- connection is interrupted and re-established as a standalone connection
-- (the connection is not deleted; to delete the connection, use the
-- DeleteConnection request). If the LAG has associated virtual interfaces
-- or hosted connections, they remain associated with the LAG. A
-- disassociated connection owned by an AWS Direct Connect Partner is
-- automatically converted to an interconnect.
--
-- If disassociating the connection would cause the LAG to fall below its
-- setting for minimum number of operational connections, the request
-- fails, except when it\'s the last member of the LAG. If all connections
-- are disassociated, the LAG continues to exist as an empty LAG with no
-- physical connections.
module Network.AWS.DirectConnect.DisassociateConnectionFromLag
  ( -- * Creating a Request
    DisassociateConnectionFromLag (..),
    newDisassociateConnectionFromLag,

    -- * Request Lenses
    disassociateConnectionFromLag_connectionId,
    disassociateConnectionFromLag_lagId,

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

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateConnectionFromLag' smart constructor.
data DisassociateConnectionFromLag = DisassociateConnectionFromLag'
  { -- | The ID of the connection.
    connectionId :: Prelude.Text,
    -- | The ID of the LAG.
    lagId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateConnectionFromLag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'disassociateConnectionFromLag_connectionId' - The ID of the connection.
--
-- 'lagId', 'disassociateConnectionFromLag_lagId' - The ID of the LAG.
newDisassociateConnectionFromLag ::
  -- | 'connectionId'
  Prelude.Text ->
  -- | 'lagId'
  Prelude.Text ->
  DisassociateConnectionFromLag
newDisassociateConnectionFromLag
  pConnectionId_
  pLagId_ =
    DisassociateConnectionFromLag'
      { connectionId =
          pConnectionId_,
        lagId = pLagId_
      }

-- | The ID of the connection.
disassociateConnectionFromLag_connectionId :: Lens.Lens' DisassociateConnectionFromLag Prelude.Text
disassociateConnectionFromLag_connectionId = Lens.lens (\DisassociateConnectionFromLag' {connectionId} -> connectionId) (\s@DisassociateConnectionFromLag' {} a -> s {connectionId = a} :: DisassociateConnectionFromLag)

-- | The ID of the LAG.
disassociateConnectionFromLag_lagId :: Lens.Lens' DisassociateConnectionFromLag Prelude.Text
disassociateConnectionFromLag_lagId = Lens.lens (\DisassociateConnectionFromLag' {lagId} -> lagId) (\s@DisassociateConnectionFromLag' {} a -> s {lagId = a} :: DisassociateConnectionFromLag)

instance
  Prelude.AWSRequest
    DisassociateConnectionFromLag
  where
  type Rs DisassociateConnectionFromLag = Connection
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance
  Prelude.Hashable
    DisassociateConnectionFromLag

instance Prelude.NFData DisassociateConnectionFromLag

instance
  Prelude.ToHeaders
    DisassociateConnectionFromLag
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OvertureService.DisassociateConnectionFromLag" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateConnectionFromLag where
  toJSON DisassociateConnectionFromLag' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("connectionId" Prelude..= connectionId),
            Prelude.Just ("lagId" Prelude..= lagId)
          ]
      )

instance Prelude.ToPath DisassociateConnectionFromLag where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisassociateConnectionFromLag
  where
  toQuery = Prelude.const Prelude.mempty
