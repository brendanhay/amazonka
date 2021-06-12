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
-- Module      : Network.AWS.DirectConnect.CreateConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection between a customer network and a specific AWS
-- Direct Connect location.
--
-- A connection links your internal network to an AWS Direct Connect
-- location over a standard Ethernet fiber-optic cable. One end of the
-- cable is connected to your router, the other to an AWS Direct Connect
-- router.
--
-- To find the locations for your Region, use DescribeLocations.
--
-- You can automatically add the new connection to a link aggregation group
-- (LAG) by specifying a LAG ID in the request. This ensures that the new
-- connection is allocated on the same AWS Direct Connect endpoint that
-- hosts the specified LAG. If there are no available ports on the
-- endpoint, the request fails and no connection is created.
module Network.AWS.DirectConnect.CreateConnection
  ( -- * Creating a Request
    CreateConnection (..),
    newCreateConnection,

    -- * Request Lenses
    createConnection_providerName,
    createConnection_lagId,
    createConnection_tags,
    createConnection_location,
    createConnection_bandwidth,
    createConnection_connectionName,

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

-- | /See:/ 'newCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { -- | The name of the service provider associated with the requested
    -- connection.
    providerName :: Core.Maybe Core.Text,
    -- | The ID of the LAG.
    lagId :: Core.Maybe Core.Text,
    -- | The tags to associate with the lag.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The location of the connection.
    location :: Core.Text,
    -- | The bandwidth of the connection.
    bandwidth :: Core.Text,
    -- | The name of the connection.
    connectionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'providerName', 'createConnection_providerName' - The name of the service provider associated with the requested
-- connection.
--
-- 'lagId', 'createConnection_lagId' - The ID of the LAG.
--
-- 'tags', 'createConnection_tags' - The tags to associate with the lag.
--
-- 'location', 'createConnection_location' - The location of the connection.
--
-- 'bandwidth', 'createConnection_bandwidth' - The bandwidth of the connection.
--
-- 'connectionName', 'createConnection_connectionName' - The name of the connection.
newCreateConnection ::
  -- | 'location'
  Core.Text ->
  -- | 'bandwidth'
  Core.Text ->
  -- | 'connectionName'
  Core.Text ->
  CreateConnection
newCreateConnection
  pLocation_
  pBandwidth_
  pConnectionName_ =
    CreateConnection'
      { providerName = Core.Nothing,
        lagId = Core.Nothing,
        tags = Core.Nothing,
        location = pLocation_,
        bandwidth = pBandwidth_,
        connectionName = pConnectionName_
      }

-- | The name of the service provider associated with the requested
-- connection.
createConnection_providerName :: Lens.Lens' CreateConnection (Core.Maybe Core.Text)
createConnection_providerName = Lens.lens (\CreateConnection' {providerName} -> providerName) (\s@CreateConnection' {} a -> s {providerName = a} :: CreateConnection)

-- | The ID of the LAG.
createConnection_lagId :: Lens.Lens' CreateConnection (Core.Maybe Core.Text)
createConnection_lagId = Lens.lens (\CreateConnection' {lagId} -> lagId) (\s@CreateConnection' {} a -> s {lagId = a} :: CreateConnection)

-- | The tags to associate with the lag.
createConnection_tags :: Lens.Lens' CreateConnection (Core.Maybe (Core.NonEmpty Tag))
createConnection_tags = Lens.lens (\CreateConnection' {tags} -> tags) (\s@CreateConnection' {} a -> s {tags = a} :: CreateConnection) Core.. Lens.mapping Lens._Coerce

-- | The location of the connection.
createConnection_location :: Lens.Lens' CreateConnection Core.Text
createConnection_location = Lens.lens (\CreateConnection' {location} -> location) (\s@CreateConnection' {} a -> s {location = a} :: CreateConnection)

-- | The bandwidth of the connection.
createConnection_bandwidth :: Lens.Lens' CreateConnection Core.Text
createConnection_bandwidth = Lens.lens (\CreateConnection' {bandwidth} -> bandwidth) (\s@CreateConnection' {} a -> s {bandwidth = a} :: CreateConnection)

-- | The name of the connection.
createConnection_connectionName :: Lens.Lens' CreateConnection Core.Text
createConnection_connectionName = Lens.lens (\CreateConnection' {connectionName} -> connectionName) (\s@CreateConnection' {} a -> s {connectionName = a} :: CreateConnection)

instance Core.AWSRequest CreateConnection where
  type AWSResponse CreateConnection = Connection
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateConnection

instance Core.NFData CreateConnection

instance Core.ToHeaders CreateConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.CreateConnection" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("providerName" Core..=) Core.<$> providerName,
            ("lagId" Core..=) Core.<$> lagId,
            ("tags" Core..=) Core.<$> tags,
            Core.Just ("location" Core..= location),
            Core.Just ("bandwidth" Core..= bandwidth),
            Core.Just ("connectionName" Core..= connectionName)
          ]
      )

instance Core.ToPath CreateConnection where
  toPath = Core.const "/"

instance Core.ToQuery CreateConnection where
  toQuery = Core.const Core.mempty
