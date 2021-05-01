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

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { -- | The name of the service provider associated with the requested
    -- connection.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the LAG.
    lagId :: Prelude.Maybe Prelude.Text,
    -- | The tags to associate with the lag.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The location of the connection.
    location :: Prelude.Text,
    -- | The bandwidth of the connection.
    bandwidth :: Prelude.Text,
    -- | The name of the connection.
    connectionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'bandwidth'
  Prelude.Text ->
  -- | 'connectionName'
  Prelude.Text ->
  CreateConnection
newCreateConnection
  pLocation_
  pBandwidth_
  pConnectionName_ =
    CreateConnection'
      { providerName = Prelude.Nothing,
        lagId = Prelude.Nothing,
        tags = Prelude.Nothing,
        location = pLocation_,
        bandwidth = pBandwidth_,
        connectionName = pConnectionName_
      }

-- | The name of the service provider associated with the requested
-- connection.
createConnection_providerName :: Lens.Lens' CreateConnection (Prelude.Maybe Prelude.Text)
createConnection_providerName = Lens.lens (\CreateConnection' {providerName} -> providerName) (\s@CreateConnection' {} a -> s {providerName = a} :: CreateConnection)

-- | The ID of the LAG.
createConnection_lagId :: Lens.Lens' CreateConnection (Prelude.Maybe Prelude.Text)
createConnection_lagId = Lens.lens (\CreateConnection' {lagId} -> lagId) (\s@CreateConnection' {} a -> s {lagId = a} :: CreateConnection)

-- | The tags to associate with the lag.
createConnection_tags :: Lens.Lens' CreateConnection (Prelude.Maybe (Prelude.NonEmpty Tag))
createConnection_tags = Lens.lens (\CreateConnection' {tags} -> tags) (\s@CreateConnection' {} a -> s {tags = a} :: CreateConnection) Prelude.. Lens.mapping Prelude._Coerce

-- | The location of the connection.
createConnection_location :: Lens.Lens' CreateConnection Prelude.Text
createConnection_location = Lens.lens (\CreateConnection' {location} -> location) (\s@CreateConnection' {} a -> s {location = a} :: CreateConnection)

-- | The bandwidth of the connection.
createConnection_bandwidth :: Lens.Lens' CreateConnection Prelude.Text
createConnection_bandwidth = Lens.lens (\CreateConnection' {bandwidth} -> bandwidth) (\s@CreateConnection' {} a -> s {bandwidth = a} :: CreateConnection)

-- | The name of the connection.
createConnection_connectionName :: Lens.Lens' CreateConnection Prelude.Text
createConnection_connectionName = Lens.lens (\CreateConnection' {connectionName} -> connectionName) (\s@CreateConnection' {} a -> s {connectionName = a} :: CreateConnection)

instance Prelude.AWSRequest CreateConnection where
  type Rs CreateConnection = Connection
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable CreateConnection

instance Prelude.NFData CreateConnection

instance Prelude.ToHeaders CreateConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OvertureService.CreateConnection" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("providerName" Prelude..=)
              Prelude.<$> providerName,
            ("lagId" Prelude..=) Prelude.<$> lagId,
            ("tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just ("location" Prelude..= location),
            Prelude.Just ("bandwidth" Prelude..= bandwidth),
            Prelude.Just
              ("connectionName" Prelude..= connectionName)
          ]
      )

instance Prelude.ToPath CreateConnection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateConnection where
  toQuery = Prelude.const Prelude.mempty
