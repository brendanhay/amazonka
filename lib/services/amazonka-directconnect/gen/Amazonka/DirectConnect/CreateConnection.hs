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
-- Module      : Amazonka.DirectConnect.CreateConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection between a customer network and a specific Direct
-- Connect location.
--
-- A connection links your internal network to an Direct Connect location
-- over a standard Ethernet fiber-optic cable. One end of the cable is
-- connected to your router, the other to an Direct Connect router.
--
-- To find the locations for your Region, use DescribeLocations.
--
-- You can automatically add the new connection to a link aggregation group
-- (LAG) by specifying a LAG ID in the request. This ensures that the new
-- connection is allocated on the same Direct Connect endpoint that hosts
-- the specified LAG. If there are no available ports on the endpoint, the
-- request fails and no connection is created.
module Amazonka.DirectConnect.CreateConnection
  ( -- * Creating a Request
    CreateConnection (..),
    newCreateConnection,

    -- * Request Lenses
    createConnection_lagId,
    createConnection_providerName,
    createConnection_requestMACSec,
    createConnection_tags,
    createConnection_location,
    createConnection_bandwidth,
    createConnection_connectionName,

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

-- | /See:/ 'newCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { -- | The ID of the LAG.
    lagId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service provider associated with the requested
    -- connection.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether you want the connection to support MAC Security
    -- (MACsec).
    --
    -- MAC Security (MACsec) is only available on dedicated connections. For
    -- information about MAC Security (MACsec) prerequisties, see
    -- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/direct-connect-mac-sec-getting-started.html#mac-sec-prerequisites MACsec prerequisties>
    -- in the /Direct Connect User Guide/.
    requestMACSec :: Prelude.Maybe Prelude.Bool,
    -- | The tags to associate with the lag.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The location of the connection.
    location :: Prelude.Text,
    -- | The bandwidth of the connection.
    bandwidth :: Prelude.Text,
    -- | The name of the connection.
    connectionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lagId', 'createConnection_lagId' - The ID of the LAG.
--
-- 'providerName', 'createConnection_providerName' - The name of the service provider associated with the requested
-- connection.
--
-- 'requestMACSec', 'createConnection_requestMACSec' - Indicates whether you want the connection to support MAC Security
-- (MACsec).
--
-- MAC Security (MACsec) is only available on dedicated connections. For
-- information about MAC Security (MACsec) prerequisties, see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/direct-connect-mac-sec-getting-started.html#mac-sec-prerequisites MACsec prerequisties>
-- in the /Direct Connect User Guide/.
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
      { lagId = Prelude.Nothing,
        providerName = Prelude.Nothing,
        requestMACSec = Prelude.Nothing,
        tags = Prelude.Nothing,
        location = pLocation_,
        bandwidth = pBandwidth_,
        connectionName = pConnectionName_
      }

-- | The ID of the LAG.
createConnection_lagId :: Lens.Lens' CreateConnection (Prelude.Maybe Prelude.Text)
createConnection_lagId = Lens.lens (\CreateConnection' {lagId} -> lagId) (\s@CreateConnection' {} a -> s {lagId = a} :: CreateConnection)

-- | The name of the service provider associated with the requested
-- connection.
createConnection_providerName :: Lens.Lens' CreateConnection (Prelude.Maybe Prelude.Text)
createConnection_providerName = Lens.lens (\CreateConnection' {providerName} -> providerName) (\s@CreateConnection' {} a -> s {providerName = a} :: CreateConnection)

-- | Indicates whether you want the connection to support MAC Security
-- (MACsec).
--
-- MAC Security (MACsec) is only available on dedicated connections. For
-- information about MAC Security (MACsec) prerequisties, see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/direct-connect-mac-sec-getting-started.html#mac-sec-prerequisites MACsec prerequisties>
-- in the /Direct Connect User Guide/.
createConnection_requestMACSec :: Lens.Lens' CreateConnection (Prelude.Maybe Prelude.Bool)
createConnection_requestMACSec = Lens.lens (\CreateConnection' {requestMACSec} -> requestMACSec) (\s@CreateConnection' {} a -> s {requestMACSec = a} :: CreateConnection)

-- | The tags to associate with the lag.
createConnection_tags :: Lens.Lens' CreateConnection (Prelude.Maybe (Prelude.NonEmpty Tag))
createConnection_tags = Lens.lens (\CreateConnection' {tags} -> tags) (\s@CreateConnection' {} a -> s {tags = a} :: CreateConnection) Prelude.. Lens.mapping Lens.coerced

-- | The location of the connection.
createConnection_location :: Lens.Lens' CreateConnection Prelude.Text
createConnection_location = Lens.lens (\CreateConnection' {location} -> location) (\s@CreateConnection' {} a -> s {location = a} :: CreateConnection)

-- | The bandwidth of the connection.
createConnection_bandwidth :: Lens.Lens' CreateConnection Prelude.Text
createConnection_bandwidth = Lens.lens (\CreateConnection' {bandwidth} -> bandwidth) (\s@CreateConnection' {} a -> s {bandwidth = a} :: CreateConnection)

-- | The name of the connection.
createConnection_connectionName :: Lens.Lens' CreateConnection Prelude.Text
createConnection_connectionName = Lens.lens (\CreateConnection' {connectionName} -> connectionName) (\s@CreateConnection' {} a -> s {connectionName = a} :: CreateConnection)

instance Core.AWSRequest CreateConnection where
  type AWSResponse CreateConnection = Connection
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateConnection where
  hashWithSalt _salt CreateConnection' {..} =
    _salt
      `Prelude.hashWithSalt` lagId
      `Prelude.hashWithSalt` providerName
      `Prelude.hashWithSalt` requestMACSec
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` bandwidth
      `Prelude.hashWithSalt` connectionName

instance Prelude.NFData CreateConnection where
  rnf CreateConnection' {..} =
    Prelude.rnf lagId `Prelude.seq`
      Prelude.rnf providerName `Prelude.seq`
        Prelude.rnf requestMACSec `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf location `Prelude.seq`
              Prelude.rnf bandwidth `Prelude.seq`
                Prelude.rnf connectionName

instance Data.ToHeaders CreateConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.CreateConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("lagId" Data..=) Prelude.<$> lagId,
            ("providerName" Data..=) Prelude.<$> providerName,
            ("requestMACSec" Data..=) Prelude.<$> requestMACSec,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("location" Data..= location),
            Prelude.Just ("bandwidth" Data..= bandwidth),
            Prelude.Just
              ("connectionName" Data..= connectionName)
          ]
      )

instance Data.ToPath CreateConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateConnection where
  toQuery = Prelude.const Prelude.mempty
