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
-- Module      : Amazonka.DirectConnect.UpdateConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Direct Connect dedicated connection configuration.
--
-- You can update the following parameters for a connection:
--
-- -   The connection name
--
-- -   The connection\'s MAC Security (MACsec) encryption mode.
module Amazonka.DirectConnect.UpdateConnection
  ( -- * Creating a Request
    UpdateConnection (..),
    newUpdateConnection,

    -- * Request Lenses
    updateConnection_connectionName,
    updateConnection_encryptionMode,
    updateConnection_connectionId,

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

-- | /See:/ 'newUpdateConnection' smart constructor.
data UpdateConnection = UpdateConnection'
  { -- | The name of the connection.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | The connection MAC Security (MACsec) encryption mode.
    --
    -- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
    encryptionMode :: Prelude.Maybe Prelude.Text,
    -- | The ID of the dedicated connection.
    --
    -- You can use DescribeConnections to retrieve the connection ID.
    connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionName', 'updateConnection_connectionName' - The name of the connection.
--
-- 'encryptionMode', 'updateConnection_encryptionMode' - The connection MAC Security (MACsec) encryption mode.
--
-- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
--
-- 'connectionId', 'updateConnection_connectionId' - The ID of the dedicated connection.
--
-- You can use DescribeConnections to retrieve the connection ID.
newUpdateConnection ::
  -- | 'connectionId'
  Prelude.Text ->
  UpdateConnection
newUpdateConnection pConnectionId_ =
  UpdateConnection'
    { connectionName = Prelude.Nothing,
      encryptionMode = Prelude.Nothing,
      connectionId = pConnectionId_
    }

-- | The name of the connection.
updateConnection_connectionName :: Lens.Lens' UpdateConnection (Prelude.Maybe Prelude.Text)
updateConnection_connectionName = Lens.lens (\UpdateConnection' {connectionName} -> connectionName) (\s@UpdateConnection' {} a -> s {connectionName = a} :: UpdateConnection)

-- | The connection MAC Security (MACsec) encryption mode.
--
-- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
updateConnection_encryptionMode :: Lens.Lens' UpdateConnection (Prelude.Maybe Prelude.Text)
updateConnection_encryptionMode = Lens.lens (\UpdateConnection' {encryptionMode} -> encryptionMode) (\s@UpdateConnection' {} a -> s {encryptionMode = a} :: UpdateConnection)

-- | The ID of the dedicated connection.
--
-- You can use DescribeConnections to retrieve the connection ID.
updateConnection_connectionId :: Lens.Lens' UpdateConnection Prelude.Text
updateConnection_connectionId = Lens.lens (\UpdateConnection' {connectionId} -> connectionId) (\s@UpdateConnection' {} a -> s {connectionId = a} :: UpdateConnection)

instance Core.AWSRequest UpdateConnection where
  type AWSResponse UpdateConnection = Connection
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateConnection where
  hashWithSalt _salt UpdateConnection' {..} =
    _salt `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` encryptionMode
      `Prelude.hashWithSalt` connectionId

instance Prelude.NFData UpdateConnection where
  rnf UpdateConnection' {..} =
    Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf encryptionMode
      `Prelude.seq` Prelude.rnf connectionId

instance Data.ToHeaders UpdateConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.UpdateConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConnection where
  toJSON UpdateConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("connectionName" Data..=)
              Prelude.<$> connectionName,
            ("encryptionMode" Data..=)
              Prelude.<$> encryptionMode,
            Prelude.Just ("connectionId" Data..= connectionId)
          ]
      )

instance Data.ToPath UpdateConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateConnection where
  toQuery = Prelude.const Prelude.mempty
