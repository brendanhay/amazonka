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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    updateConnection_encryptionMode,
    updateConnection_connectionName,
    updateConnection_connectionId,

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
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConnection' smart constructor.
data UpdateConnection = UpdateConnection'
  { -- | The connection MAC Security (MACsec) encryption mode.
    --
    -- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
    encryptionMode :: Prelude.Maybe Prelude.Text,
    -- | The name of the connection.
    connectionName :: Prelude.Maybe Prelude.Text,
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
-- 'encryptionMode', 'updateConnection_encryptionMode' - The connection MAC Security (MACsec) encryption mode.
--
-- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
--
-- 'connectionName', 'updateConnection_connectionName' - The name of the connection.
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
    { encryptionMode = Prelude.Nothing,
      connectionName = Prelude.Nothing,
      connectionId = pConnectionId_
    }

-- | The connection MAC Security (MACsec) encryption mode.
--
-- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
updateConnection_encryptionMode :: Lens.Lens' UpdateConnection (Prelude.Maybe Prelude.Text)
updateConnection_encryptionMode = Lens.lens (\UpdateConnection' {encryptionMode} -> encryptionMode) (\s@UpdateConnection' {} a -> s {encryptionMode = a} :: UpdateConnection)

-- | The name of the connection.
updateConnection_connectionName :: Lens.Lens' UpdateConnection (Prelude.Maybe Prelude.Text)
updateConnection_connectionName = Lens.lens (\UpdateConnection' {connectionName} -> connectionName) (\s@UpdateConnection' {} a -> s {connectionName = a} :: UpdateConnection)

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
    _salt `Prelude.hashWithSalt` encryptionMode
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` connectionId

instance Prelude.NFData UpdateConnection where
  rnf UpdateConnection' {..} =
    Prelude.rnf encryptionMode
      `Prelude.seq` Prelude.rnf connectionName
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
          [ ("encryptionMode" Data..=)
              Prelude.<$> encryptionMode,
            ("connectionName" Data..=)
              Prelude.<$> connectionName,
            Prelude.Just ("connectionId" Data..= connectionId)
          ]
      )

instance Data.ToPath UpdateConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateConnection where
  toQuery = Prelude.const Prelude.mempty
