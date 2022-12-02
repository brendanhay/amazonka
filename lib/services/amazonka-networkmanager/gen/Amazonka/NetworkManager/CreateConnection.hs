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
-- Module      : Amazonka.NetworkManager.CreateConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection between two devices. The devices can be a physical
-- or virtual appliance that connects to a third-party appliance in a VPC,
-- or a physical appliance that connects to another physical appliance in
-- an on-premises network.
module Amazonka.NetworkManager.CreateConnection
  ( -- * Creating a Request
    CreateConnection (..),
    newCreateConnection,

    -- * Request Lenses
    createConnection_tags,
    createConnection_linkId,
    createConnection_description,
    createConnection_connectedLinkId,
    createConnection_globalNetworkId,
    createConnection_deviceId,
    createConnection_connectedDeviceId,

    -- * Destructuring the Response
    CreateConnectionResponse (..),
    newCreateConnectionResponse,

    -- * Response Lenses
    createConnectionResponse_connection,
    createConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { -- | The tags to apply to the resource during creation.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the link for the first device.
    linkId :: Prelude.Maybe Prelude.Text,
    -- | A description of the connection.
    --
    -- Length Constraints: Maximum length of 256 characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the link for the second device.
    connectedLinkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The ID of the first device in the connection.
    deviceId :: Prelude.Text,
    -- | The ID of the second device in the connection.
    connectedDeviceId :: Prelude.Text
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
-- 'tags', 'createConnection_tags' - The tags to apply to the resource during creation.
--
-- 'linkId', 'createConnection_linkId' - The ID of the link for the first device.
--
-- 'description', 'createConnection_description' - A description of the connection.
--
-- Length Constraints: Maximum length of 256 characters.
--
-- 'connectedLinkId', 'createConnection_connectedLinkId' - The ID of the link for the second device.
--
-- 'globalNetworkId', 'createConnection_globalNetworkId' - The ID of the global network.
--
-- 'deviceId', 'createConnection_deviceId' - The ID of the first device in the connection.
--
-- 'connectedDeviceId', 'createConnection_connectedDeviceId' - The ID of the second device in the connection.
newCreateConnection ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'connectedDeviceId'
  Prelude.Text ->
  CreateConnection
newCreateConnection
  pGlobalNetworkId_
  pDeviceId_
  pConnectedDeviceId_ =
    CreateConnection'
      { tags = Prelude.Nothing,
        linkId = Prelude.Nothing,
        description = Prelude.Nothing,
        connectedLinkId = Prelude.Nothing,
        globalNetworkId = pGlobalNetworkId_,
        deviceId = pDeviceId_,
        connectedDeviceId = pConnectedDeviceId_
      }

-- | The tags to apply to the resource during creation.
createConnection_tags :: Lens.Lens' CreateConnection (Prelude.Maybe [Tag])
createConnection_tags = Lens.lens (\CreateConnection' {tags} -> tags) (\s@CreateConnection' {} a -> s {tags = a} :: CreateConnection) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the link for the first device.
createConnection_linkId :: Lens.Lens' CreateConnection (Prelude.Maybe Prelude.Text)
createConnection_linkId = Lens.lens (\CreateConnection' {linkId} -> linkId) (\s@CreateConnection' {} a -> s {linkId = a} :: CreateConnection)

-- | A description of the connection.
--
-- Length Constraints: Maximum length of 256 characters.
createConnection_description :: Lens.Lens' CreateConnection (Prelude.Maybe Prelude.Text)
createConnection_description = Lens.lens (\CreateConnection' {description} -> description) (\s@CreateConnection' {} a -> s {description = a} :: CreateConnection)

-- | The ID of the link for the second device.
createConnection_connectedLinkId :: Lens.Lens' CreateConnection (Prelude.Maybe Prelude.Text)
createConnection_connectedLinkId = Lens.lens (\CreateConnection' {connectedLinkId} -> connectedLinkId) (\s@CreateConnection' {} a -> s {connectedLinkId = a} :: CreateConnection)

-- | The ID of the global network.
createConnection_globalNetworkId :: Lens.Lens' CreateConnection Prelude.Text
createConnection_globalNetworkId = Lens.lens (\CreateConnection' {globalNetworkId} -> globalNetworkId) (\s@CreateConnection' {} a -> s {globalNetworkId = a} :: CreateConnection)

-- | The ID of the first device in the connection.
createConnection_deviceId :: Lens.Lens' CreateConnection Prelude.Text
createConnection_deviceId = Lens.lens (\CreateConnection' {deviceId} -> deviceId) (\s@CreateConnection' {} a -> s {deviceId = a} :: CreateConnection)

-- | The ID of the second device in the connection.
createConnection_connectedDeviceId :: Lens.Lens' CreateConnection Prelude.Text
createConnection_connectedDeviceId = Lens.lens (\CreateConnection' {connectedDeviceId} -> connectedDeviceId) (\s@CreateConnection' {} a -> s {connectedDeviceId = a} :: CreateConnection)

instance Core.AWSRequest CreateConnection where
  type
    AWSResponse CreateConnection =
      CreateConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectionResponse'
            Prelude.<$> (x Data..?> "Connection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConnection where
  hashWithSalt _salt CreateConnection' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` linkId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` connectedLinkId
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` connectedDeviceId

instance Prelude.NFData CreateConnection where
  rnf CreateConnection' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf linkId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf connectedLinkId
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf connectedDeviceId

instance Data.ToHeaders CreateConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("LinkId" Data..=) Prelude.<$> linkId,
            ("Description" Data..=) Prelude.<$> description,
            ("ConnectedLinkId" Data..=)
              Prelude.<$> connectedLinkId,
            Prelude.Just ("DeviceId" Data..= deviceId),
            Prelude.Just
              ("ConnectedDeviceId" Data..= connectedDeviceId)
          ]
      )

instance Data.ToPath CreateConnection where
  toPath CreateConnection' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/connections"
      ]

instance Data.ToQuery CreateConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectionResponse' smart constructor.
data CreateConnectionResponse = CreateConnectionResponse'
  { -- | Information about the connection.
    connection :: Prelude.Maybe Connection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connection', 'createConnectionResponse_connection' - Information about the connection.
--
-- 'httpStatus', 'createConnectionResponse_httpStatus' - The response's http status code.
newCreateConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConnectionResponse
newCreateConnectionResponse pHttpStatus_ =
  CreateConnectionResponse'
    { connection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the connection.
createConnectionResponse_connection :: Lens.Lens' CreateConnectionResponse (Prelude.Maybe Connection)
createConnectionResponse_connection = Lens.lens (\CreateConnectionResponse' {connection} -> connection) (\s@CreateConnectionResponse' {} a -> s {connection = a} :: CreateConnectionResponse)

-- | The response's http status code.
createConnectionResponse_httpStatus :: Lens.Lens' CreateConnectionResponse Prelude.Int
createConnectionResponse_httpStatus = Lens.lens (\CreateConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectionResponse' {} a -> s {httpStatus = a} :: CreateConnectionResponse)

instance Prelude.NFData CreateConnectionResponse where
  rnf CreateConnectionResponse' {..} =
    Prelude.rnf connection
      `Prelude.seq` Prelude.rnf httpStatus
