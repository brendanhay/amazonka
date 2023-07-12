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
-- Module      : Amazonka.KinesisVideoSignaling.GetIceServerConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Interactive Connectivity Establishment (ICE) server
-- configuration information, including URIs, username, and password which
-- can be used to configure the WebRTC connection. The ICE component uses
-- this configuration information to setup the WebRTC connection, including
-- authenticating with the Traversal Using Relays around NAT (TURN) relay
-- server.
--
-- TURN is a protocol that is used to improve the connectivity of
-- peer-to-peer applications. By providing a cloud-based relay service,
-- TURN ensures that a connection can be established even when one or more
-- peers are incapable of a direct peer-to-peer connection. For more
-- information, see
-- <https://tools.ietf.org/html/draft-uberti-rtcweb-turn-rest-00 A REST API For Access To TURN Services>.
--
-- You can invoke this API to establish a fallback mechanism in case either
-- of the peers is unable to establish a direct peer-to-peer connection
-- over a signaling channel. You must specify either a signaling channel
-- ARN or the client ID in order to invoke this API.
module Amazonka.KinesisVideoSignaling.GetIceServerConfig
  ( -- * Creating a Request
    GetIceServerConfig (..),
    newGetIceServerConfig,

    -- * Request Lenses
    getIceServerConfig_clientId,
    getIceServerConfig_service,
    getIceServerConfig_username,
    getIceServerConfig_channelARN,

    -- * Destructuring the Response
    GetIceServerConfigResponse (..),
    newGetIceServerConfigResponse,

    -- * Response Lenses
    getIceServerConfigResponse_iceServerList,
    getIceServerConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideoSignaling.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIceServerConfig' smart constructor.
data GetIceServerConfig = GetIceServerConfig'
  { -- | Unique identifier for the viewer. Must be unique within the signaling
    -- channel.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the desired service. Currently, @TURN@ is the only valid
    -- value.
    service :: Prelude.Maybe Service,
    -- | An optional user ID to be associated with the credentials.
    username :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the signaling channel to be used for the peer-to-peer
    -- connection between configured peers.
    channelARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIceServerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'getIceServerConfig_clientId' - Unique identifier for the viewer. Must be unique within the signaling
-- channel.
--
-- 'service', 'getIceServerConfig_service' - Specifies the desired service. Currently, @TURN@ is the only valid
-- value.
--
-- 'username', 'getIceServerConfig_username' - An optional user ID to be associated with the credentials.
--
-- 'channelARN', 'getIceServerConfig_channelARN' - The ARN of the signaling channel to be used for the peer-to-peer
-- connection between configured peers.
newGetIceServerConfig ::
  -- | 'channelARN'
  Prelude.Text ->
  GetIceServerConfig
newGetIceServerConfig pChannelARN_ =
  GetIceServerConfig'
    { clientId = Prelude.Nothing,
      service = Prelude.Nothing,
      username = Prelude.Nothing,
      channelARN = pChannelARN_
    }

-- | Unique identifier for the viewer. Must be unique within the signaling
-- channel.
getIceServerConfig_clientId :: Lens.Lens' GetIceServerConfig (Prelude.Maybe Prelude.Text)
getIceServerConfig_clientId = Lens.lens (\GetIceServerConfig' {clientId} -> clientId) (\s@GetIceServerConfig' {} a -> s {clientId = a} :: GetIceServerConfig)

-- | Specifies the desired service. Currently, @TURN@ is the only valid
-- value.
getIceServerConfig_service :: Lens.Lens' GetIceServerConfig (Prelude.Maybe Service)
getIceServerConfig_service = Lens.lens (\GetIceServerConfig' {service} -> service) (\s@GetIceServerConfig' {} a -> s {service = a} :: GetIceServerConfig)

-- | An optional user ID to be associated with the credentials.
getIceServerConfig_username :: Lens.Lens' GetIceServerConfig (Prelude.Maybe Prelude.Text)
getIceServerConfig_username = Lens.lens (\GetIceServerConfig' {username} -> username) (\s@GetIceServerConfig' {} a -> s {username = a} :: GetIceServerConfig)

-- | The ARN of the signaling channel to be used for the peer-to-peer
-- connection between configured peers.
getIceServerConfig_channelARN :: Lens.Lens' GetIceServerConfig Prelude.Text
getIceServerConfig_channelARN = Lens.lens (\GetIceServerConfig' {channelARN} -> channelARN) (\s@GetIceServerConfig' {} a -> s {channelARN = a} :: GetIceServerConfig)

instance Core.AWSRequest GetIceServerConfig where
  type
    AWSResponse GetIceServerConfig =
      GetIceServerConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIceServerConfigResponse'
            Prelude.<$> (x Data..?> "IceServerList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIceServerConfig where
  hashWithSalt _salt GetIceServerConfig' {..} =
    _salt
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` channelARN

instance Prelude.NFData GetIceServerConfig where
  rnf GetIceServerConfig' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf channelARN

instance Data.ToHeaders GetIceServerConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetIceServerConfig where
  toJSON GetIceServerConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientId" Data..=) Prelude.<$> clientId,
            ("Service" Data..=) Prelude.<$> service,
            ("Username" Data..=) Prelude.<$> username,
            Prelude.Just ("ChannelARN" Data..= channelARN)
          ]
      )

instance Data.ToPath GetIceServerConfig where
  toPath = Prelude.const "/v1/get-ice-server-config"

instance Data.ToQuery GetIceServerConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIceServerConfigResponse' smart constructor.
data GetIceServerConfigResponse = GetIceServerConfigResponse'
  { -- | The list of ICE server information objects.
    iceServerList :: Prelude.Maybe [IceServer],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIceServerConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iceServerList', 'getIceServerConfigResponse_iceServerList' - The list of ICE server information objects.
--
-- 'httpStatus', 'getIceServerConfigResponse_httpStatus' - The response's http status code.
newGetIceServerConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIceServerConfigResponse
newGetIceServerConfigResponse pHttpStatus_ =
  GetIceServerConfigResponse'
    { iceServerList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of ICE server information objects.
getIceServerConfigResponse_iceServerList :: Lens.Lens' GetIceServerConfigResponse (Prelude.Maybe [IceServer])
getIceServerConfigResponse_iceServerList = Lens.lens (\GetIceServerConfigResponse' {iceServerList} -> iceServerList) (\s@GetIceServerConfigResponse' {} a -> s {iceServerList = a} :: GetIceServerConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getIceServerConfigResponse_httpStatus :: Lens.Lens' GetIceServerConfigResponse Prelude.Int
getIceServerConfigResponse_httpStatus = Lens.lens (\GetIceServerConfigResponse' {httpStatus} -> httpStatus) (\s@GetIceServerConfigResponse' {} a -> s {httpStatus = a} :: GetIceServerConfigResponse)

instance Prelude.NFData GetIceServerConfigResponse where
  rnf GetIceServerConfigResponse' {..} =
    Prelude.rnf iceServerList
      `Prelude.seq` Prelude.rnf httpStatus
