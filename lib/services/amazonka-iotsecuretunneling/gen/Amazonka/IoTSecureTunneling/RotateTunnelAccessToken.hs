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
-- Module      : Amazonka.IoTSecureTunneling.RotateTunnelAccessToken
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes the current client access token (CAT) and returns new CAT for
-- clients to use when reconnecting to secure tunneling to access the same
-- tunnel.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions RotateTunnelAccessToken>
-- action.
--
-- Rotating the CAT doesn\'t extend the tunnel duration. For example, say
-- the tunnel duration is 12 hours and the tunnel has already been open for
-- 4 hours. When you rotate the access tokens, the new tokens that are
-- generated can only be used for the remaining 8 hours.
module Amazonka.IoTSecureTunneling.RotateTunnelAccessToken
  ( -- * Creating a Request
    RotateTunnelAccessToken (..),
    newRotateTunnelAccessToken,

    -- * Request Lenses
    rotateTunnelAccessToken_destinationConfig,
    rotateTunnelAccessToken_tunnelId,
    rotateTunnelAccessToken_clientMode,

    -- * Destructuring the Response
    RotateTunnelAccessTokenResponse (..),
    newRotateTunnelAccessTokenResponse,

    -- * Response Lenses
    rotateTunnelAccessTokenResponse_destinationAccessToken,
    rotateTunnelAccessTokenResponse_sourceAccessToken,
    rotateTunnelAccessTokenResponse_tunnelArn,
    rotateTunnelAccessTokenResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSecureTunneling.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRotateTunnelAccessToken' smart constructor.
data RotateTunnelAccessToken = RotateTunnelAccessToken'
  { destinationConfig :: Prelude.Maybe DestinationConfig,
    -- | The tunnel for which you want to rotate the access tokens.
    tunnelId :: Prelude.Text,
    -- | The mode of the client that will use the client token, which can be
    -- either the source or destination, or both source and destination.
    clientMode :: ClientMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RotateTunnelAccessToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationConfig', 'rotateTunnelAccessToken_destinationConfig' - Undocumented member.
--
-- 'tunnelId', 'rotateTunnelAccessToken_tunnelId' - The tunnel for which you want to rotate the access tokens.
--
-- 'clientMode', 'rotateTunnelAccessToken_clientMode' - The mode of the client that will use the client token, which can be
-- either the source or destination, or both source and destination.
newRotateTunnelAccessToken ::
  -- | 'tunnelId'
  Prelude.Text ->
  -- | 'clientMode'
  ClientMode ->
  RotateTunnelAccessToken
newRotateTunnelAccessToken pTunnelId_ pClientMode_ =
  RotateTunnelAccessToken'
    { destinationConfig =
        Prelude.Nothing,
      tunnelId = pTunnelId_,
      clientMode = pClientMode_
    }

-- | Undocumented member.
rotateTunnelAccessToken_destinationConfig :: Lens.Lens' RotateTunnelAccessToken (Prelude.Maybe DestinationConfig)
rotateTunnelAccessToken_destinationConfig = Lens.lens (\RotateTunnelAccessToken' {destinationConfig} -> destinationConfig) (\s@RotateTunnelAccessToken' {} a -> s {destinationConfig = a} :: RotateTunnelAccessToken)

-- | The tunnel for which you want to rotate the access tokens.
rotateTunnelAccessToken_tunnelId :: Lens.Lens' RotateTunnelAccessToken Prelude.Text
rotateTunnelAccessToken_tunnelId = Lens.lens (\RotateTunnelAccessToken' {tunnelId} -> tunnelId) (\s@RotateTunnelAccessToken' {} a -> s {tunnelId = a} :: RotateTunnelAccessToken)

-- | The mode of the client that will use the client token, which can be
-- either the source or destination, or both source and destination.
rotateTunnelAccessToken_clientMode :: Lens.Lens' RotateTunnelAccessToken ClientMode
rotateTunnelAccessToken_clientMode = Lens.lens (\RotateTunnelAccessToken' {clientMode} -> clientMode) (\s@RotateTunnelAccessToken' {} a -> s {clientMode = a} :: RotateTunnelAccessToken)

instance Core.AWSRequest RotateTunnelAccessToken where
  type
    AWSResponse RotateTunnelAccessToken =
      RotateTunnelAccessTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RotateTunnelAccessTokenResponse'
            Prelude.<$> (x Data..?> "destinationAccessToken")
            Prelude.<*> (x Data..?> "sourceAccessToken")
            Prelude.<*> (x Data..?> "tunnelArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RotateTunnelAccessToken where
  hashWithSalt _salt RotateTunnelAccessToken' {..} =
    _salt `Prelude.hashWithSalt` destinationConfig
      `Prelude.hashWithSalt` tunnelId
      `Prelude.hashWithSalt` clientMode

instance Prelude.NFData RotateTunnelAccessToken where
  rnf RotateTunnelAccessToken' {..} =
    Prelude.rnf destinationConfig
      `Prelude.seq` Prelude.rnf tunnelId
      `Prelude.seq` Prelude.rnf clientMode

instance Data.ToHeaders RotateTunnelAccessToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTSecuredTunneling.RotateTunnelAccessToken" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RotateTunnelAccessToken where
  toJSON RotateTunnelAccessToken' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("destinationConfig" Data..=)
              Prelude.<$> destinationConfig,
            Prelude.Just ("tunnelId" Data..= tunnelId),
            Prelude.Just ("clientMode" Data..= clientMode)
          ]
      )

instance Data.ToPath RotateTunnelAccessToken where
  toPath = Prelude.const "/"

instance Data.ToQuery RotateTunnelAccessToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRotateTunnelAccessTokenResponse' smart constructor.
data RotateTunnelAccessTokenResponse = RotateTunnelAccessTokenResponse'
  { -- | The client access token that the destination local proxy uses to connect
    -- to IoT Secure Tunneling.
    destinationAccessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The client access token that the source local proxy uses to connect to
    -- IoT Secure Tunneling.
    sourceAccessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name for the tunnel.
    tunnelArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RotateTunnelAccessTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationAccessToken', 'rotateTunnelAccessTokenResponse_destinationAccessToken' - The client access token that the destination local proxy uses to connect
-- to IoT Secure Tunneling.
--
-- 'sourceAccessToken', 'rotateTunnelAccessTokenResponse_sourceAccessToken' - The client access token that the source local proxy uses to connect to
-- IoT Secure Tunneling.
--
-- 'tunnelArn', 'rotateTunnelAccessTokenResponse_tunnelArn' - The Amazon Resource Name for the tunnel.
--
-- 'httpStatus', 'rotateTunnelAccessTokenResponse_httpStatus' - The response's http status code.
newRotateTunnelAccessTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RotateTunnelAccessTokenResponse
newRotateTunnelAccessTokenResponse pHttpStatus_ =
  RotateTunnelAccessTokenResponse'
    { destinationAccessToken =
        Prelude.Nothing,
      sourceAccessToken = Prelude.Nothing,
      tunnelArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The client access token that the destination local proxy uses to connect
-- to IoT Secure Tunneling.
rotateTunnelAccessTokenResponse_destinationAccessToken :: Lens.Lens' RotateTunnelAccessTokenResponse (Prelude.Maybe Prelude.Text)
rotateTunnelAccessTokenResponse_destinationAccessToken = Lens.lens (\RotateTunnelAccessTokenResponse' {destinationAccessToken} -> destinationAccessToken) (\s@RotateTunnelAccessTokenResponse' {} a -> s {destinationAccessToken = a} :: RotateTunnelAccessTokenResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The client access token that the source local proxy uses to connect to
-- IoT Secure Tunneling.
rotateTunnelAccessTokenResponse_sourceAccessToken :: Lens.Lens' RotateTunnelAccessTokenResponse (Prelude.Maybe Prelude.Text)
rotateTunnelAccessTokenResponse_sourceAccessToken = Lens.lens (\RotateTunnelAccessTokenResponse' {sourceAccessToken} -> sourceAccessToken) (\s@RotateTunnelAccessTokenResponse' {} a -> s {sourceAccessToken = a} :: RotateTunnelAccessTokenResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name for the tunnel.
rotateTunnelAccessTokenResponse_tunnelArn :: Lens.Lens' RotateTunnelAccessTokenResponse (Prelude.Maybe Prelude.Text)
rotateTunnelAccessTokenResponse_tunnelArn = Lens.lens (\RotateTunnelAccessTokenResponse' {tunnelArn} -> tunnelArn) (\s@RotateTunnelAccessTokenResponse' {} a -> s {tunnelArn = a} :: RotateTunnelAccessTokenResponse)

-- | The response's http status code.
rotateTunnelAccessTokenResponse_httpStatus :: Lens.Lens' RotateTunnelAccessTokenResponse Prelude.Int
rotateTunnelAccessTokenResponse_httpStatus = Lens.lens (\RotateTunnelAccessTokenResponse' {httpStatus} -> httpStatus) (\s@RotateTunnelAccessTokenResponse' {} a -> s {httpStatus = a} :: RotateTunnelAccessTokenResponse)

instance
  Prelude.NFData
    RotateTunnelAccessTokenResponse
  where
  rnf RotateTunnelAccessTokenResponse' {..} =
    Prelude.rnf destinationAccessToken
      `Prelude.seq` Prelude.rnf sourceAccessToken
      `Prelude.seq` Prelude.rnf tunnelArn
      `Prelude.seq` Prelude.rnf httpStatus
