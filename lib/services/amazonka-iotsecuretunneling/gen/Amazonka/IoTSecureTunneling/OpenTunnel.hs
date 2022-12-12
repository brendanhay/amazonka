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
-- Module      : Amazonka.IoTSecureTunneling.OpenTunnel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new tunnel, and returns two client access tokens for clients
-- to use to connect to the IoT Secure Tunneling proxy server.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions OpenTunnel>
-- action.
module Amazonka.IoTSecureTunneling.OpenTunnel
  ( -- * Creating a Request
    OpenTunnel (..),
    newOpenTunnel,

    -- * Request Lenses
    openTunnel_description,
    openTunnel_destinationConfig,
    openTunnel_tags,
    openTunnel_timeoutConfig,

    -- * Destructuring the Response
    OpenTunnelResponse (..),
    newOpenTunnelResponse,

    -- * Response Lenses
    openTunnelResponse_destinationAccessToken,
    openTunnelResponse_sourceAccessToken,
    openTunnelResponse_tunnelArn,
    openTunnelResponse_tunnelId,
    openTunnelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSecureTunneling.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newOpenTunnel' smart constructor.
data OpenTunnel = OpenTunnel'
  { -- | A short text description of the tunnel.
    description :: Prelude.Maybe Prelude.Text,
    -- | The destination configuration for the OpenTunnel request.
    destinationConfig :: Prelude.Maybe DestinationConfig,
    -- | A collection of tag metadata.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Timeout configuration for a tunnel.
    timeoutConfig :: Prelude.Maybe TimeoutConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenTunnel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'openTunnel_description' - A short text description of the tunnel.
--
-- 'destinationConfig', 'openTunnel_destinationConfig' - The destination configuration for the OpenTunnel request.
--
-- 'tags', 'openTunnel_tags' - A collection of tag metadata.
--
-- 'timeoutConfig', 'openTunnel_timeoutConfig' - Timeout configuration for a tunnel.
newOpenTunnel ::
  OpenTunnel
newOpenTunnel =
  OpenTunnel'
    { description = Prelude.Nothing,
      destinationConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeoutConfig = Prelude.Nothing
    }

-- | A short text description of the tunnel.
openTunnel_description :: Lens.Lens' OpenTunnel (Prelude.Maybe Prelude.Text)
openTunnel_description = Lens.lens (\OpenTunnel' {description} -> description) (\s@OpenTunnel' {} a -> s {description = a} :: OpenTunnel)

-- | The destination configuration for the OpenTunnel request.
openTunnel_destinationConfig :: Lens.Lens' OpenTunnel (Prelude.Maybe DestinationConfig)
openTunnel_destinationConfig = Lens.lens (\OpenTunnel' {destinationConfig} -> destinationConfig) (\s@OpenTunnel' {} a -> s {destinationConfig = a} :: OpenTunnel)

-- | A collection of tag metadata.
openTunnel_tags :: Lens.Lens' OpenTunnel (Prelude.Maybe (Prelude.NonEmpty Tag))
openTunnel_tags = Lens.lens (\OpenTunnel' {tags} -> tags) (\s@OpenTunnel' {} a -> s {tags = a} :: OpenTunnel) Prelude.. Lens.mapping Lens.coerced

-- | Timeout configuration for a tunnel.
openTunnel_timeoutConfig :: Lens.Lens' OpenTunnel (Prelude.Maybe TimeoutConfig)
openTunnel_timeoutConfig = Lens.lens (\OpenTunnel' {timeoutConfig} -> timeoutConfig) (\s@OpenTunnel' {} a -> s {timeoutConfig = a} :: OpenTunnel)

instance Core.AWSRequest OpenTunnel where
  type AWSResponse OpenTunnel = OpenTunnelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          OpenTunnelResponse'
            Prelude.<$> (x Data..?> "destinationAccessToken")
            Prelude.<*> (x Data..?> "sourceAccessToken")
            Prelude.<*> (x Data..?> "tunnelArn")
            Prelude.<*> (x Data..?> "tunnelId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable OpenTunnel where
  hashWithSalt _salt OpenTunnel' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destinationConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeoutConfig

instance Prelude.NFData OpenTunnel where
  rnf OpenTunnel' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf destinationConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeoutConfig

instance Data.ToHeaders OpenTunnel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTSecuredTunneling.OpenTunnel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON OpenTunnel where
  toJSON OpenTunnel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("destinationConfig" Data..=)
              Prelude.<$> destinationConfig,
            ("tags" Data..=) Prelude.<$> tags,
            ("timeoutConfig" Data..=) Prelude.<$> timeoutConfig
          ]
      )

instance Data.ToPath OpenTunnel where
  toPath = Prelude.const "/"

instance Data.ToQuery OpenTunnel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newOpenTunnelResponse' smart constructor.
data OpenTunnelResponse = OpenTunnelResponse'
  { -- | The access token the destination local proxy uses to connect to IoT
    -- Secure Tunneling.
    destinationAccessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The access token the source local proxy uses to connect to IoT Secure
    -- Tunneling.
    sourceAccessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name for the tunnel.
    tunnelArn :: Prelude.Maybe Prelude.Text,
    -- | A unique alpha-numeric tunnel ID.
    tunnelId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenTunnelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationAccessToken', 'openTunnelResponse_destinationAccessToken' - The access token the destination local proxy uses to connect to IoT
-- Secure Tunneling.
--
-- 'sourceAccessToken', 'openTunnelResponse_sourceAccessToken' - The access token the source local proxy uses to connect to IoT Secure
-- Tunneling.
--
-- 'tunnelArn', 'openTunnelResponse_tunnelArn' - The Amazon Resource Name for the tunnel.
--
-- 'tunnelId', 'openTunnelResponse_tunnelId' - A unique alpha-numeric tunnel ID.
--
-- 'httpStatus', 'openTunnelResponse_httpStatus' - The response's http status code.
newOpenTunnelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  OpenTunnelResponse
newOpenTunnelResponse pHttpStatus_ =
  OpenTunnelResponse'
    { destinationAccessToken =
        Prelude.Nothing,
      sourceAccessToken = Prelude.Nothing,
      tunnelArn = Prelude.Nothing,
      tunnelId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The access token the destination local proxy uses to connect to IoT
-- Secure Tunneling.
openTunnelResponse_destinationAccessToken :: Lens.Lens' OpenTunnelResponse (Prelude.Maybe Prelude.Text)
openTunnelResponse_destinationAccessToken = Lens.lens (\OpenTunnelResponse' {destinationAccessToken} -> destinationAccessToken) (\s@OpenTunnelResponse' {} a -> s {destinationAccessToken = a} :: OpenTunnelResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The access token the source local proxy uses to connect to IoT Secure
-- Tunneling.
openTunnelResponse_sourceAccessToken :: Lens.Lens' OpenTunnelResponse (Prelude.Maybe Prelude.Text)
openTunnelResponse_sourceAccessToken = Lens.lens (\OpenTunnelResponse' {sourceAccessToken} -> sourceAccessToken) (\s@OpenTunnelResponse' {} a -> s {sourceAccessToken = a} :: OpenTunnelResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name for the tunnel.
openTunnelResponse_tunnelArn :: Lens.Lens' OpenTunnelResponse (Prelude.Maybe Prelude.Text)
openTunnelResponse_tunnelArn = Lens.lens (\OpenTunnelResponse' {tunnelArn} -> tunnelArn) (\s@OpenTunnelResponse' {} a -> s {tunnelArn = a} :: OpenTunnelResponse)

-- | A unique alpha-numeric tunnel ID.
openTunnelResponse_tunnelId :: Lens.Lens' OpenTunnelResponse (Prelude.Maybe Prelude.Text)
openTunnelResponse_tunnelId = Lens.lens (\OpenTunnelResponse' {tunnelId} -> tunnelId) (\s@OpenTunnelResponse' {} a -> s {tunnelId = a} :: OpenTunnelResponse)

-- | The response's http status code.
openTunnelResponse_httpStatus :: Lens.Lens' OpenTunnelResponse Prelude.Int
openTunnelResponse_httpStatus = Lens.lens (\OpenTunnelResponse' {httpStatus} -> httpStatus) (\s@OpenTunnelResponse' {} a -> s {httpStatus = a} :: OpenTunnelResponse)

instance Prelude.NFData OpenTunnelResponse where
  rnf OpenTunnelResponse' {..} =
    Prelude.rnf destinationAccessToken
      `Prelude.seq` Prelude.rnf sourceAccessToken
      `Prelude.seq` Prelude.rnf tunnelArn
      `Prelude.seq` Prelude.rnf tunnelId
      `Prelude.seq` Prelude.rnf httpStatus
