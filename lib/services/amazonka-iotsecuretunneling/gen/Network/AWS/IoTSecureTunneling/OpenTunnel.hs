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
-- Module      : Network.AWS.IoTSecureTunneling.OpenTunnel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new tunnel, and returns two client access tokens for clients
-- to use to connect to the AWS IoT Secure Tunneling proxy server.
module Network.AWS.IoTSecureTunneling.OpenTunnel
  ( -- * Creating a Request
    OpenTunnel (..),
    newOpenTunnel,

    -- * Request Lenses
    openTunnel_destinationConfig,
    openTunnel_description,
    openTunnel_timeoutConfig,
    openTunnel_tags,

    -- * Destructuring the Response
    OpenTunnelResponse (..),
    newOpenTunnelResponse,

    -- * Response Lenses
    openTunnelResponse_sourceAccessToken,
    openTunnelResponse_tunnelArn,
    openTunnelResponse_destinationAccessToken,
    openTunnelResponse_tunnelId,
    openTunnelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTSecureTunneling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newOpenTunnel' smart constructor.
data OpenTunnel = OpenTunnel'
  { -- | The destination configuration for the OpenTunnel request.
    destinationConfig :: Prelude.Maybe DestinationConfig,
    -- | A short text description of the tunnel.
    description :: Prelude.Maybe Prelude.Text,
    -- | Timeout configuration for a tunnel.
    timeoutConfig :: Prelude.Maybe TimeoutConfig,
    -- | A collection of tag metadata.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag)
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
-- 'destinationConfig', 'openTunnel_destinationConfig' - The destination configuration for the OpenTunnel request.
--
-- 'description', 'openTunnel_description' - A short text description of the tunnel.
--
-- 'timeoutConfig', 'openTunnel_timeoutConfig' - Timeout configuration for a tunnel.
--
-- 'tags', 'openTunnel_tags' - A collection of tag metadata.
newOpenTunnel ::
  OpenTunnel
newOpenTunnel =
  OpenTunnel'
    { destinationConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      timeoutConfig = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The destination configuration for the OpenTunnel request.
openTunnel_destinationConfig :: Lens.Lens' OpenTunnel (Prelude.Maybe DestinationConfig)
openTunnel_destinationConfig = Lens.lens (\OpenTunnel' {destinationConfig} -> destinationConfig) (\s@OpenTunnel' {} a -> s {destinationConfig = a} :: OpenTunnel)

-- | A short text description of the tunnel.
openTunnel_description :: Lens.Lens' OpenTunnel (Prelude.Maybe Prelude.Text)
openTunnel_description = Lens.lens (\OpenTunnel' {description} -> description) (\s@OpenTunnel' {} a -> s {description = a} :: OpenTunnel)

-- | Timeout configuration for a tunnel.
openTunnel_timeoutConfig :: Lens.Lens' OpenTunnel (Prelude.Maybe TimeoutConfig)
openTunnel_timeoutConfig = Lens.lens (\OpenTunnel' {timeoutConfig} -> timeoutConfig) (\s@OpenTunnel' {} a -> s {timeoutConfig = a} :: OpenTunnel)

-- | A collection of tag metadata.
openTunnel_tags :: Lens.Lens' OpenTunnel (Prelude.Maybe (Prelude.NonEmpty Tag))
openTunnel_tags = Lens.lens (\OpenTunnel' {tags} -> tags) (\s@OpenTunnel' {} a -> s {tags = a} :: OpenTunnel) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest OpenTunnel where
  type AWSResponse OpenTunnel = OpenTunnelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          OpenTunnelResponse'
            Prelude.<$> (x Core..?> "sourceAccessToken")
            Prelude.<*> (x Core..?> "tunnelArn")
            Prelude.<*> (x Core..?> "destinationAccessToken")
            Prelude.<*> (x Core..?> "tunnelId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable OpenTunnel

instance Prelude.NFData OpenTunnel

instance Core.ToHeaders OpenTunnel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IoTSecuredTunneling.OpenTunnel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON OpenTunnel where
  toJSON OpenTunnel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("destinationConfig" Core..=)
              Prelude.<$> destinationConfig,
            ("description" Core..=) Prelude.<$> description,
            ("timeoutConfig" Core..=) Prelude.<$> timeoutConfig,
            ("tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath OpenTunnel where
  toPath = Prelude.const "/"

instance Core.ToQuery OpenTunnel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newOpenTunnelResponse' smart constructor.
data OpenTunnelResponse = OpenTunnelResponse'
  { -- | The access token the source local proxy uses to connect to AWS IoT
    -- Secure Tunneling.
    sourceAccessToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name for the tunnel. The tunnel ARN format is
    -- @arn:aws:tunnel:\<region>:\<account-id>:tunnel\/\<tunnel-id>@
    tunnelArn :: Prelude.Maybe Prelude.Text,
    -- | The access token the destination local proxy uses to connect to AWS IoT
    -- Secure Tunneling.
    destinationAccessToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
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
-- 'sourceAccessToken', 'openTunnelResponse_sourceAccessToken' - The access token the source local proxy uses to connect to AWS IoT
-- Secure Tunneling.
--
-- 'tunnelArn', 'openTunnelResponse_tunnelArn' - The Amazon Resource Name for the tunnel. The tunnel ARN format is
-- @arn:aws:tunnel:\<region>:\<account-id>:tunnel\/\<tunnel-id>@
--
-- 'destinationAccessToken', 'openTunnelResponse_destinationAccessToken' - The access token the destination local proxy uses to connect to AWS IoT
-- Secure Tunneling.
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
    { sourceAccessToken =
        Prelude.Nothing,
      tunnelArn = Prelude.Nothing,
      destinationAccessToken = Prelude.Nothing,
      tunnelId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The access token the source local proxy uses to connect to AWS IoT
-- Secure Tunneling.
openTunnelResponse_sourceAccessToken :: Lens.Lens' OpenTunnelResponse (Prelude.Maybe Prelude.Text)
openTunnelResponse_sourceAccessToken = Lens.lens (\OpenTunnelResponse' {sourceAccessToken} -> sourceAccessToken) (\s@OpenTunnelResponse' {} a -> s {sourceAccessToken = a} :: OpenTunnelResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name for the tunnel. The tunnel ARN format is
-- @arn:aws:tunnel:\<region>:\<account-id>:tunnel\/\<tunnel-id>@
openTunnelResponse_tunnelArn :: Lens.Lens' OpenTunnelResponse (Prelude.Maybe Prelude.Text)
openTunnelResponse_tunnelArn = Lens.lens (\OpenTunnelResponse' {tunnelArn} -> tunnelArn) (\s@OpenTunnelResponse' {} a -> s {tunnelArn = a} :: OpenTunnelResponse)

-- | The access token the destination local proxy uses to connect to AWS IoT
-- Secure Tunneling.
openTunnelResponse_destinationAccessToken :: Lens.Lens' OpenTunnelResponse (Prelude.Maybe Prelude.Text)
openTunnelResponse_destinationAccessToken = Lens.lens (\OpenTunnelResponse' {destinationAccessToken} -> destinationAccessToken) (\s@OpenTunnelResponse' {} a -> s {destinationAccessToken = a} :: OpenTunnelResponse) Prelude.. Lens.mapping Core._Sensitive

-- | A unique alpha-numeric tunnel ID.
openTunnelResponse_tunnelId :: Lens.Lens' OpenTunnelResponse (Prelude.Maybe Prelude.Text)
openTunnelResponse_tunnelId = Lens.lens (\OpenTunnelResponse' {tunnelId} -> tunnelId) (\s@OpenTunnelResponse' {} a -> s {tunnelId = a} :: OpenTunnelResponse)

-- | The response's http status code.
openTunnelResponse_httpStatus :: Lens.Lens' OpenTunnelResponse Prelude.Int
openTunnelResponse_httpStatus = Lens.lens (\OpenTunnelResponse' {httpStatus} -> httpStatus) (\s@OpenTunnelResponse' {} a -> s {httpStatus = a} :: OpenTunnelResponse)

instance Prelude.NFData OpenTunnelResponse
