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
-- Module      : Amazonka.Route53Resolver.UpdateFirewallConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of the firewall behavior provided by DNS
-- Firewall for a single VPC from Amazon Virtual Private Cloud (Amazon
-- VPC).
module Amazonka.Route53Resolver.UpdateFirewallConfig
  ( -- * Creating a Request
    UpdateFirewallConfig (..),
    newUpdateFirewallConfig,

    -- * Request Lenses
    updateFirewallConfig_resourceId,
    updateFirewallConfig_firewallFailOpen,

    -- * Destructuring the Response
    UpdateFirewallConfigResponse (..),
    newUpdateFirewallConfigResponse,

    -- * Response Lenses
    updateFirewallConfigResponse_firewallConfig,
    updateFirewallConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newUpdateFirewallConfig' smart constructor.
data UpdateFirewallConfig = UpdateFirewallConfig'
  { -- | The ID of the VPC that the configuration is for.
    resourceId :: Prelude.Text,
    -- | Determines how Route 53 Resolver handles queries during failures, for
    -- example when all traffic that is sent to DNS Firewall fails to receive a
    -- reply.
    --
    -- -   By default, fail open is disabled, which means the failure mode is
    --     closed. This approach favors security over availability. DNS
    --     Firewall blocks queries that it is unable to evaluate properly.
    --
    -- -   If you enable this option, the failure mode is open. This approach
    --     favors availability over security. DNS Firewall allows queries to
    --     proceed if it is unable to properly evaluate them.
    --
    -- This behavior is only enforced for VPCs that have at least one DNS
    -- Firewall rule group association.
    firewallFailOpen :: FirewallFailOpenStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFirewallConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'updateFirewallConfig_resourceId' - The ID of the VPC that the configuration is for.
--
-- 'firewallFailOpen', 'updateFirewallConfig_firewallFailOpen' - Determines how Route 53 Resolver handles queries during failures, for
-- example when all traffic that is sent to DNS Firewall fails to receive a
-- reply.
--
-- -   By default, fail open is disabled, which means the failure mode is
--     closed. This approach favors security over availability. DNS
--     Firewall blocks queries that it is unable to evaluate properly.
--
-- -   If you enable this option, the failure mode is open. This approach
--     favors availability over security. DNS Firewall allows queries to
--     proceed if it is unable to properly evaluate them.
--
-- This behavior is only enforced for VPCs that have at least one DNS
-- Firewall rule group association.
newUpdateFirewallConfig ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'firewallFailOpen'
  FirewallFailOpenStatus ->
  UpdateFirewallConfig
newUpdateFirewallConfig
  pResourceId_
  pFirewallFailOpen_ =
    UpdateFirewallConfig'
      { resourceId = pResourceId_,
        firewallFailOpen = pFirewallFailOpen_
      }

-- | The ID of the VPC that the configuration is for.
updateFirewallConfig_resourceId :: Lens.Lens' UpdateFirewallConfig Prelude.Text
updateFirewallConfig_resourceId = Lens.lens (\UpdateFirewallConfig' {resourceId} -> resourceId) (\s@UpdateFirewallConfig' {} a -> s {resourceId = a} :: UpdateFirewallConfig)

-- | Determines how Route 53 Resolver handles queries during failures, for
-- example when all traffic that is sent to DNS Firewall fails to receive a
-- reply.
--
-- -   By default, fail open is disabled, which means the failure mode is
--     closed. This approach favors security over availability. DNS
--     Firewall blocks queries that it is unable to evaluate properly.
--
-- -   If you enable this option, the failure mode is open. This approach
--     favors availability over security. DNS Firewall allows queries to
--     proceed if it is unable to properly evaluate them.
--
-- This behavior is only enforced for VPCs that have at least one DNS
-- Firewall rule group association.
updateFirewallConfig_firewallFailOpen :: Lens.Lens' UpdateFirewallConfig FirewallFailOpenStatus
updateFirewallConfig_firewallFailOpen = Lens.lens (\UpdateFirewallConfig' {firewallFailOpen} -> firewallFailOpen) (\s@UpdateFirewallConfig' {} a -> s {firewallFailOpen = a} :: UpdateFirewallConfig)

instance Core.AWSRequest UpdateFirewallConfig where
  type
    AWSResponse UpdateFirewallConfig =
      UpdateFirewallConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFirewallConfigResponse'
            Prelude.<$> (x Data..?> "FirewallConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFirewallConfig where
  hashWithSalt _salt UpdateFirewallConfig' {..} =
    _salt
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` firewallFailOpen

instance Prelude.NFData UpdateFirewallConfig where
  rnf UpdateFirewallConfig' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf firewallFailOpen

instance Data.ToHeaders UpdateFirewallConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.UpdateFirewallConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFirewallConfig where
  toJSON UpdateFirewallConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just
              ("FirewallFailOpen" Data..= firewallFailOpen)
          ]
      )

instance Data.ToPath UpdateFirewallConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFirewallConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFirewallConfigResponse' smart constructor.
data UpdateFirewallConfigResponse = UpdateFirewallConfigResponse'
  { -- | Configuration of the firewall behavior provided by DNS Firewall for a
    -- single VPC.
    firewallConfig :: Prelude.Maybe FirewallConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFirewallConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallConfig', 'updateFirewallConfigResponse_firewallConfig' - Configuration of the firewall behavior provided by DNS Firewall for a
-- single VPC.
--
-- 'httpStatus', 'updateFirewallConfigResponse_httpStatus' - The response's http status code.
newUpdateFirewallConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFirewallConfigResponse
newUpdateFirewallConfigResponse pHttpStatus_ =
  UpdateFirewallConfigResponse'
    { firewallConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Configuration of the firewall behavior provided by DNS Firewall for a
-- single VPC.
updateFirewallConfigResponse_firewallConfig :: Lens.Lens' UpdateFirewallConfigResponse (Prelude.Maybe FirewallConfig)
updateFirewallConfigResponse_firewallConfig = Lens.lens (\UpdateFirewallConfigResponse' {firewallConfig} -> firewallConfig) (\s@UpdateFirewallConfigResponse' {} a -> s {firewallConfig = a} :: UpdateFirewallConfigResponse)

-- | The response's http status code.
updateFirewallConfigResponse_httpStatus :: Lens.Lens' UpdateFirewallConfigResponse Prelude.Int
updateFirewallConfigResponse_httpStatus = Lens.lens (\UpdateFirewallConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateFirewallConfigResponse' {} a -> s {httpStatus = a} :: UpdateFirewallConfigResponse)

instance Prelude.NFData UpdateFirewallConfigResponse where
  rnf UpdateFirewallConfigResponse' {..} =
    Prelude.rnf firewallConfig
      `Prelude.seq` Prelude.rnf httpStatus
