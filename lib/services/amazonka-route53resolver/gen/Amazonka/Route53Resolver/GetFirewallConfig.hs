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
-- Module      : Amazonka.Route53Resolver.GetFirewallConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration of the firewall behavior provided by DNS
-- Firewall for a single VPC from Amazon Virtual Private Cloud (Amazon
-- VPC).
module Amazonka.Route53Resolver.GetFirewallConfig
  ( -- * Creating a Request
    GetFirewallConfig (..),
    newGetFirewallConfig,

    -- * Request Lenses
    getFirewallConfig_resourceId,

    -- * Destructuring the Response
    GetFirewallConfigResponse (..),
    newGetFirewallConfigResponse,

    -- * Response Lenses
    getFirewallConfigResponse_firewallConfig,
    getFirewallConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newGetFirewallConfig' smart constructor.
data GetFirewallConfig = GetFirewallConfig'
  { -- | The ID of the VPC from Amazon VPC that the configuration is for.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFirewallConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'getFirewallConfig_resourceId' - The ID of the VPC from Amazon VPC that the configuration is for.
newGetFirewallConfig ::
  -- | 'resourceId'
  Prelude.Text ->
  GetFirewallConfig
newGetFirewallConfig pResourceId_ =
  GetFirewallConfig' {resourceId = pResourceId_}

-- | The ID of the VPC from Amazon VPC that the configuration is for.
getFirewallConfig_resourceId :: Lens.Lens' GetFirewallConfig Prelude.Text
getFirewallConfig_resourceId = Lens.lens (\GetFirewallConfig' {resourceId} -> resourceId) (\s@GetFirewallConfig' {} a -> s {resourceId = a} :: GetFirewallConfig)

instance Core.AWSRequest GetFirewallConfig where
  type
    AWSResponse GetFirewallConfig =
      GetFirewallConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFirewallConfigResponse'
            Prelude.<$> (x Data..?> "FirewallConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFirewallConfig where
  hashWithSalt _salt GetFirewallConfig' {..} =
    _salt `Prelude.hashWithSalt` resourceId

instance Prelude.NFData GetFirewallConfig where
  rnf GetFirewallConfig' {..} = Prelude.rnf resourceId

instance Data.ToHeaders GetFirewallConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.GetFirewallConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetFirewallConfig where
  toJSON GetFirewallConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceId" Data..= resourceId)]
      )

instance Data.ToPath GetFirewallConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery GetFirewallConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFirewallConfigResponse' smart constructor.
data GetFirewallConfigResponse = GetFirewallConfigResponse'
  { -- | Configuration of the firewall behavior provided by DNS Firewall for a
    -- single VPC from AmazonVPC.
    firewallConfig :: Prelude.Maybe FirewallConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFirewallConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallConfig', 'getFirewallConfigResponse_firewallConfig' - Configuration of the firewall behavior provided by DNS Firewall for a
-- single VPC from AmazonVPC.
--
-- 'httpStatus', 'getFirewallConfigResponse_httpStatus' - The response's http status code.
newGetFirewallConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFirewallConfigResponse
newGetFirewallConfigResponse pHttpStatus_ =
  GetFirewallConfigResponse'
    { firewallConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Configuration of the firewall behavior provided by DNS Firewall for a
-- single VPC from AmazonVPC.
getFirewallConfigResponse_firewallConfig :: Lens.Lens' GetFirewallConfigResponse (Prelude.Maybe FirewallConfig)
getFirewallConfigResponse_firewallConfig = Lens.lens (\GetFirewallConfigResponse' {firewallConfig} -> firewallConfig) (\s@GetFirewallConfigResponse' {} a -> s {firewallConfig = a} :: GetFirewallConfigResponse)

-- | The response's http status code.
getFirewallConfigResponse_httpStatus :: Lens.Lens' GetFirewallConfigResponse Prelude.Int
getFirewallConfigResponse_httpStatus = Lens.lens (\GetFirewallConfigResponse' {httpStatus} -> httpStatus) (\s@GetFirewallConfigResponse' {} a -> s {httpStatus = a} :: GetFirewallConfigResponse)

instance Prelude.NFData GetFirewallConfigResponse where
  rnf GetFirewallConfigResponse' {..} =
    Prelude.rnf firewallConfig
      `Prelude.seq` Prelude.rnf httpStatus
