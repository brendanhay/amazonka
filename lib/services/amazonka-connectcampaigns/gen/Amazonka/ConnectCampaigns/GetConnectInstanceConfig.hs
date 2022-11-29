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
-- Module      : Amazonka.ConnectCampaigns.GetConnectInstanceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the specific Connect instance config.
module Amazonka.ConnectCampaigns.GetConnectInstanceConfig
  ( -- * Creating a Request
    GetConnectInstanceConfig (..),
    newGetConnectInstanceConfig,

    -- * Request Lenses
    getConnectInstanceConfig_connectInstanceId,

    -- * Destructuring the Response
    GetConnectInstanceConfigResponse (..),
    newGetConnectInstanceConfigResponse,

    -- * Response Lenses
    getConnectInstanceConfigResponse_connectInstanceConfig,
    getConnectInstanceConfigResponse_httpStatus,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | GetConnectInstanceConfigRequest
--
-- /See:/ 'newGetConnectInstanceConfig' smart constructor.
data GetConnectInstanceConfig = GetConnectInstanceConfig'
  { connectInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectInstanceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectInstanceId', 'getConnectInstanceConfig_connectInstanceId' - Undocumented member.
newGetConnectInstanceConfig ::
  -- | 'connectInstanceId'
  Prelude.Text ->
  GetConnectInstanceConfig
newGetConnectInstanceConfig pConnectInstanceId_ =
  GetConnectInstanceConfig'
    { connectInstanceId =
        pConnectInstanceId_
    }

-- | Undocumented member.
getConnectInstanceConfig_connectInstanceId :: Lens.Lens' GetConnectInstanceConfig Prelude.Text
getConnectInstanceConfig_connectInstanceId = Lens.lens (\GetConnectInstanceConfig' {connectInstanceId} -> connectInstanceId) (\s@GetConnectInstanceConfig' {} a -> s {connectInstanceId = a} :: GetConnectInstanceConfig)

instance Core.AWSRequest GetConnectInstanceConfig where
  type
    AWSResponse GetConnectInstanceConfig =
      GetConnectInstanceConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectInstanceConfigResponse'
            Prelude.<$> (x Core..?> "connectInstanceConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnectInstanceConfig where
  hashWithSalt _salt GetConnectInstanceConfig' {..} =
    _salt `Prelude.hashWithSalt` connectInstanceId

instance Prelude.NFData GetConnectInstanceConfig where
  rnf GetConnectInstanceConfig' {..} =
    Prelude.rnf connectInstanceId

instance Core.ToHeaders GetConnectInstanceConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetConnectInstanceConfig where
  toPath GetConnectInstanceConfig' {..} =
    Prelude.mconcat
      [ "/connect-instance/",
        Core.toBS connectInstanceId,
        "/config"
      ]

instance Core.ToQuery GetConnectInstanceConfig where
  toQuery = Prelude.const Prelude.mempty

-- | GetConnectInstanceConfigResponse
--
-- /See:/ 'newGetConnectInstanceConfigResponse' smart constructor.
data GetConnectInstanceConfigResponse = GetConnectInstanceConfigResponse'
  { connectInstanceConfig :: Prelude.Maybe InstanceConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectInstanceConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectInstanceConfig', 'getConnectInstanceConfigResponse_connectInstanceConfig' - Undocumented member.
--
-- 'httpStatus', 'getConnectInstanceConfigResponse_httpStatus' - The response's http status code.
newGetConnectInstanceConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConnectInstanceConfigResponse
newGetConnectInstanceConfigResponse pHttpStatus_ =
  GetConnectInstanceConfigResponse'
    { connectInstanceConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getConnectInstanceConfigResponse_connectInstanceConfig :: Lens.Lens' GetConnectInstanceConfigResponse (Prelude.Maybe InstanceConfig)
getConnectInstanceConfigResponse_connectInstanceConfig = Lens.lens (\GetConnectInstanceConfigResponse' {connectInstanceConfig} -> connectInstanceConfig) (\s@GetConnectInstanceConfigResponse' {} a -> s {connectInstanceConfig = a} :: GetConnectInstanceConfigResponse)

-- | The response's http status code.
getConnectInstanceConfigResponse_httpStatus :: Lens.Lens' GetConnectInstanceConfigResponse Prelude.Int
getConnectInstanceConfigResponse_httpStatus = Lens.lens (\GetConnectInstanceConfigResponse' {httpStatus} -> httpStatus) (\s@GetConnectInstanceConfigResponse' {} a -> s {httpStatus = a} :: GetConnectInstanceConfigResponse)

instance
  Prelude.NFData
    GetConnectInstanceConfigResponse
  where
  rnf GetConnectInstanceConfigResponse' {..} =
    Prelude.rnf connectInstanceConfig
      `Prelude.seq` Prelude.rnf httpStatus
