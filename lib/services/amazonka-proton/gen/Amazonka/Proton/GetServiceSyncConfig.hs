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
-- Module      : Amazonka.Proton.GetServiceSyncConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detailed information for the service sync configuration.
module Amazonka.Proton.GetServiceSyncConfig
  ( -- * Creating a Request
    GetServiceSyncConfig (..),
    newGetServiceSyncConfig,

    -- * Request Lenses
    getServiceSyncConfig_serviceName,

    -- * Destructuring the Response
    GetServiceSyncConfigResponse (..),
    newGetServiceSyncConfigResponse,

    -- * Response Lenses
    getServiceSyncConfigResponse_serviceSyncConfig,
    getServiceSyncConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServiceSyncConfig' smart constructor.
data GetServiceSyncConfig = GetServiceSyncConfig'
  { -- | The name of the service that you want to get the service sync
    -- configuration for.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceSyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'getServiceSyncConfig_serviceName' - The name of the service that you want to get the service sync
-- configuration for.
newGetServiceSyncConfig ::
  -- | 'serviceName'
  Prelude.Text ->
  GetServiceSyncConfig
newGetServiceSyncConfig pServiceName_ =
  GetServiceSyncConfig' {serviceName = pServiceName_}

-- | The name of the service that you want to get the service sync
-- configuration for.
getServiceSyncConfig_serviceName :: Lens.Lens' GetServiceSyncConfig Prelude.Text
getServiceSyncConfig_serviceName = Lens.lens (\GetServiceSyncConfig' {serviceName} -> serviceName) (\s@GetServiceSyncConfig' {} a -> s {serviceName = a} :: GetServiceSyncConfig)

instance Core.AWSRequest GetServiceSyncConfig where
  type
    AWSResponse GetServiceSyncConfig =
      GetServiceSyncConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceSyncConfigResponse'
            Prelude.<$> (x Data..?> "serviceSyncConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServiceSyncConfig where
  hashWithSalt _salt GetServiceSyncConfig' {..} =
    _salt `Prelude.hashWithSalt` serviceName

instance Prelude.NFData GetServiceSyncConfig where
  rnf GetServiceSyncConfig' {..} =
    Prelude.rnf serviceName

instance Data.ToHeaders GetServiceSyncConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetServiceSyncConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetServiceSyncConfig where
  toJSON GetServiceSyncConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("serviceName" Data..= serviceName)]
      )

instance Data.ToPath GetServiceSyncConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery GetServiceSyncConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceSyncConfigResponse' smart constructor.
data GetServiceSyncConfigResponse = GetServiceSyncConfigResponse'
  { -- | The detailed data of the requested service sync configuration.
    serviceSyncConfig :: Prelude.Maybe ServiceSyncConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceSyncConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSyncConfig', 'getServiceSyncConfigResponse_serviceSyncConfig' - The detailed data of the requested service sync configuration.
--
-- 'httpStatus', 'getServiceSyncConfigResponse_httpStatus' - The response's http status code.
newGetServiceSyncConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceSyncConfigResponse
newGetServiceSyncConfigResponse pHttpStatus_ =
  GetServiceSyncConfigResponse'
    { serviceSyncConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detailed data of the requested service sync configuration.
getServiceSyncConfigResponse_serviceSyncConfig :: Lens.Lens' GetServiceSyncConfigResponse (Prelude.Maybe ServiceSyncConfig)
getServiceSyncConfigResponse_serviceSyncConfig = Lens.lens (\GetServiceSyncConfigResponse' {serviceSyncConfig} -> serviceSyncConfig) (\s@GetServiceSyncConfigResponse' {} a -> s {serviceSyncConfig = a} :: GetServiceSyncConfigResponse)

-- | The response's http status code.
getServiceSyncConfigResponse_httpStatus :: Lens.Lens' GetServiceSyncConfigResponse Prelude.Int
getServiceSyncConfigResponse_httpStatus = Lens.lens (\GetServiceSyncConfigResponse' {httpStatus} -> httpStatus) (\s@GetServiceSyncConfigResponse' {} a -> s {httpStatus = a} :: GetServiceSyncConfigResponse)

instance Prelude.NFData GetServiceSyncConfigResponse where
  rnf GetServiceSyncConfigResponse' {..} =
    Prelude.rnf serviceSyncConfig
      `Prelude.seq` Prelude.rnf httpStatus
