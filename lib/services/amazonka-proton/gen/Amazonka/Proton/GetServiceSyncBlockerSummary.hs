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
-- Module      : Amazonka.Proton.GetServiceSyncBlockerSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detailed data for the service sync blocker summary.
module Amazonka.Proton.GetServiceSyncBlockerSummary
  ( -- * Creating a Request
    GetServiceSyncBlockerSummary (..),
    newGetServiceSyncBlockerSummary,

    -- * Request Lenses
    getServiceSyncBlockerSummary_serviceInstanceName,
    getServiceSyncBlockerSummary_serviceName,

    -- * Destructuring the Response
    GetServiceSyncBlockerSummaryResponse (..),
    newGetServiceSyncBlockerSummaryResponse,

    -- * Response Lenses
    getServiceSyncBlockerSummaryResponse_serviceSyncBlockerSummary,
    getServiceSyncBlockerSummaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServiceSyncBlockerSummary' smart constructor.
data GetServiceSyncBlockerSummary = GetServiceSyncBlockerSummary'
  { -- | The name of the service instance that you want to get the service sync
    -- blocker summary for. If given bothe the instance name and the service
    -- name, only the instance is blocked.
    serviceInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the service that you want to get the service sync blocker
    -- summary for. If given only the service name, all instances are blocked.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceSyncBlockerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceInstanceName', 'getServiceSyncBlockerSummary_serviceInstanceName' - The name of the service instance that you want to get the service sync
-- blocker summary for. If given bothe the instance name and the service
-- name, only the instance is blocked.
--
-- 'serviceName', 'getServiceSyncBlockerSummary_serviceName' - The name of the service that you want to get the service sync blocker
-- summary for. If given only the service name, all instances are blocked.
newGetServiceSyncBlockerSummary ::
  -- | 'serviceName'
  Prelude.Text ->
  GetServiceSyncBlockerSummary
newGetServiceSyncBlockerSummary pServiceName_ =
  GetServiceSyncBlockerSummary'
    { serviceInstanceName =
        Prelude.Nothing,
      serviceName = pServiceName_
    }

-- | The name of the service instance that you want to get the service sync
-- blocker summary for. If given bothe the instance name and the service
-- name, only the instance is blocked.
getServiceSyncBlockerSummary_serviceInstanceName :: Lens.Lens' GetServiceSyncBlockerSummary (Prelude.Maybe Prelude.Text)
getServiceSyncBlockerSummary_serviceInstanceName = Lens.lens (\GetServiceSyncBlockerSummary' {serviceInstanceName} -> serviceInstanceName) (\s@GetServiceSyncBlockerSummary' {} a -> s {serviceInstanceName = a} :: GetServiceSyncBlockerSummary)

-- | The name of the service that you want to get the service sync blocker
-- summary for. If given only the service name, all instances are blocked.
getServiceSyncBlockerSummary_serviceName :: Lens.Lens' GetServiceSyncBlockerSummary Prelude.Text
getServiceSyncBlockerSummary_serviceName = Lens.lens (\GetServiceSyncBlockerSummary' {serviceName} -> serviceName) (\s@GetServiceSyncBlockerSummary' {} a -> s {serviceName = a} :: GetServiceSyncBlockerSummary)

instance Core.AWSRequest GetServiceSyncBlockerSummary where
  type
    AWSResponse GetServiceSyncBlockerSummary =
      GetServiceSyncBlockerSummaryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceSyncBlockerSummaryResponse'
            Prelude.<$> (x Data..?> "serviceSyncBlockerSummary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetServiceSyncBlockerSummary
  where
  hashWithSalt _salt GetServiceSyncBlockerSummary' {..} =
    _salt
      `Prelude.hashWithSalt` serviceInstanceName
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData GetServiceSyncBlockerSummary where
  rnf GetServiceSyncBlockerSummary' {..} =
    Prelude.rnf serviceInstanceName
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToHeaders GetServiceSyncBlockerSummary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetServiceSyncBlockerSummary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetServiceSyncBlockerSummary where
  toJSON GetServiceSyncBlockerSummary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("serviceInstanceName" Data..=)
              Prelude.<$> serviceInstanceName,
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )

instance Data.ToPath GetServiceSyncBlockerSummary where
  toPath = Prelude.const "/"

instance Data.ToQuery GetServiceSyncBlockerSummary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceSyncBlockerSummaryResponse' smart constructor.
data GetServiceSyncBlockerSummaryResponse = GetServiceSyncBlockerSummaryResponse'
  { -- | The detailed data of the requested service sync blocker summary.
    serviceSyncBlockerSummary :: Prelude.Maybe ServiceSyncBlockerSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceSyncBlockerSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSyncBlockerSummary', 'getServiceSyncBlockerSummaryResponse_serviceSyncBlockerSummary' - The detailed data of the requested service sync blocker summary.
--
-- 'httpStatus', 'getServiceSyncBlockerSummaryResponse_httpStatus' - The response's http status code.
newGetServiceSyncBlockerSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceSyncBlockerSummaryResponse
newGetServiceSyncBlockerSummaryResponse pHttpStatus_ =
  GetServiceSyncBlockerSummaryResponse'
    { serviceSyncBlockerSummary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detailed data of the requested service sync blocker summary.
getServiceSyncBlockerSummaryResponse_serviceSyncBlockerSummary :: Lens.Lens' GetServiceSyncBlockerSummaryResponse (Prelude.Maybe ServiceSyncBlockerSummary)
getServiceSyncBlockerSummaryResponse_serviceSyncBlockerSummary = Lens.lens (\GetServiceSyncBlockerSummaryResponse' {serviceSyncBlockerSummary} -> serviceSyncBlockerSummary) (\s@GetServiceSyncBlockerSummaryResponse' {} a -> s {serviceSyncBlockerSummary = a} :: GetServiceSyncBlockerSummaryResponse)

-- | The response's http status code.
getServiceSyncBlockerSummaryResponse_httpStatus :: Lens.Lens' GetServiceSyncBlockerSummaryResponse Prelude.Int
getServiceSyncBlockerSummaryResponse_httpStatus = Lens.lens (\GetServiceSyncBlockerSummaryResponse' {httpStatus} -> httpStatus) (\s@GetServiceSyncBlockerSummaryResponse' {} a -> s {httpStatus = a} :: GetServiceSyncBlockerSummaryResponse)

instance
  Prelude.NFData
    GetServiceSyncBlockerSummaryResponse
  where
  rnf GetServiceSyncBlockerSummaryResponse' {..} =
    Prelude.rnf serviceSyncBlockerSummary
      `Prelude.seq` Prelude.rnf httpStatus
