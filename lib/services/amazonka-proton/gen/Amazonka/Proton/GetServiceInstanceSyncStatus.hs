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
-- Module      : Amazonka.Proton.GetServiceInstanceSyncStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the status of the synced service instance.
module Amazonka.Proton.GetServiceInstanceSyncStatus
  ( -- * Creating a Request
    GetServiceInstanceSyncStatus (..),
    newGetServiceInstanceSyncStatus,

    -- * Request Lenses
    getServiceInstanceSyncStatus_serviceInstanceName,
    getServiceInstanceSyncStatus_serviceName,

    -- * Destructuring the Response
    GetServiceInstanceSyncStatusResponse (..),
    newGetServiceInstanceSyncStatusResponse,

    -- * Response Lenses
    getServiceInstanceSyncStatusResponse_desiredState,
    getServiceInstanceSyncStatusResponse_latestSuccessfulSync,
    getServiceInstanceSyncStatusResponse_latestSync,
    getServiceInstanceSyncStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServiceInstanceSyncStatus' smart constructor.
data GetServiceInstanceSyncStatus = GetServiceInstanceSyncStatus'
  { -- | The name of the service instance that you want the sync status input
    -- for.
    serviceInstanceName :: Prelude.Text,
    -- | The name of the service that the service instance belongs to.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceInstanceSyncStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceInstanceName', 'getServiceInstanceSyncStatus_serviceInstanceName' - The name of the service instance that you want the sync status input
-- for.
--
-- 'serviceName', 'getServiceInstanceSyncStatus_serviceName' - The name of the service that the service instance belongs to.
newGetServiceInstanceSyncStatus ::
  -- | 'serviceInstanceName'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  GetServiceInstanceSyncStatus
newGetServiceInstanceSyncStatus
  pServiceInstanceName_
  pServiceName_ =
    GetServiceInstanceSyncStatus'
      { serviceInstanceName =
          pServiceInstanceName_,
        serviceName = pServiceName_
      }

-- | The name of the service instance that you want the sync status input
-- for.
getServiceInstanceSyncStatus_serviceInstanceName :: Lens.Lens' GetServiceInstanceSyncStatus Prelude.Text
getServiceInstanceSyncStatus_serviceInstanceName = Lens.lens (\GetServiceInstanceSyncStatus' {serviceInstanceName} -> serviceInstanceName) (\s@GetServiceInstanceSyncStatus' {} a -> s {serviceInstanceName = a} :: GetServiceInstanceSyncStatus)

-- | The name of the service that the service instance belongs to.
getServiceInstanceSyncStatus_serviceName :: Lens.Lens' GetServiceInstanceSyncStatus Prelude.Text
getServiceInstanceSyncStatus_serviceName = Lens.lens (\GetServiceInstanceSyncStatus' {serviceName} -> serviceName) (\s@GetServiceInstanceSyncStatus' {} a -> s {serviceName = a} :: GetServiceInstanceSyncStatus)

instance Core.AWSRequest GetServiceInstanceSyncStatus where
  type
    AWSResponse GetServiceInstanceSyncStatus =
      GetServiceInstanceSyncStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceInstanceSyncStatusResponse'
            Prelude.<$> (x Data..?> "desiredState")
            Prelude.<*> (x Data..?> "latestSuccessfulSync")
            Prelude.<*> (x Data..?> "latestSync")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetServiceInstanceSyncStatus
  where
  hashWithSalt _salt GetServiceInstanceSyncStatus' {..} =
    _salt
      `Prelude.hashWithSalt` serviceInstanceName
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData GetServiceInstanceSyncStatus where
  rnf GetServiceInstanceSyncStatus' {..} =
    Prelude.rnf serviceInstanceName
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToHeaders GetServiceInstanceSyncStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetServiceInstanceSyncStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetServiceInstanceSyncStatus where
  toJSON GetServiceInstanceSyncStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("serviceInstanceName" Data..= serviceInstanceName),
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )

instance Data.ToPath GetServiceInstanceSyncStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetServiceInstanceSyncStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceInstanceSyncStatusResponse' smart constructor.
data GetServiceInstanceSyncStatusResponse = GetServiceInstanceSyncStatusResponse'
  { -- | The service instance sync desired state that\'s returned by Proton
    desiredState :: Prelude.Maybe Revision,
    -- | The detailed data of the latest successful sync with the service
    -- instance.
    latestSuccessfulSync :: Prelude.Maybe ResourceSyncAttempt,
    -- | The detailed data of the latest sync with the service instance.
    latestSync :: Prelude.Maybe ResourceSyncAttempt,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceInstanceSyncStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredState', 'getServiceInstanceSyncStatusResponse_desiredState' - The service instance sync desired state that\'s returned by Proton
--
-- 'latestSuccessfulSync', 'getServiceInstanceSyncStatusResponse_latestSuccessfulSync' - The detailed data of the latest successful sync with the service
-- instance.
--
-- 'latestSync', 'getServiceInstanceSyncStatusResponse_latestSync' - The detailed data of the latest sync with the service instance.
--
-- 'httpStatus', 'getServiceInstanceSyncStatusResponse_httpStatus' - The response's http status code.
newGetServiceInstanceSyncStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceInstanceSyncStatusResponse
newGetServiceInstanceSyncStatusResponse pHttpStatus_ =
  GetServiceInstanceSyncStatusResponse'
    { desiredState =
        Prelude.Nothing,
      latestSuccessfulSync =
        Prelude.Nothing,
      latestSync = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The service instance sync desired state that\'s returned by Proton
getServiceInstanceSyncStatusResponse_desiredState :: Lens.Lens' GetServiceInstanceSyncStatusResponse (Prelude.Maybe Revision)
getServiceInstanceSyncStatusResponse_desiredState = Lens.lens (\GetServiceInstanceSyncStatusResponse' {desiredState} -> desiredState) (\s@GetServiceInstanceSyncStatusResponse' {} a -> s {desiredState = a} :: GetServiceInstanceSyncStatusResponse)

-- | The detailed data of the latest successful sync with the service
-- instance.
getServiceInstanceSyncStatusResponse_latestSuccessfulSync :: Lens.Lens' GetServiceInstanceSyncStatusResponse (Prelude.Maybe ResourceSyncAttempt)
getServiceInstanceSyncStatusResponse_latestSuccessfulSync = Lens.lens (\GetServiceInstanceSyncStatusResponse' {latestSuccessfulSync} -> latestSuccessfulSync) (\s@GetServiceInstanceSyncStatusResponse' {} a -> s {latestSuccessfulSync = a} :: GetServiceInstanceSyncStatusResponse)

-- | The detailed data of the latest sync with the service instance.
getServiceInstanceSyncStatusResponse_latestSync :: Lens.Lens' GetServiceInstanceSyncStatusResponse (Prelude.Maybe ResourceSyncAttempt)
getServiceInstanceSyncStatusResponse_latestSync = Lens.lens (\GetServiceInstanceSyncStatusResponse' {latestSync} -> latestSync) (\s@GetServiceInstanceSyncStatusResponse' {} a -> s {latestSync = a} :: GetServiceInstanceSyncStatusResponse)

-- | The response's http status code.
getServiceInstanceSyncStatusResponse_httpStatus :: Lens.Lens' GetServiceInstanceSyncStatusResponse Prelude.Int
getServiceInstanceSyncStatusResponse_httpStatus = Lens.lens (\GetServiceInstanceSyncStatusResponse' {httpStatus} -> httpStatus) (\s@GetServiceInstanceSyncStatusResponse' {} a -> s {httpStatus = a} :: GetServiceInstanceSyncStatusResponse)

instance
  Prelude.NFData
    GetServiceInstanceSyncStatusResponse
  where
  rnf GetServiceInstanceSyncStatusResponse' {..} =
    Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf latestSuccessfulSync
      `Prelude.seq` Prelude.rnf latestSync
      `Prelude.seq` Prelude.rnf httpStatus
