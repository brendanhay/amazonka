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
-- Module      : Amazonka.ConnectCampaigns.GetInstanceOnboardingJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the specific instance onboarding job status.
module Amazonka.ConnectCampaigns.GetInstanceOnboardingJobStatus
  ( -- * Creating a Request
    GetInstanceOnboardingJobStatus (..),
    newGetInstanceOnboardingJobStatus,

    -- * Request Lenses
    getInstanceOnboardingJobStatus_connectInstanceId,

    -- * Destructuring the Response
    GetInstanceOnboardingJobStatusResponse (..),
    newGetInstanceOnboardingJobStatusResponse,

    -- * Response Lenses
    getInstanceOnboardingJobStatusResponse_connectInstanceOnboardingJobStatus,
    getInstanceOnboardingJobStatusResponse_httpStatus,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | GetInstanceOnboardingJobStatusRequest
--
-- /See:/ 'newGetInstanceOnboardingJobStatus' smart constructor.
data GetInstanceOnboardingJobStatus = GetInstanceOnboardingJobStatus'
  { connectInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceOnboardingJobStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectInstanceId', 'getInstanceOnboardingJobStatus_connectInstanceId' - Undocumented member.
newGetInstanceOnboardingJobStatus ::
  -- | 'connectInstanceId'
  Prelude.Text ->
  GetInstanceOnboardingJobStatus
newGetInstanceOnboardingJobStatus pConnectInstanceId_ =
  GetInstanceOnboardingJobStatus'
    { connectInstanceId =
        pConnectInstanceId_
    }

-- | Undocumented member.
getInstanceOnboardingJobStatus_connectInstanceId :: Lens.Lens' GetInstanceOnboardingJobStatus Prelude.Text
getInstanceOnboardingJobStatus_connectInstanceId = Lens.lens (\GetInstanceOnboardingJobStatus' {connectInstanceId} -> connectInstanceId) (\s@GetInstanceOnboardingJobStatus' {} a -> s {connectInstanceId = a} :: GetInstanceOnboardingJobStatus)

instance
  Core.AWSRequest
    GetInstanceOnboardingJobStatus
  where
  type
    AWSResponse GetInstanceOnboardingJobStatus =
      GetInstanceOnboardingJobStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceOnboardingJobStatusResponse'
            Prelude.<$> (x Data..?> "connectInstanceOnboardingJobStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetInstanceOnboardingJobStatus
  where
  hashWithSalt
    _salt
    GetInstanceOnboardingJobStatus' {..} =
      _salt `Prelude.hashWithSalt` connectInstanceId

instance
  Prelude.NFData
    GetInstanceOnboardingJobStatus
  where
  rnf GetInstanceOnboardingJobStatus' {..} =
    Prelude.rnf connectInstanceId

instance
  Data.ToHeaders
    GetInstanceOnboardingJobStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetInstanceOnboardingJobStatus where
  toPath GetInstanceOnboardingJobStatus' {..} =
    Prelude.mconcat
      [ "/connect-instance/",
        Data.toBS connectInstanceId,
        "/onboarding"
      ]

instance Data.ToQuery GetInstanceOnboardingJobStatus where
  toQuery = Prelude.const Prelude.mempty

-- | GetInstanceOnboardingJobStatusResponse
--
-- /See:/ 'newGetInstanceOnboardingJobStatusResponse' smart constructor.
data GetInstanceOnboardingJobStatusResponse = GetInstanceOnboardingJobStatusResponse'
  { connectInstanceOnboardingJobStatus :: Prelude.Maybe InstanceOnboardingJobStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceOnboardingJobStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectInstanceOnboardingJobStatus', 'getInstanceOnboardingJobStatusResponse_connectInstanceOnboardingJobStatus' - Undocumented member.
--
-- 'httpStatus', 'getInstanceOnboardingJobStatusResponse_httpStatus' - The response's http status code.
newGetInstanceOnboardingJobStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInstanceOnboardingJobStatusResponse
newGetInstanceOnboardingJobStatusResponse
  pHttpStatus_ =
    GetInstanceOnboardingJobStatusResponse'
      { connectInstanceOnboardingJobStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
getInstanceOnboardingJobStatusResponse_connectInstanceOnboardingJobStatus :: Lens.Lens' GetInstanceOnboardingJobStatusResponse (Prelude.Maybe InstanceOnboardingJobStatus)
getInstanceOnboardingJobStatusResponse_connectInstanceOnboardingJobStatus = Lens.lens (\GetInstanceOnboardingJobStatusResponse' {connectInstanceOnboardingJobStatus} -> connectInstanceOnboardingJobStatus) (\s@GetInstanceOnboardingJobStatusResponse' {} a -> s {connectInstanceOnboardingJobStatus = a} :: GetInstanceOnboardingJobStatusResponse)

-- | The response's http status code.
getInstanceOnboardingJobStatusResponse_httpStatus :: Lens.Lens' GetInstanceOnboardingJobStatusResponse Prelude.Int
getInstanceOnboardingJobStatusResponse_httpStatus = Lens.lens (\GetInstanceOnboardingJobStatusResponse' {httpStatus} -> httpStatus) (\s@GetInstanceOnboardingJobStatusResponse' {} a -> s {httpStatus = a} :: GetInstanceOnboardingJobStatusResponse)

instance
  Prelude.NFData
    GetInstanceOnboardingJobStatusResponse
  where
  rnf GetInstanceOnboardingJobStatusResponse' {..} =
    Prelude.rnf connectInstanceOnboardingJobStatus
      `Prelude.seq` Prelude.rnf httpStatus
