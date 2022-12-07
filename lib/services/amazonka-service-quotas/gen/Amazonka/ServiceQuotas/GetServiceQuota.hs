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
-- Module      : Amazonka.ServiceQuotas.GetServiceQuota
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the applied quota value for the specified quota. For some
-- quotas, only the default values are available. If the applied quota
-- value is not available for a quota, the quota is not retrieved.
module Amazonka.ServiceQuotas.GetServiceQuota
  ( -- * Creating a Request
    GetServiceQuota (..),
    newGetServiceQuota,

    -- * Request Lenses
    getServiceQuota_serviceCode,
    getServiceQuota_quotaCode,

    -- * Destructuring the Response
    GetServiceQuotaResponse (..),
    newGetServiceQuotaResponse,

    -- * Response Lenses
    getServiceQuotaResponse_quota,
    getServiceQuotaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceQuotas.Types

-- | /See:/ 'newGetServiceQuota' smart constructor.
data GetServiceQuota = GetServiceQuota'
  { -- | The service identifier.
    serviceCode :: Prelude.Text,
    -- | The quota identifier.
    quotaCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceQuota' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceCode', 'getServiceQuota_serviceCode' - The service identifier.
--
-- 'quotaCode', 'getServiceQuota_quotaCode' - The quota identifier.
newGetServiceQuota ::
  -- | 'serviceCode'
  Prelude.Text ->
  -- | 'quotaCode'
  Prelude.Text ->
  GetServiceQuota
newGetServiceQuota pServiceCode_ pQuotaCode_ =
  GetServiceQuota'
    { serviceCode = pServiceCode_,
      quotaCode = pQuotaCode_
    }

-- | The service identifier.
getServiceQuota_serviceCode :: Lens.Lens' GetServiceQuota Prelude.Text
getServiceQuota_serviceCode = Lens.lens (\GetServiceQuota' {serviceCode} -> serviceCode) (\s@GetServiceQuota' {} a -> s {serviceCode = a} :: GetServiceQuota)

-- | The quota identifier.
getServiceQuota_quotaCode :: Lens.Lens' GetServiceQuota Prelude.Text
getServiceQuota_quotaCode = Lens.lens (\GetServiceQuota' {quotaCode} -> quotaCode) (\s@GetServiceQuota' {} a -> s {quotaCode = a} :: GetServiceQuota)

instance Core.AWSRequest GetServiceQuota where
  type
    AWSResponse GetServiceQuota =
      GetServiceQuotaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceQuotaResponse'
            Prelude.<$> (x Data..?> "Quota")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServiceQuota where
  hashWithSalt _salt GetServiceQuota' {..} =
    _salt `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` quotaCode

instance Prelude.NFData GetServiceQuota where
  rnf GetServiceQuota' {..} =
    Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf quotaCode

instance Data.ToHeaders GetServiceQuota where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ServiceQuotasV20190624.GetServiceQuota" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetServiceQuota where
  toJSON GetServiceQuota' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServiceCode" Data..= serviceCode),
            Prelude.Just ("QuotaCode" Data..= quotaCode)
          ]
      )

instance Data.ToPath GetServiceQuota where
  toPath = Prelude.const "/"

instance Data.ToQuery GetServiceQuota where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceQuotaResponse' smart constructor.
data GetServiceQuotaResponse = GetServiceQuotaResponse'
  { -- | Information about the quota.
    quota :: Prelude.Maybe ServiceQuota,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceQuotaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quota', 'getServiceQuotaResponse_quota' - Information about the quota.
--
-- 'httpStatus', 'getServiceQuotaResponse_httpStatus' - The response's http status code.
newGetServiceQuotaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceQuotaResponse
newGetServiceQuotaResponse pHttpStatus_ =
  GetServiceQuotaResponse'
    { quota = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the quota.
getServiceQuotaResponse_quota :: Lens.Lens' GetServiceQuotaResponse (Prelude.Maybe ServiceQuota)
getServiceQuotaResponse_quota = Lens.lens (\GetServiceQuotaResponse' {quota} -> quota) (\s@GetServiceQuotaResponse' {} a -> s {quota = a} :: GetServiceQuotaResponse)

-- | The response's http status code.
getServiceQuotaResponse_httpStatus :: Lens.Lens' GetServiceQuotaResponse Prelude.Int
getServiceQuotaResponse_httpStatus = Lens.lens (\GetServiceQuotaResponse' {httpStatus} -> httpStatus) (\s@GetServiceQuotaResponse' {} a -> s {httpStatus = a} :: GetServiceQuotaResponse)

instance Prelude.NFData GetServiceQuotaResponse where
  rnf GetServiceQuotaResponse' {..} =
    Prelude.rnf quota
      `Prelude.seq` Prelude.rnf httpStatus
