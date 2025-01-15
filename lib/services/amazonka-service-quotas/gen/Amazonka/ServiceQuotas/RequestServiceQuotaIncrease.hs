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
-- Module      : Amazonka.ServiceQuotas.RequestServiceQuotaIncrease
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a quota increase request for the specified quota.
module Amazonka.ServiceQuotas.RequestServiceQuotaIncrease
  ( -- * Creating a Request
    RequestServiceQuotaIncrease (..),
    newRequestServiceQuotaIncrease,

    -- * Request Lenses
    requestServiceQuotaIncrease_serviceCode,
    requestServiceQuotaIncrease_quotaCode,
    requestServiceQuotaIncrease_desiredValue,

    -- * Destructuring the Response
    RequestServiceQuotaIncreaseResponse (..),
    newRequestServiceQuotaIncreaseResponse,

    -- * Response Lenses
    requestServiceQuotaIncreaseResponse_requestedQuota,
    requestServiceQuotaIncreaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceQuotas.Types

-- | /See:/ 'newRequestServiceQuotaIncrease' smart constructor.
data RequestServiceQuotaIncrease = RequestServiceQuotaIncrease'
  { -- | The service identifier.
    serviceCode :: Prelude.Text,
    -- | The quota identifier.
    quotaCode :: Prelude.Text,
    -- | The new, increased value for the quota.
    desiredValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestServiceQuotaIncrease' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceCode', 'requestServiceQuotaIncrease_serviceCode' - The service identifier.
--
-- 'quotaCode', 'requestServiceQuotaIncrease_quotaCode' - The quota identifier.
--
-- 'desiredValue', 'requestServiceQuotaIncrease_desiredValue' - The new, increased value for the quota.
newRequestServiceQuotaIncrease ::
  -- | 'serviceCode'
  Prelude.Text ->
  -- | 'quotaCode'
  Prelude.Text ->
  -- | 'desiredValue'
  Prelude.Double ->
  RequestServiceQuotaIncrease
newRequestServiceQuotaIncrease
  pServiceCode_
  pQuotaCode_
  pDesiredValue_ =
    RequestServiceQuotaIncrease'
      { serviceCode =
          pServiceCode_,
        quotaCode = pQuotaCode_,
        desiredValue = pDesiredValue_
      }

-- | The service identifier.
requestServiceQuotaIncrease_serviceCode :: Lens.Lens' RequestServiceQuotaIncrease Prelude.Text
requestServiceQuotaIncrease_serviceCode = Lens.lens (\RequestServiceQuotaIncrease' {serviceCode} -> serviceCode) (\s@RequestServiceQuotaIncrease' {} a -> s {serviceCode = a} :: RequestServiceQuotaIncrease)

-- | The quota identifier.
requestServiceQuotaIncrease_quotaCode :: Lens.Lens' RequestServiceQuotaIncrease Prelude.Text
requestServiceQuotaIncrease_quotaCode = Lens.lens (\RequestServiceQuotaIncrease' {quotaCode} -> quotaCode) (\s@RequestServiceQuotaIncrease' {} a -> s {quotaCode = a} :: RequestServiceQuotaIncrease)

-- | The new, increased value for the quota.
requestServiceQuotaIncrease_desiredValue :: Lens.Lens' RequestServiceQuotaIncrease Prelude.Double
requestServiceQuotaIncrease_desiredValue = Lens.lens (\RequestServiceQuotaIncrease' {desiredValue} -> desiredValue) (\s@RequestServiceQuotaIncrease' {} a -> s {desiredValue = a} :: RequestServiceQuotaIncrease)

instance Core.AWSRequest RequestServiceQuotaIncrease where
  type
    AWSResponse RequestServiceQuotaIncrease =
      RequestServiceQuotaIncreaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RequestServiceQuotaIncreaseResponse'
            Prelude.<$> (x Data..?> "RequestedQuota")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RequestServiceQuotaIncrease where
  hashWithSalt _salt RequestServiceQuotaIncrease' {..} =
    _salt
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` quotaCode
      `Prelude.hashWithSalt` desiredValue

instance Prelude.NFData RequestServiceQuotaIncrease where
  rnf RequestServiceQuotaIncrease' {..} =
    Prelude.rnf serviceCode `Prelude.seq`
      Prelude.rnf quotaCode `Prelude.seq`
        Prelude.rnf desiredValue

instance Data.ToHeaders RequestServiceQuotaIncrease where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ServiceQuotasV20190624.RequestServiceQuotaIncrease" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RequestServiceQuotaIncrease where
  toJSON RequestServiceQuotaIncrease' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServiceCode" Data..= serviceCode),
            Prelude.Just ("QuotaCode" Data..= quotaCode),
            Prelude.Just ("DesiredValue" Data..= desiredValue)
          ]
      )

instance Data.ToPath RequestServiceQuotaIncrease where
  toPath = Prelude.const "/"

instance Data.ToQuery RequestServiceQuotaIncrease where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRequestServiceQuotaIncreaseResponse' smart constructor.
data RequestServiceQuotaIncreaseResponse = RequestServiceQuotaIncreaseResponse'
  { -- | Information about the quota increase request.
    requestedQuota :: Prelude.Maybe RequestedServiceQuotaChange,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestServiceQuotaIncreaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestedQuota', 'requestServiceQuotaIncreaseResponse_requestedQuota' - Information about the quota increase request.
--
-- 'httpStatus', 'requestServiceQuotaIncreaseResponse_httpStatus' - The response's http status code.
newRequestServiceQuotaIncreaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RequestServiceQuotaIncreaseResponse
newRequestServiceQuotaIncreaseResponse pHttpStatus_ =
  RequestServiceQuotaIncreaseResponse'
    { requestedQuota =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the quota increase request.
requestServiceQuotaIncreaseResponse_requestedQuota :: Lens.Lens' RequestServiceQuotaIncreaseResponse (Prelude.Maybe RequestedServiceQuotaChange)
requestServiceQuotaIncreaseResponse_requestedQuota = Lens.lens (\RequestServiceQuotaIncreaseResponse' {requestedQuota} -> requestedQuota) (\s@RequestServiceQuotaIncreaseResponse' {} a -> s {requestedQuota = a} :: RequestServiceQuotaIncreaseResponse)

-- | The response's http status code.
requestServiceQuotaIncreaseResponse_httpStatus :: Lens.Lens' RequestServiceQuotaIncreaseResponse Prelude.Int
requestServiceQuotaIncreaseResponse_httpStatus = Lens.lens (\RequestServiceQuotaIncreaseResponse' {httpStatus} -> httpStatus) (\s@RequestServiceQuotaIncreaseResponse' {} a -> s {httpStatus = a} :: RequestServiceQuotaIncreaseResponse)

instance
  Prelude.NFData
    RequestServiceQuotaIncreaseResponse
  where
  rnf RequestServiceQuotaIncreaseResponse' {..} =
    Prelude.rnf requestedQuota `Prelude.seq`
      Prelude.rnf httpStatus
