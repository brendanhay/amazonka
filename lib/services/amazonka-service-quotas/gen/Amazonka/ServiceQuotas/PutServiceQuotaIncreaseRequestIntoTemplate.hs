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
-- Module      : Amazonka.ServiceQuotas.PutServiceQuotaIncreaseRequestIntoTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a quota increase request to your quota request template.
module Amazonka.ServiceQuotas.PutServiceQuotaIncreaseRequestIntoTemplate
  ( -- * Creating a Request
    PutServiceQuotaIncreaseRequestIntoTemplate (..),
    newPutServiceQuotaIncreaseRequestIntoTemplate,

    -- * Request Lenses
    putServiceQuotaIncreaseRequestIntoTemplate_quotaCode,
    putServiceQuotaIncreaseRequestIntoTemplate_serviceCode,
    putServiceQuotaIncreaseRequestIntoTemplate_awsRegion,
    putServiceQuotaIncreaseRequestIntoTemplate_desiredValue,

    -- * Destructuring the Response
    PutServiceQuotaIncreaseRequestIntoTemplateResponse (..),
    newPutServiceQuotaIncreaseRequestIntoTemplateResponse,

    -- * Response Lenses
    putServiceQuotaIncreaseRequestIntoTemplateResponse_serviceQuotaIncreaseRequestInTemplate,
    putServiceQuotaIncreaseRequestIntoTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceQuotas.Types

-- | /See:/ 'newPutServiceQuotaIncreaseRequestIntoTemplate' smart constructor.
data PutServiceQuotaIncreaseRequestIntoTemplate = PutServiceQuotaIncreaseRequestIntoTemplate'
  { -- | The quota identifier.
    quotaCode :: Prelude.Text,
    -- | The service identifier.
    serviceCode :: Prelude.Text,
    -- | The AWS Region.
    awsRegion :: Prelude.Text,
    -- | The new, increased value for the quota.
    desiredValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutServiceQuotaIncreaseRequestIntoTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quotaCode', 'putServiceQuotaIncreaseRequestIntoTemplate_quotaCode' - The quota identifier.
--
-- 'serviceCode', 'putServiceQuotaIncreaseRequestIntoTemplate_serviceCode' - The service identifier.
--
-- 'awsRegion', 'putServiceQuotaIncreaseRequestIntoTemplate_awsRegion' - The AWS Region.
--
-- 'desiredValue', 'putServiceQuotaIncreaseRequestIntoTemplate_desiredValue' - The new, increased value for the quota.
newPutServiceQuotaIncreaseRequestIntoTemplate ::
  -- | 'quotaCode'
  Prelude.Text ->
  -- | 'serviceCode'
  Prelude.Text ->
  -- | 'awsRegion'
  Prelude.Text ->
  -- | 'desiredValue'
  Prelude.Double ->
  PutServiceQuotaIncreaseRequestIntoTemplate
newPutServiceQuotaIncreaseRequestIntoTemplate
  pQuotaCode_
  pServiceCode_
  pAwsRegion_
  pDesiredValue_ =
    PutServiceQuotaIncreaseRequestIntoTemplate'
      { quotaCode =
          pQuotaCode_,
        serviceCode = pServiceCode_,
        awsRegion = pAwsRegion_,
        desiredValue = pDesiredValue_
      }

-- | The quota identifier.
putServiceQuotaIncreaseRequestIntoTemplate_quotaCode :: Lens.Lens' PutServiceQuotaIncreaseRequestIntoTemplate Prelude.Text
putServiceQuotaIncreaseRequestIntoTemplate_quotaCode = Lens.lens (\PutServiceQuotaIncreaseRequestIntoTemplate' {quotaCode} -> quotaCode) (\s@PutServiceQuotaIncreaseRequestIntoTemplate' {} a -> s {quotaCode = a} :: PutServiceQuotaIncreaseRequestIntoTemplate)

-- | The service identifier.
putServiceQuotaIncreaseRequestIntoTemplate_serviceCode :: Lens.Lens' PutServiceQuotaIncreaseRequestIntoTemplate Prelude.Text
putServiceQuotaIncreaseRequestIntoTemplate_serviceCode = Lens.lens (\PutServiceQuotaIncreaseRequestIntoTemplate' {serviceCode} -> serviceCode) (\s@PutServiceQuotaIncreaseRequestIntoTemplate' {} a -> s {serviceCode = a} :: PutServiceQuotaIncreaseRequestIntoTemplate)

-- | The AWS Region.
putServiceQuotaIncreaseRequestIntoTemplate_awsRegion :: Lens.Lens' PutServiceQuotaIncreaseRequestIntoTemplate Prelude.Text
putServiceQuotaIncreaseRequestIntoTemplate_awsRegion = Lens.lens (\PutServiceQuotaIncreaseRequestIntoTemplate' {awsRegion} -> awsRegion) (\s@PutServiceQuotaIncreaseRequestIntoTemplate' {} a -> s {awsRegion = a} :: PutServiceQuotaIncreaseRequestIntoTemplate)

-- | The new, increased value for the quota.
putServiceQuotaIncreaseRequestIntoTemplate_desiredValue :: Lens.Lens' PutServiceQuotaIncreaseRequestIntoTemplate Prelude.Double
putServiceQuotaIncreaseRequestIntoTemplate_desiredValue = Lens.lens (\PutServiceQuotaIncreaseRequestIntoTemplate' {desiredValue} -> desiredValue) (\s@PutServiceQuotaIncreaseRequestIntoTemplate' {} a -> s {desiredValue = a} :: PutServiceQuotaIncreaseRequestIntoTemplate)

instance
  Core.AWSRequest
    PutServiceQuotaIncreaseRequestIntoTemplate
  where
  type
    AWSResponse
      PutServiceQuotaIncreaseRequestIntoTemplate =
      PutServiceQuotaIncreaseRequestIntoTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutServiceQuotaIncreaseRequestIntoTemplateResponse'
            Prelude.<$> (x Data..?> "ServiceQuotaIncreaseRequestInTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutServiceQuotaIncreaseRequestIntoTemplate
  where
  hashWithSalt
    _salt
    PutServiceQuotaIncreaseRequestIntoTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` quotaCode
        `Prelude.hashWithSalt` serviceCode
        `Prelude.hashWithSalt` awsRegion
        `Prelude.hashWithSalt` desiredValue

instance
  Prelude.NFData
    PutServiceQuotaIncreaseRequestIntoTemplate
  where
  rnf PutServiceQuotaIncreaseRequestIntoTemplate' {..} =
    Prelude.rnf quotaCode `Prelude.seq`
      Prelude.rnf serviceCode `Prelude.seq`
        Prelude.rnf awsRegion `Prelude.seq`
          Prelude.rnf desiredValue

instance
  Data.ToHeaders
    PutServiceQuotaIncreaseRequestIntoTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ServiceQuotasV20190624.PutServiceQuotaIncreaseRequestIntoTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    PutServiceQuotaIncreaseRequestIntoTemplate
  where
  toJSON
    PutServiceQuotaIncreaseRequestIntoTemplate' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just ("QuotaCode" Data..= quotaCode),
              Prelude.Just ("ServiceCode" Data..= serviceCode),
              Prelude.Just ("AwsRegion" Data..= awsRegion),
              Prelude.Just ("DesiredValue" Data..= desiredValue)
            ]
        )

instance
  Data.ToPath
    PutServiceQuotaIncreaseRequestIntoTemplate
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    PutServiceQuotaIncreaseRequestIntoTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutServiceQuotaIncreaseRequestIntoTemplateResponse' smart constructor.
data PutServiceQuotaIncreaseRequestIntoTemplateResponse = PutServiceQuotaIncreaseRequestIntoTemplateResponse'
  { -- | Information about the quota increase request.
    serviceQuotaIncreaseRequestInTemplate :: Prelude.Maybe ServiceQuotaIncreaseRequestInTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutServiceQuotaIncreaseRequestIntoTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceQuotaIncreaseRequestInTemplate', 'putServiceQuotaIncreaseRequestIntoTemplateResponse_serviceQuotaIncreaseRequestInTemplate' - Information about the quota increase request.
--
-- 'httpStatus', 'putServiceQuotaIncreaseRequestIntoTemplateResponse_httpStatus' - The response's http status code.
newPutServiceQuotaIncreaseRequestIntoTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutServiceQuotaIncreaseRequestIntoTemplateResponse
newPutServiceQuotaIncreaseRequestIntoTemplateResponse
  pHttpStatus_ =
    PutServiceQuotaIncreaseRequestIntoTemplateResponse'
      { serviceQuotaIncreaseRequestInTemplate =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the quota increase request.
putServiceQuotaIncreaseRequestIntoTemplateResponse_serviceQuotaIncreaseRequestInTemplate :: Lens.Lens' PutServiceQuotaIncreaseRequestIntoTemplateResponse (Prelude.Maybe ServiceQuotaIncreaseRequestInTemplate)
putServiceQuotaIncreaseRequestIntoTemplateResponse_serviceQuotaIncreaseRequestInTemplate = Lens.lens (\PutServiceQuotaIncreaseRequestIntoTemplateResponse' {serviceQuotaIncreaseRequestInTemplate} -> serviceQuotaIncreaseRequestInTemplate) (\s@PutServiceQuotaIncreaseRequestIntoTemplateResponse' {} a -> s {serviceQuotaIncreaseRequestInTemplate = a} :: PutServiceQuotaIncreaseRequestIntoTemplateResponse)

-- | The response's http status code.
putServiceQuotaIncreaseRequestIntoTemplateResponse_httpStatus :: Lens.Lens' PutServiceQuotaIncreaseRequestIntoTemplateResponse Prelude.Int
putServiceQuotaIncreaseRequestIntoTemplateResponse_httpStatus = Lens.lens (\PutServiceQuotaIncreaseRequestIntoTemplateResponse' {httpStatus} -> httpStatus) (\s@PutServiceQuotaIncreaseRequestIntoTemplateResponse' {} a -> s {httpStatus = a} :: PutServiceQuotaIncreaseRequestIntoTemplateResponse)

instance
  Prelude.NFData
    PutServiceQuotaIncreaseRequestIntoTemplateResponse
  where
  rnf
    PutServiceQuotaIncreaseRequestIntoTemplateResponse' {..} =
      Prelude.rnf serviceQuotaIncreaseRequestInTemplate `Prelude.seq`
        Prelude.rnf httpStatus
