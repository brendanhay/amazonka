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
-- Module      : Amazonka.ServiceQuotas.DeleteServiceQuotaIncreaseRequestFromTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the quota increase request for the specified quota from your
-- quota request template.
module Amazonka.ServiceQuotas.DeleteServiceQuotaIncreaseRequestFromTemplate
  ( -- * Creating a Request
    DeleteServiceQuotaIncreaseRequestFromTemplate (..),
    newDeleteServiceQuotaIncreaseRequestFromTemplate,

    -- * Request Lenses
    deleteServiceQuotaIncreaseRequestFromTemplate_serviceCode,
    deleteServiceQuotaIncreaseRequestFromTemplate_quotaCode,
    deleteServiceQuotaIncreaseRequestFromTemplate_awsRegion,

    -- * Destructuring the Response
    DeleteServiceQuotaIncreaseRequestFromTemplateResponse (..),
    newDeleteServiceQuotaIncreaseRequestFromTemplateResponse,

    -- * Response Lenses
    deleteServiceQuotaIncreaseRequestFromTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceQuotas.Types

-- | /See:/ 'newDeleteServiceQuotaIncreaseRequestFromTemplate' smart constructor.
data DeleteServiceQuotaIncreaseRequestFromTemplate = DeleteServiceQuotaIncreaseRequestFromTemplate'
  { -- | The service identifier.
    serviceCode :: Prelude.Text,
    -- | The quota identifier.
    quotaCode :: Prelude.Text,
    -- | The AWS Region.
    awsRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceQuotaIncreaseRequestFromTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceCode', 'deleteServiceQuotaIncreaseRequestFromTemplate_serviceCode' - The service identifier.
--
-- 'quotaCode', 'deleteServiceQuotaIncreaseRequestFromTemplate_quotaCode' - The quota identifier.
--
-- 'awsRegion', 'deleteServiceQuotaIncreaseRequestFromTemplate_awsRegion' - The AWS Region.
newDeleteServiceQuotaIncreaseRequestFromTemplate ::
  -- | 'serviceCode'
  Prelude.Text ->
  -- | 'quotaCode'
  Prelude.Text ->
  -- | 'awsRegion'
  Prelude.Text ->
  DeleteServiceQuotaIncreaseRequestFromTemplate
newDeleteServiceQuotaIncreaseRequestFromTemplate
  pServiceCode_
  pQuotaCode_
  pAwsRegion_ =
    DeleteServiceQuotaIncreaseRequestFromTemplate'
      { serviceCode =
          pServiceCode_,
        quotaCode = pQuotaCode_,
        awsRegion = pAwsRegion_
      }

-- | The service identifier.
deleteServiceQuotaIncreaseRequestFromTemplate_serviceCode :: Lens.Lens' DeleteServiceQuotaIncreaseRequestFromTemplate Prelude.Text
deleteServiceQuotaIncreaseRequestFromTemplate_serviceCode = Lens.lens (\DeleteServiceQuotaIncreaseRequestFromTemplate' {serviceCode} -> serviceCode) (\s@DeleteServiceQuotaIncreaseRequestFromTemplate' {} a -> s {serviceCode = a} :: DeleteServiceQuotaIncreaseRequestFromTemplate)

-- | The quota identifier.
deleteServiceQuotaIncreaseRequestFromTemplate_quotaCode :: Lens.Lens' DeleteServiceQuotaIncreaseRequestFromTemplate Prelude.Text
deleteServiceQuotaIncreaseRequestFromTemplate_quotaCode = Lens.lens (\DeleteServiceQuotaIncreaseRequestFromTemplate' {quotaCode} -> quotaCode) (\s@DeleteServiceQuotaIncreaseRequestFromTemplate' {} a -> s {quotaCode = a} :: DeleteServiceQuotaIncreaseRequestFromTemplate)

-- | The AWS Region.
deleteServiceQuotaIncreaseRequestFromTemplate_awsRegion :: Lens.Lens' DeleteServiceQuotaIncreaseRequestFromTemplate Prelude.Text
deleteServiceQuotaIncreaseRequestFromTemplate_awsRegion = Lens.lens (\DeleteServiceQuotaIncreaseRequestFromTemplate' {awsRegion} -> awsRegion) (\s@DeleteServiceQuotaIncreaseRequestFromTemplate' {} a -> s {awsRegion = a} :: DeleteServiceQuotaIncreaseRequestFromTemplate)

instance
  Core.AWSRequest
    DeleteServiceQuotaIncreaseRequestFromTemplate
  where
  type
    AWSResponse
      DeleteServiceQuotaIncreaseRequestFromTemplate =
      DeleteServiceQuotaIncreaseRequestFromTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteServiceQuotaIncreaseRequestFromTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteServiceQuotaIncreaseRequestFromTemplate
  where
  hashWithSalt
    _salt
    DeleteServiceQuotaIncreaseRequestFromTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` serviceCode
        `Prelude.hashWithSalt` quotaCode
        `Prelude.hashWithSalt` awsRegion

instance
  Prelude.NFData
    DeleteServiceQuotaIncreaseRequestFromTemplate
  where
  rnf
    DeleteServiceQuotaIncreaseRequestFromTemplate' {..} =
      Prelude.rnf serviceCode `Prelude.seq`
        Prelude.rnf quotaCode `Prelude.seq`
          Prelude.rnf awsRegion

instance
  Data.ToHeaders
    DeleteServiceQuotaIncreaseRequestFromTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ServiceQuotasV20190624.DeleteServiceQuotaIncreaseRequestFromTemplate" ::
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
    DeleteServiceQuotaIncreaseRequestFromTemplate
  where
  toJSON
    DeleteServiceQuotaIncreaseRequestFromTemplate' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just ("ServiceCode" Data..= serviceCode),
              Prelude.Just ("QuotaCode" Data..= quotaCode),
              Prelude.Just ("AwsRegion" Data..= awsRegion)
            ]
        )

instance
  Data.ToPath
    DeleteServiceQuotaIncreaseRequestFromTemplate
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteServiceQuotaIncreaseRequestFromTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServiceQuotaIncreaseRequestFromTemplateResponse' smart constructor.
data DeleteServiceQuotaIncreaseRequestFromTemplateResponse = DeleteServiceQuotaIncreaseRequestFromTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceQuotaIncreaseRequestFromTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteServiceQuotaIncreaseRequestFromTemplateResponse_httpStatus' - The response's http status code.
newDeleteServiceQuotaIncreaseRequestFromTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServiceQuotaIncreaseRequestFromTemplateResponse
newDeleteServiceQuotaIncreaseRequestFromTemplateResponse
  pHttpStatus_ =
    DeleteServiceQuotaIncreaseRequestFromTemplateResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteServiceQuotaIncreaseRequestFromTemplateResponse_httpStatus :: Lens.Lens' DeleteServiceQuotaIncreaseRequestFromTemplateResponse Prelude.Int
deleteServiceQuotaIncreaseRequestFromTemplateResponse_httpStatus = Lens.lens (\DeleteServiceQuotaIncreaseRequestFromTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceQuotaIncreaseRequestFromTemplateResponse' {} a -> s {httpStatus = a} :: DeleteServiceQuotaIncreaseRequestFromTemplateResponse)

instance
  Prelude.NFData
    DeleteServiceQuotaIncreaseRequestFromTemplateResponse
  where
  rnf
    DeleteServiceQuotaIncreaseRequestFromTemplateResponse' {..} =
      Prelude.rnf httpStatus
