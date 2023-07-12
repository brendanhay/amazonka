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
-- Module      : Amazonka.ServiceQuotas.GetAssociationForServiceQuotaTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of the association for the quota request template.
module Amazonka.ServiceQuotas.GetAssociationForServiceQuotaTemplate
  ( -- * Creating a Request
    GetAssociationForServiceQuotaTemplate (..),
    newGetAssociationForServiceQuotaTemplate,

    -- * Destructuring the Response
    GetAssociationForServiceQuotaTemplateResponse (..),
    newGetAssociationForServiceQuotaTemplateResponse,

    -- * Response Lenses
    getAssociationForServiceQuotaTemplateResponse_serviceQuotaTemplateAssociationStatus,
    getAssociationForServiceQuotaTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceQuotas.Types

-- | /See:/ 'newGetAssociationForServiceQuotaTemplate' smart constructor.
data GetAssociationForServiceQuotaTemplate = GetAssociationForServiceQuotaTemplate'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssociationForServiceQuotaTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAssociationForServiceQuotaTemplate ::
  GetAssociationForServiceQuotaTemplate
newGetAssociationForServiceQuotaTemplate =
  GetAssociationForServiceQuotaTemplate'

instance
  Core.AWSRequest
    GetAssociationForServiceQuotaTemplate
  where
  type
    AWSResponse
      GetAssociationForServiceQuotaTemplate =
      GetAssociationForServiceQuotaTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssociationForServiceQuotaTemplateResponse'
            Prelude.<$> (x Data..?> "ServiceQuotaTemplateAssociationStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAssociationForServiceQuotaTemplate
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetAssociationForServiceQuotaTemplate
  where
  rnf _ = ()

instance
  Data.ToHeaders
    GetAssociationForServiceQuotaTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ServiceQuotasV20190624.GetAssociationForServiceQuotaTemplate" ::
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
    GetAssociationForServiceQuotaTemplate
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    GetAssociationForServiceQuotaTemplate
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetAssociationForServiceQuotaTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssociationForServiceQuotaTemplateResponse' smart constructor.
data GetAssociationForServiceQuotaTemplateResponse = GetAssociationForServiceQuotaTemplateResponse'
  { -- | The association status. If the status is @ASSOCIATED@, the quota
    -- increase requests in the template are automatically applied to new
    -- accounts in your organization.
    serviceQuotaTemplateAssociationStatus :: Prelude.Maybe ServiceQuotaTemplateAssociationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssociationForServiceQuotaTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceQuotaTemplateAssociationStatus', 'getAssociationForServiceQuotaTemplateResponse_serviceQuotaTemplateAssociationStatus' - The association status. If the status is @ASSOCIATED@, the quota
-- increase requests in the template are automatically applied to new
-- accounts in your organization.
--
-- 'httpStatus', 'getAssociationForServiceQuotaTemplateResponse_httpStatus' - The response's http status code.
newGetAssociationForServiceQuotaTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssociationForServiceQuotaTemplateResponse
newGetAssociationForServiceQuotaTemplateResponse
  pHttpStatus_ =
    GetAssociationForServiceQuotaTemplateResponse'
      { serviceQuotaTemplateAssociationStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The association status. If the status is @ASSOCIATED@, the quota
-- increase requests in the template are automatically applied to new
-- accounts in your organization.
getAssociationForServiceQuotaTemplateResponse_serviceQuotaTemplateAssociationStatus :: Lens.Lens' GetAssociationForServiceQuotaTemplateResponse (Prelude.Maybe ServiceQuotaTemplateAssociationStatus)
getAssociationForServiceQuotaTemplateResponse_serviceQuotaTemplateAssociationStatus = Lens.lens (\GetAssociationForServiceQuotaTemplateResponse' {serviceQuotaTemplateAssociationStatus} -> serviceQuotaTemplateAssociationStatus) (\s@GetAssociationForServiceQuotaTemplateResponse' {} a -> s {serviceQuotaTemplateAssociationStatus = a} :: GetAssociationForServiceQuotaTemplateResponse)

-- | The response's http status code.
getAssociationForServiceQuotaTemplateResponse_httpStatus :: Lens.Lens' GetAssociationForServiceQuotaTemplateResponse Prelude.Int
getAssociationForServiceQuotaTemplateResponse_httpStatus = Lens.lens (\GetAssociationForServiceQuotaTemplateResponse' {httpStatus} -> httpStatus) (\s@GetAssociationForServiceQuotaTemplateResponse' {} a -> s {httpStatus = a} :: GetAssociationForServiceQuotaTemplateResponse)

instance
  Prelude.NFData
    GetAssociationForServiceQuotaTemplateResponse
  where
  rnf
    GetAssociationForServiceQuotaTemplateResponse' {..} =
      Prelude.rnf serviceQuotaTemplateAssociationStatus
        `Prelude.seq` Prelude.rnf httpStatus
