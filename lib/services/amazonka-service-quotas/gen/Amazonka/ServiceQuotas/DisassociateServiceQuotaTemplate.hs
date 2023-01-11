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
-- Module      : Amazonka.ServiceQuotas.DisassociateServiceQuotaTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables your quota request template. After a template is disabled, the
-- quota increase requests in the template are not applied to new accounts
-- in your organization. Disabling a quota request template does not apply
-- its quota increase requests.
module Amazonka.ServiceQuotas.DisassociateServiceQuotaTemplate
  ( -- * Creating a Request
    DisassociateServiceQuotaTemplate (..),
    newDisassociateServiceQuotaTemplate,

    -- * Destructuring the Response
    DisassociateServiceQuotaTemplateResponse (..),
    newDisassociateServiceQuotaTemplateResponse,

    -- * Response Lenses
    disassociateServiceQuotaTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceQuotas.Types

-- | /See:/ 'newDisassociateServiceQuotaTemplate' smart constructor.
data DisassociateServiceQuotaTemplate = DisassociateServiceQuotaTemplate'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateServiceQuotaTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateServiceQuotaTemplate ::
  DisassociateServiceQuotaTemplate
newDisassociateServiceQuotaTemplate =
  DisassociateServiceQuotaTemplate'

instance
  Core.AWSRequest
    DisassociateServiceQuotaTemplate
  where
  type
    AWSResponse DisassociateServiceQuotaTemplate =
      DisassociateServiceQuotaTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateServiceQuotaTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateServiceQuotaTemplate
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DisassociateServiceQuotaTemplate
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DisassociateServiceQuotaTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ServiceQuotasV20190624.DisassociateServiceQuotaTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateServiceQuotaTemplate where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DisassociateServiceQuotaTemplate where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateServiceQuotaTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateServiceQuotaTemplateResponse' smart constructor.
data DisassociateServiceQuotaTemplateResponse = DisassociateServiceQuotaTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateServiceQuotaTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateServiceQuotaTemplateResponse_httpStatus' - The response's http status code.
newDisassociateServiceQuotaTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateServiceQuotaTemplateResponse
newDisassociateServiceQuotaTemplateResponse
  pHttpStatus_ =
    DisassociateServiceQuotaTemplateResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateServiceQuotaTemplateResponse_httpStatus :: Lens.Lens' DisassociateServiceQuotaTemplateResponse Prelude.Int
disassociateServiceQuotaTemplateResponse_httpStatus = Lens.lens (\DisassociateServiceQuotaTemplateResponse' {httpStatus} -> httpStatus) (\s@DisassociateServiceQuotaTemplateResponse' {} a -> s {httpStatus = a} :: DisassociateServiceQuotaTemplateResponse)

instance
  Prelude.NFData
    DisassociateServiceQuotaTemplateResponse
  where
  rnf DisassociateServiceQuotaTemplateResponse' {..} =
    Prelude.rnf httpStatus
