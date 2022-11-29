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
-- Module      : Amazonka.ServiceQuotas.AssociateServiceQuotaTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates your quota request template with your organization. When a
-- new account is created in your organization, the quota increase requests
-- in the template are automatically applied to the account. You can add a
-- quota increase request for any adjustable quota to your template.
module Amazonka.ServiceQuotas.AssociateServiceQuotaTemplate
  ( -- * Creating a Request
    AssociateServiceQuotaTemplate (..),
    newAssociateServiceQuotaTemplate,

    -- * Destructuring the Response
    AssociateServiceQuotaTemplateResponse (..),
    newAssociateServiceQuotaTemplateResponse,

    -- * Response Lenses
    associateServiceQuotaTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceQuotas.Types

-- | /See:/ 'newAssociateServiceQuotaTemplate' smart constructor.
data AssociateServiceQuotaTemplate = AssociateServiceQuotaTemplate'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateServiceQuotaTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateServiceQuotaTemplate ::
  AssociateServiceQuotaTemplate
newAssociateServiceQuotaTemplate =
  AssociateServiceQuotaTemplate'

instance
  Core.AWSRequest
    AssociateServiceQuotaTemplate
  where
  type
    AWSResponse AssociateServiceQuotaTemplate =
      AssociateServiceQuotaTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateServiceQuotaTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateServiceQuotaTemplate
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData AssociateServiceQuotaTemplate where
  rnf _ = ()

instance Core.ToHeaders AssociateServiceQuotaTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ServiceQuotasV20190624.AssociateServiceQuotaTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateServiceQuotaTemplate where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath AssociateServiceQuotaTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateServiceQuotaTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateServiceQuotaTemplateResponse' smart constructor.
data AssociateServiceQuotaTemplateResponse = AssociateServiceQuotaTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateServiceQuotaTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateServiceQuotaTemplateResponse_httpStatus' - The response's http status code.
newAssociateServiceQuotaTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateServiceQuotaTemplateResponse
newAssociateServiceQuotaTemplateResponse pHttpStatus_ =
  AssociateServiceQuotaTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateServiceQuotaTemplateResponse_httpStatus :: Lens.Lens' AssociateServiceQuotaTemplateResponse Prelude.Int
associateServiceQuotaTemplateResponse_httpStatus = Lens.lens (\AssociateServiceQuotaTemplateResponse' {httpStatus} -> httpStatus) (\s@AssociateServiceQuotaTemplateResponse' {} a -> s {httpStatus = a} :: AssociateServiceQuotaTemplateResponse)

instance
  Prelude.NFData
    AssociateServiceQuotaTemplateResponse
  where
  rnf AssociateServiceQuotaTemplateResponse' {..} =
    Prelude.rnf httpStatus
