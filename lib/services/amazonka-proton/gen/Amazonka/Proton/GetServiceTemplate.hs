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
-- Module      : Amazonka.Proton.GetServiceTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detailed data for a service template.
module Amazonka.Proton.GetServiceTemplate
  ( -- * Creating a Request
    GetServiceTemplate (..),
    newGetServiceTemplate,

    -- * Request Lenses
    getServiceTemplate_name,

    -- * Destructuring the Response
    GetServiceTemplateResponse (..),
    newGetServiceTemplateResponse,

    -- * Response Lenses
    getServiceTemplateResponse_httpStatus,
    getServiceTemplateResponse_serviceTemplate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServiceTemplate' smart constructor.
data GetServiceTemplate = GetServiceTemplate'
  { -- | The name of the service template that you want to get detailed data for.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getServiceTemplate_name' - The name of the service template that you want to get detailed data for.
newGetServiceTemplate ::
  -- | 'name'
  Prelude.Text ->
  GetServiceTemplate
newGetServiceTemplate pName_ =
  GetServiceTemplate' {name = pName_}

-- | The name of the service template that you want to get detailed data for.
getServiceTemplate_name :: Lens.Lens' GetServiceTemplate Prelude.Text
getServiceTemplate_name = Lens.lens (\GetServiceTemplate' {name} -> name) (\s@GetServiceTemplate' {} a -> s {name = a} :: GetServiceTemplate)

instance Core.AWSRequest GetServiceTemplate where
  type
    AWSResponse GetServiceTemplate =
      GetServiceTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "serviceTemplate")
      )

instance Prelude.Hashable GetServiceTemplate where
  hashWithSalt _salt GetServiceTemplate' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetServiceTemplate where
  rnf GetServiceTemplate' {..} = Prelude.rnf name

instance Core.ToHeaders GetServiceTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.GetServiceTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetServiceTemplate where
  toJSON GetServiceTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Core..= name)]
      )

instance Core.ToPath GetServiceTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery GetServiceTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceTemplateResponse' smart constructor.
data GetServiceTemplateResponse = GetServiceTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The detailed data of the requested service template.
    serviceTemplate :: ServiceTemplate
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getServiceTemplateResponse_httpStatus' - The response's http status code.
--
-- 'serviceTemplate', 'getServiceTemplateResponse_serviceTemplate' - The detailed data of the requested service template.
newGetServiceTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serviceTemplate'
  ServiceTemplate ->
  GetServiceTemplateResponse
newGetServiceTemplateResponse
  pHttpStatus_
  pServiceTemplate_ =
    GetServiceTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        serviceTemplate = pServiceTemplate_
      }

-- | The response's http status code.
getServiceTemplateResponse_httpStatus :: Lens.Lens' GetServiceTemplateResponse Prelude.Int
getServiceTemplateResponse_httpStatus = Lens.lens (\GetServiceTemplateResponse' {httpStatus} -> httpStatus) (\s@GetServiceTemplateResponse' {} a -> s {httpStatus = a} :: GetServiceTemplateResponse)

-- | The detailed data of the requested service template.
getServiceTemplateResponse_serviceTemplate :: Lens.Lens' GetServiceTemplateResponse ServiceTemplate
getServiceTemplateResponse_serviceTemplate = Lens.lens (\GetServiceTemplateResponse' {serviceTemplate} -> serviceTemplate) (\s@GetServiceTemplateResponse' {} a -> s {serviceTemplate = a} :: GetServiceTemplateResponse)

instance Prelude.NFData GetServiceTemplateResponse where
  rnf GetServiceTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serviceTemplate
