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
-- Module      : Amazonka.AuditManager.GetServicesInScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all of the Amazon Web Services that you can choose to
-- include in your assessment. When you
-- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_CreateAssessment.html create an assessment>,
-- specify which of these services you want to include to narrow the
-- assessment\'s
-- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_Scope.html scope>.
module Amazonka.AuditManager.GetServicesInScope
  ( -- * Creating a Request
    GetServicesInScope (..),
    newGetServicesInScope,

    -- * Destructuring the Response
    GetServicesInScopeResponse (..),
    newGetServicesInScopeResponse,

    -- * Response Lenses
    getServicesInScopeResponse_serviceMetadata,
    getServicesInScopeResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServicesInScope' smart constructor.
data GetServicesInScope = GetServicesInScope'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServicesInScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetServicesInScope ::
  GetServicesInScope
newGetServicesInScope = GetServicesInScope'

instance Core.AWSRequest GetServicesInScope where
  type
    AWSResponse GetServicesInScope =
      GetServicesInScopeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServicesInScopeResponse'
            Prelude.<$> ( x
                            Data..?> "serviceMetadata"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServicesInScope where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetServicesInScope where
  rnf _ = ()

instance Data.ToHeaders GetServicesInScope where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetServicesInScope where
  toPath = Prelude.const "/services"

instance Data.ToQuery GetServicesInScope where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServicesInScopeResponse' smart constructor.
data GetServicesInScopeResponse = GetServicesInScopeResponse'
  { -- | The metadata that\'s associated with the Amazon Web Service.
    serviceMetadata :: Prelude.Maybe [ServiceMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServicesInScopeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceMetadata', 'getServicesInScopeResponse_serviceMetadata' - The metadata that\'s associated with the Amazon Web Service.
--
-- 'httpStatus', 'getServicesInScopeResponse_httpStatus' - The response's http status code.
newGetServicesInScopeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServicesInScopeResponse
newGetServicesInScopeResponse pHttpStatus_ =
  GetServicesInScopeResponse'
    { serviceMetadata =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata that\'s associated with the Amazon Web Service.
getServicesInScopeResponse_serviceMetadata :: Lens.Lens' GetServicesInScopeResponse (Prelude.Maybe [ServiceMetadata])
getServicesInScopeResponse_serviceMetadata = Lens.lens (\GetServicesInScopeResponse' {serviceMetadata} -> serviceMetadata) (\s@GetServicesInScopeResponse' {} a -> s {serviceMetadata = a} :: GetServicesInScopeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getServicesInScopeResponse_httpStatus :: Lens.Lens' GetServicesInScopeResponse Prelude.Int
getServicesInScopeResponse_httpStatus = Lens.lens (\GetServicesInScopeResponse' {httpStatus} -> httpStatus) (\s@GetServicesInScopeResponse' {} a -> s {httpStatus = a} :: GetServicesInScopeResponse)

instance Prelude.NFData GetServicesInScopeResponse where
  rnf GetServicesInScopeResponse' {..} =
    Prelude.rnf serviceMetadata
      `Prelude.seq` Prelude.rnf httpStatus
