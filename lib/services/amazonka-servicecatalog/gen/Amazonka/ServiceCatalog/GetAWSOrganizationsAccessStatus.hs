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
-- Module      : Amazonka.ServiceCatalog.GetAWSOrganizationsAccessStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the Access Status for Organizations portfolio share feature. This
-- API can only be called by the management account in the organization or
-- by a delegated admin.
module Amazonka.ServiceCatalog.GetAWSOrganizationsAccessStatus
  ( -- * Creating a Request
    GetAWSOrganizationsAccessStatus (..),
    newGetAWSOrganizationsAccessStatus,

    -- * Destructuring the Response
    GetAWSOrganizationsAccessStatusResponse (..),
    newGetAWSOrganizationsAccessStatusResponse,

    -- * Response Lenses
    getAWSOrganizationsAccessStatusResponse_accessStatus,
    getAWSOrganizationsAccessStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newGetAWSOrganizationsAccessStatus' smart constructor.
data GetAWSOrganizationsAccessStatus = GetAWSOrganizationsAccessStatus'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAWSOrganizationsAccessStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAWSOrganizationsAccessStatus ::
  GetAWSOrganizationsAccessStatus
newGetAWSOrganizationsAccessStatus =
  GetAWSOrganizationsAccessStatus'

instance
  Core.AWSRequest
    GetAWSOrganizationsAccessStatus
  where
  type
    AWSResponse GetAWSOrganizationsAccessStatus =
      GetAWSOrganizationsAccessStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAWSOrganizationsAccessStatusResponse'
            Prelude.<$> (x Core..?> "AccessStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAWSOrganizationsAccessStatus
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetAWSOrganizationsAccessStatus
  where
  rnf _ = ()

instance
  Core.ToHeaders
    GetAWSOrganizationsAccessStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.GetAWSOrganizationsAccessStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetAWSOrganizationsAccessStatus where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath GetAWSOrganizationsAccessStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery GetAWSOrganizationsAccessStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAWSOrganizationsAccessStatusResponse' smart constructor.
data GetAWSOrganizationsAccessStatusResponse = GetAWSOrganizationsAccessStatusResponse'
  { -- | The status of the portfolio share feature.
    accessStatus :: Prelude.Maybe AccessStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAWSOrganizationsAccessStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessStatus', 'getAWSOrganizationsAccessStatusResponse_accessStatus' - The status of the portfolio share feature.
--
-- 'httpStatus', 'getAWSOrganizationsAccessStatusResponse_httpStatus' - The response's http status code.
newGetAWSOrganizationsAccessStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAWSOrganizationsAccessStatusResponse
newGetAWSOrganizationsAccessStatusResponse
  pHttpStatus_ =
    GetAWSOrganizationsAccessStatusResponse'
      { accessStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the portfolio share feature.
getAWSOrganizationsAccessStatusResponse_accessStatus :: Lens.Lens' GetAWSOrganizationsAccessStatusResponse (Prelude.Maybe AccessStatus)
getAWSOrganizationsAccessStatusResponse_accessStatus = Lens.lens (\GetAWSOrganizationsAccessStatusResponse' {accessStatus} -> accessStatus) (\s@GetAWSOrganizationsAccessStatusResponse' {} a -> s {accessStatus = a} :: GetAWSOrganizationsAccessStatusResponse)

-- | The response's http status code.
getAWSOrganizationsAccessStatusResponse_httpStatus :: Lens.Lens' GetAWSOrganizationsAccessStatusResponse Prelude.Int
getAWSOrganizationsAccessStatusResponse_httpStatus = Lens.lens (\GetAWSOrganizationsAccessStatusResponse' {httpStatus} -> httpStatus) (\s@GetAWSOrganizationsAccessStatusResponse' {} a -> s {httpStatus = a} :: GetAWSOrganizationsAccessStatusResponse)

instance
  Prelude.NFData
    GetAWSOrganizationsAccessStatusResponse
  where
  rnf GetAWSOrganizationsAccessStatusResponse' {..} =
    Prelude.rnf accessStatus
      `Prelude.seq` Prelude.rnf httpStatus
