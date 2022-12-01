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
-- Module      : Amazonka.ServiceCatalog.DisableAWSOrganizationsAccess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disable portfolio sharing through the Organizations service. This
-- command will not delete your current shares, but prevents you from
-- creating new shares throughout your organization. Current shares are not
-- kept in sync with your organization structure if the structure changes
-- after calling this API. Only the management account in the organization
-- can call this API.
--
-- You cannot call this API if there are active delegated administrators in
-- the organization.
--
-- Note that a delegated administrator is not authorized to invoke
-- @DisableAWSOrganizationsAccess@.
--
-- If you share an Service Catalog portfolio in an organization within
-- Organizations, and then disable Organizations access for Service
-- Catalog, the portfolio access permissions will not sync with the latest
-- changes to the organization structure. Specifically, accounts that you
-- removed from the organization after disabling Service Catalog access
-- will retain access to the previously shared portfolio.
module Amazonka.ServiceCatalog.DisableAWSOrganizationsAccess
  ( -- * Creating a Request
    DisableAWSOrganizationsAccess (..),
    newDisableAWSOrganizationsAccess,

    -- * Destructuring the Response
    DisableAWSOrganizationsAccessResponse (..),
    newDisableAWSOrganizationsAccessResponse,

    -- * Response Lenses
    disableAWSOrganizationsAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDisableAWSOrganizationsAccess' smart constructor.
data DisableAWSOrganizationsAccess = DisableAWSOrganizationsAccess'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableAWSOrganizationsAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableAWSOrganizationsAccess ::
  DisableAWSOrganizationsAccess
newDisableAWSOrganizationsAccess =
  DisableAWSOrganizationsAccess'

instance
  Core.AWSRequest
    DisableAWSOrganizationsAccess
  where
  type
    AWSResponse DisableAWSOrganizationsAccess =
      DisableAWSOrganizationsAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableAWSOrganizationsAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisableAWSOrganizationsAccess
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DisableAWSOrganizationsAccess where
  rnf _ = ()

instance Core.ToHeaders DisableAWSOrganizationsAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DisableAWSOrganizationsAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisableAWSOrganizationsAccess where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DisableAWSOrganizationsAccess where
  toPath = Prelude.const "/"

instance Core.ToQuery DisableAWSOrganizationsAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableAWSOrganizationsAccessResponse' smart constructor.
data DisableAWSOrganizationsAccessResponse = DisableAWSOrganizationsAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableAWSOrganizationsAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableAWSOrganizationsAccessResponse_httpStatus' - The response's http status code.
newDisableAWSOrganizationsAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableAWSOrganizationsAccessResponse
newDisableAWSOrganizationsAccessResponse pHttpStatus_ =
  DisableAWSOrganizationsAccessResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disableAWSOrganizationsAccessResponse_httpStatus :: Lens.Lens' DisableAWSOrganizationsAccessResponse Prelude.Int
disableAWSOrganizationsAccessResponse_httpStatus = Lens.lens (\DisableAWSOrganizationsAccessResponse' {httpStatus} -> httpStatus) (\s@DisableAWSOrganizationsAccessResponse' {} a -> s {httpStatus = a} :: DisableAWSOrganizationsAccessResponse)

instance
  Prelude.NFData
    DisableAWSOrganizationsAccessResponse
  where
  rnf DisableAWSOrganizationsAccessResponse' {..} =
    Prelude.rnf httpStatus
