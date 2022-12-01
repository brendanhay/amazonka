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
-- Module      : Amazonka.ServiceCatalog.EnableAWSOrganizationsAccess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable portfolio sharing feature through Organizations. This API will
-- allow Service Catalog to receive updates on your organization in order
-- to sync your shares with the current structure. This API can only be
-- called by the management account in the organization.
--
-- When you call this API, Service Catalog calls
-- @organizations:EnableAWSServiceAccess@ on your behalf so that your
-- shares stay in sync with any changes in your Organizations structure.
--
-- Note that a delegated administrator is not authorized to invoke
-- @EnableAWSOrganizationsAccess@.
--
-- If you have previously disabled Organizations access for Service
-- Catalog, and then enable access again, the portfolio access permissions
-- might not sync with the latest changes to the organization structure.
-- Specifically, accounts that you removed from the organization after
-- disabling Service Catalog access, and before you enabled access again,
-- can retain access to the previously shared portfolio. As a result, an
-- account that has been removed from the organization might still be able
-- to create or manage Amazon Web Services resources when it is no longer
-- authorized to do so. Amazon Web Services is working to resolve this
-- issue.
module Amazonka.ServiceCatalog.EnableAWSOrganizationsAccess
  ( -- * Creating a Request
    EnableAWSOrganizationsAccess (..),
    newEnableAWSOrganizationsAccess,

    -- * Destructuring the Response
    EnableAWSOrganizationsAccessResponse (..),
    newEnableAWSOrganizationsAccessResponse,

    -- * Response Lenses
    enableAWSOrganizationsAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newEnableAWSOrganizationsAccess' smart constructor.
data EnableAWSOrganizationsAccess = EnableAWSOrganizationsAccess'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableAWSOrganizationsAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableAWSOrganizationsAccess ::
  EnableAWSOrganizationsAccess
newEnableAWSOrganizationsAccess =
  EnableAWSOrganizationsAccess'

instance Core.AWSRequest EnableAWSOrganizationsAccess where
  type
    AWSResponse EnableAWSOrganizationsAccess =
      EnableAWSOrganizationsAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableAWSOrganizationsAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableAWSOrganizationsAccess
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData EnableAWSOrganizationsAccess where
  rnf _ = ()

instance Core.ToHeaders EnableAWSOrganizationsAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.EnableAWSOrganizationsAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON EnableAWSOrganizationsAccess where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath EnableAWSOrganizationsAccess where
  toPath = Prelude.const "/"

instance Core.ToQuery EnableAWSOrganizationsAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableAWSOrganizationsAccessResponse' smart constructor.
data EnableAWSOrganizationsAccessResponse = EnableAWSOrganizationsAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableAWSOrganizationsAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableAWSOrganizationsAccessResponse_httpStatus' - The response's http status code.
newEnableAWSOrganizationsAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableAWSOrganizationsAccessResponse
newEnableAWSOrganizationsAccessResponse pHttpStatus_ =
  EnableAWSOrganizationsAccessResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
enableAWSOrganizationsAccessResponse_httpStatus :: Lens.Lens' EnableAWSOrganizationsAccessResponse Prelude.Int
enableAWSOrganizationsAccessResponse_httpStatus = Lens.lens (\EnableAWSOrganizationsAccessResponse' {httpStatus} -> httpStatus) (\s@EnableAWSOrganizationsAccessResponse' {} a -> s {httpStatus = a} :: EnableAWSOrganizationsAccessResponse)

instance
  Prelude.NFData
    EnableAWSOrganizationsAccessResponse
  where
  rnf EnableAWSOrganizationsAccessResponse' {..} =
    Prelude.rnf httpStatus
