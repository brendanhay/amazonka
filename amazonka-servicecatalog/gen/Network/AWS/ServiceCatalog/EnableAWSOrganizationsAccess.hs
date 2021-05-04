{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ServiceCatalog.EnableAWSOrganizationsAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable portfolio sharing feature through AWS Organizations. This API
-- will allow Service Catalog to receive updates on your organization in
-- order to sync your shares with the current structure. This API can only
-- be called by the management account in the organization.
--
-- By calling this API Service Catalog will make a call to
-- organizations:EnableAWSServiceAccess on your behalf so that your shares
-- can be in sync with any changes in your AWS Organizations structure.
--
-- Note that a delegated administrator is not authorized to invoke
-- @EnableAWSOrganizationsAccess@.
module Network.AWS.ServiceCatalog.EnableAWSOrganizationsAccess
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newEnableAWSOrganizationsAccess' smart constructor.
data EnableAWSOrganizationsAccess = EnableAWSOrganizationsAccess'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableAWSOrganizationsAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableAWSOrganizationsAccess ::
  EnableAWSOrganizationsAccess
newEnableAWSOrganizationsAccess =
  EnableAWSOrganizationsAccess'

instance
  Prelude.AWSRequest
    EnableAWSOrganizationsAccess
  where
  type
    Rs EnableAWSOrganizationsAccess =
      EnableAWSOrganizationsAccessResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableAWSOrganizationsAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableAWSOrganizationsAccess

instance Prelude.NFData EnableAWSOrganizationsAccess

instance
  Prelude.ToHeaders
    EnableAWSOrganizationsAccess
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.EnableAWSOrganizationsAccess" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON EnableAWSOrganizationsAccess where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath EnableAWSOrganizationsAccess where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableAWSOrganizationsAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableAWSOrganizationsAccessResponse' smart constructor.
data EnableAWSOrganizationsAccessResponse = EnableAWSOrganizationsAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
