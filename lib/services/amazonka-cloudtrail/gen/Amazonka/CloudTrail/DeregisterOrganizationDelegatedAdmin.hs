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
-- Module      : Amazonka.CloudTrail.DeregisterOrganizationDelegatedAdmin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes CloudTrail delegated administrator permissions from a member
-- account in an organization.
module Amazonka.CloudTrail.DeregisterOrganizationDelegatedAdmin
  ( -- * Creating a Request
    DeregisterOrganizationDelegatedAdmin (..),
    newDeregisterOrganizationDelegatedAdmin,

    -- * Request Lenses
    deregisterOrganizationDelegatedAdmin_delegatedAdminAccountId,

    -- * Destructuring the Response
    DeregisterOrganizationDelegatedAdminResponse (..),
    newDeregisterOrganizationDelegatedAdminResponse,

    -- * Response Lenses
    deregisterOrganizationDelegatedAdminResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Removes CloudTrail delegated administrator permissions from a specified
-- member account in an organization that is currently designated as a
-- delegated administrator.
--
-- /See:/ 'newDeregisterOrganizationDelegatedAdmin' smart constructor.
data DeregisterOrganizationDelegatedAdmin = DeregisterOrganizationDelegatedAdmin'
  { -- | A delegated administrator account ID. This is a member account in an
    -- organization that is currently designated as a delegated administrator.
    delegatedAdminAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterOrganizationDelegatedAdmin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delegatedAdminAccountId', 'deregisterOrganizationDelegatedAdmin_delegatedAdminAccountId' - A delegated administrator account ID. This is a member account in an
-- organization that is currently designated as a delegated administrator.
newDeregisterOrganizationDelegatedAdmin ::
  -- | 'delegatedAdminAccountId'
  Prelude.Text ->
  DeregisterOrganizationDelegatedAdmin
newDeregisterOrganizationDelegatedAdmin
  pDelegatedAdminAccountId_ =
    DeregisterOrganizationDelegatedAdmin'
      { delegatedAdminAccountId =
          pDelegatedAdminAccountId_
      }

-- | A delegated administrator account ID. This is a member account in an
-- organization that is currently designated as a delegated administrator.
deregisterOrganizationDelegatedAdmin_delegatedAdminAccountId :: Lens.Lens' DeregisterOrganizationDelegatedAdmin Prelude.Text
deregisterOrganizationDelegatedAdmin_delegatedAdminAccountId = Lens.lens (\DeregisterOrganizationDelegatedAdmin' {delegatedAdminAccountId} -> delegatedAdminAccountId) (\s@DeregisterOrganizationDelegatedAdmin' {} a -> s {delegatedAdminAccountId = a} :: DeregisterOrganizationDelegatedAdmin)

instance
  Core.AWSRequest
    DeregisterOrganizationDelegatedAdmin
  where
  type
    AWSResponse DeregisterOrganizationDelegatedAdmin =
      DeregisterOrganizationDelegatedAdminResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterOrganizationDelegatedAdminResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterOrganizationDelegatedAdmin
  where
  hashWithSalt
    _salt
    DeregisterOrganizationDelegatedAdmin' {..} =
      _salt
        `Prelude.hashWithSalt` delegatedAdminAccountId

instance
  Prelude.NFData
    DeregisterOrganizationDelegatedAdmin
  where
  rnf DeregisterOrganizationDelegatedAdmin' {..} =
    Prelude.rnf delegatedAdminAccountId

instance
  Data.ToHeaders
    DeregisterOrganizationDelegatedAdmin
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DeregisterOrganizationDelegatedAdmin" ::
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
    DeregisterOrganizationDelegatedAdmin
  where
  toJSON DeregisterOrganizationDelegatedAdmin' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DelegatedAdminAccountId"
                  Data..= delegatedAdminAccountId
              )
          ]
      )

instance
  Data.ToPath
    DeregisterOrganizationDelegatedAdmin
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeregisterOrganizationDelegatedAdmin
  where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the following response if successful. Otherwise, returns an
-- error.
--
-- /See:/ 'newDeregisterOrganizationDelegatedAdminResponse' smart constructor.
data DeregisterOrganizationDelegatedAdminResponse = DeregisterOrganizationDelegatedAdminResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterOrganizationDelegatedAdminResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterOrganizationDelegatedAdminResponse_httpStatus' - The response's http status code.
newDeregisterOrganizationDelegatedAdminResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterOrganizationDelegatedAdminResponse
newDeregisterOrganizationDelegatedAdminResponse
  pHttpStatus_ =
    DeregisterOrganizationDelegatedAdminResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deregisterOrganizationDelegatedAdminResponse_httpStatus :: Lens.Lens' DeregisterOrganizationDelegatedAdminResponse Prelude.Int
deregisterOrganizationDelegatedAdminResponse_httpStatus = Lens.lens (\DeregisterOrganizationDelegatedAdminResponse' {httpStatus} -> httpStatus) (\s@DeregisterOrganizationDelegatedAdminResponse' {} a -> s {httpStatus = a} :: DeregisterOrganizationDelegatedAdminResponse)

instance
  Prelude.NFData
    DeregisterOrganizationDelegatedAdminResponse
  where
  rnf DeregisterOrganizationDelegatedAdminResponse' {..} =
    Prelude.rnf httpStatus
