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
-- Module      : Amazonka.CloudTrail.RegisterOrganizationDelegatedAdmin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an organization’s member account as the CloudTrail delegated
-- administrator.
module Amazonka.CloudTrail.RegisterOrganizationDelegatedAdmin
  ( -- * Creating a Request
    RegisterOrganizationDelegatedAdmin (..),
    newRegisterOrganizationDelegatedAdmin,

    -- * Request Lenses
    registerOrganizationDelegatedAdmin_memberAccountId,

    -- * Destructuring the Response
    RegisterOrganizationDelegatedAdminResponse (..),
    newRegisterOrganizationDelegatedAdminResponse,

    -- * Response Lenses
    registerOrganizationDelegatedAdminResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Specifies an organization member account ID as a CloudTrail delegated
-- administrator.
--
-- /See:/ 'newRegisterOrganizationDelegatedAdmin' smart constructor.
data RegisterOrganizationDelegatedAdmin = RegisterOrganizationDelegatedAdmin'
  { -- | An organization member account ID that you want to designate as a
    -- delegated administrator.
    memberAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterOrganizationDelegatedAdmin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberAccountId', 'registerOrganizationDelegatedAdmin_memberAccountId' - An organization member account ID that you want to designate as a
-- delegated administrator.
newRegisterOrganizationDelegatedAdmin ::
  -- | 'memberAccountId'
  Prelude.Text ->
  RegisterOrganizationDelegatedAdmin
newRegisterOrganizationDelegatedAdmin
  pMemberAccountId_ =
    RegisterOrganizationDelegatedAdmin'
      { memberAccountId =
          pMemberAccountId_
      }

-- | An organization member account ID that you want to designate as a
-- delegated administrator.
registerOrganizationDelegatedAdmin_memberAccountId :: Lens.Lens' RegisterOrganizationDelegatedAdmin Prelude.Text
registerOrganizationDelegatedAdmin_memberAccountId = Lens.lens (\RegisterOrganizationDelegatedAdmin' {memberAccountId} -> memberAccountId) (\s@RegisterOrganizationDelegatedAdmin' {} a -> s {memberAccountId = a} :: RegisterOrganizationDelegatedAdmin)

instance
  Core.AWSRequest
    RegisterOrganizationDelegatedAdmin
  where
  type
    AWSResponse RegisterOrganizationDelegatedAdmin =
      RegisterOrganizationDelegatedAdminResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterOrganizationDelegatedAdminResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterOrganizationDelegatedAdmin
  where
  hashWithSalt
    _salt
    RegisterOrganizationDelegatedAdmin' {..} =
      _salt `Prelude.hashWithSalt` memberAccountId

instance
  Prelude.NFData
    RegisterOrganizationDelegatedAdmin
  where
  rnf RegisterOrganizationDelegatedAdmin' {..} =
    Prelude.rnf memberAccountId

instance
  Data.ToHeaders
    RegisterOrganizationDelegatedAdmin
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.RegisterOrganizationDelegatedAdmin" ::
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
    RegisterOrganizationDelegatedAdmin
  where
  toJSON RegisterOrganizationDelegatedAdmin' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MemberAccountId" Data..= memberAccountId)
          ]
      )

instance
  Data.ToPath
    RegisterOrganizationDelegatedAdmin
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    RegisterOrganizationDelegatedAdmin
  where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the following response if successful. Otherwise, returns an
-- error.
--
-- /See:/ 'newRegisterOrganizationDelegatedAdminResponse' smart constructor.
data RegisterOrganizationDelegatedAdminResponse = RegisterOrganizationDelegatedAdminResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterOrganizationDelegatedAdminResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerOrganizationDelegatedAdminResponse_httpStatus' - The response's http status code.
newRegisterOrganizationDelegatedAdminResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterOrganizationDelegatedAdminResponse
newRegisterOrganizationDelegatedAdminResponse
  pHttpStatus_ =
    RegisterOrganizationDelegatedAdminResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
registerOrganizationDelegatedAdminResponse_httpStatus :: Lens.Lens' RegisterOrganizationDelegatedAdminResponse Prelude.Int
registerOrganizationDelegatedAdminResponse_httpStatus = Lens.lens (\RegisterOrganizationDelegatedAdminResponse' {httpStatus} -> httpStatus) (\s@RegisterOrganizationDelegatedAdminResponse' {} a -> s {httpStatus = a} :: RegisterOrganizationDelegatedAdminResponse)

instance
  Prelude.NFData
    RegisterOrganizationDelegatedAdminResponse
  where
  rnf RegisterOrganizationDelegatedAdminResponse' {..} =
    Prelude.rnf httpStatus
