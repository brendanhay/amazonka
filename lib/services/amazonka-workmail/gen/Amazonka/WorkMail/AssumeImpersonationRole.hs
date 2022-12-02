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
-- Module      : Amazonka.WorkMail.AssumeImpersonationRole
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assumes an impersonation role for the given WorkMail organization. This
-- method returns an authentication token you can use to make impersonated
-- calls.
module Amazonka.WorkMail.AssumeImpersonationRole
  ( -- * Creating a Request
    AssumeImpersonationRole (..),
    newAssumeImpersonationRole,

    -- * Request Lenses
    assumeImpersonationRole_organizationId,
    assumeImpersonationRole_impersonationRoleId,

    -- * Destructuring the Response
    AssumeImpersonationRoleResponse (..),
    newAssumeImpersonationRoleResponse,

    -- * Response Lenses
    assumeImpersonationRoleResponse_expiresIn,
    assumeImpersonationRoleResponse_token,
    assumeImpersonationRoleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newAssumeImpersonationRole' smart constructor.
data AssumeImpersonationRole = AssumeImpersonationRole'
  { -- | The WorkMail organization under which the impersonation role will be
    -- assumed.
    organizationId :: Prelude.Text,
    -- | The impersonation role ID to assume.
    impersonationRoleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssumeImpersonationRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'assumeImpersonationRole_organizationId' - The WorkMail organization under which the impersonation role will be
-- assumed.
--
-- 'impersonationRoleId', 'assumeImpersonationRole_impersonationRoleId' - The impersonation role ID to assume.
newAssumeImpersonationRole ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'impersonationRoleId'
  Prelude.Text ->
  AssumeImpersonationRole
newAssumeImpersonationRole
  pOrganizationId_
  pImpersonationRoleId_ =
    AssumeImpersonationRole'
      { organizationId =
          pOrganizationId_,
        impersonationRoleId = pImpersonationRoleId_
      }

-- | The WorkMail organization under which the impersonation role will be
-- assumed.
assumeImpersonationRole_organizationId :: Lens.Lens' AssumeImpersonationRole Prelude.Text
assumeImpersonationRole_organizationId = Lens.lens (\AssumeImpersonationRole' {organizationId} -> organizationId) (\s@AssumeImpersonationRole' {} a -> s {organizationId = a} :: AssumeImpersonationRole)

-- | The impersonation role ID to assume.
assumeImpersonationRole_impersonationRoleId :: Lens.Lens' AssumeImpersonationRole Prelude.Text
assumeImpersonationRole_impersonationRoleId = Lens.lens (\AssumeImpersonationRole' {impersonationRoleId} -> impersonationRoleId) (\s@AssumeImpersonationRole' {} a -> s {impersonationRoleId = a} :: AssumeImpersonationRole)

instance Core.AWSRequest AssumeImpersonationRole where
  type
    AWSResponse AssumeImpersonationRole =
      AssumeImpersonationRoleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssumeImpersonationRoleResponse'
            Prelude.<$> (x Data..?> "ExpiresIn")
            Prelude.<*> (x Data..?> "Token")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssumeImpersonationRole where
  hashWithSalt _salt AssumeImpersonationRole' {..} =
    _salt `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` impersonationRoleId

instance Prelude.NFData AssumeImpersonationRole where
  rnf AssumeImpersonationRole' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf impersonationRoleId

instance Data.ToHeaders AssumeImpersonationRole where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.AssumeImpersonationRole" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssumeImpersonationRole where
  toJSON AssumeImpersonationRole' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just
              ("ImpersonationRoleId" Data..= impersonationRoleId)
          ]
      )

instance Data.ToPath AssumeImpersonationRole where
  toPath = Prelude.const "/"

instance Data.ToQuery AssumeImpersonationRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssumeImpersonationRoleResponse' smart constructor.
data AssumeImpersonationRoleResponse = AssumeImpersonationRoleResponse'
  { -- | The authentication token\'s validity, in seconds.
    expiresIn :: Prelude.Maybe Prelude.Integer,
    -- | The authentication token for the impersonation role.
    token :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssumeImpersonationRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiresIn', 'assumeImpersonationRoleResponse_expiresIn' - The authentication token\'s validity, in seconds.
--
-- 'token', 'assumeImpersonationRoleResponse_token' - The authentication token for the impersonation role.
--
-- 'httpStatus', 'assumeImpersonationRoleResponse_httpStatus' - The response's http status code.
newAssumeImpersonationRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssumeImpersonationRoleResponse
newAssumeImpersonationRoleResponse pHttpStatus_ =
  AssumeImpersonationRoleResponse'
    { expiresIn =
        Prelude.Nothing,
      token = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authentication token\'s validity, in seconds.
assumeImpersonationRoleResponse_expiresIn :: Lens.Lens' AssumeImpersonationRoleResponse (Prelude.Maybe Prelude.Integer)
assumeImpersonationRoleResponse_expiresIn = Lens.lens (\AssumeImpersonationRoleResponse' {expiresIn} -> expiresIn) (\s@AssumeImpersonationRoleResponse' {} a -> s {expiresIn = a} :: AssumeImpersonationRoleResponse)

-- | The authentication token for the impersonation role.
assumeImpersonationRoleResponse_token :: Lens.Lens' AssumeImpersonationRoleResponse (Prelude.Maybe Prelude.Text)
assumeImpersonationRoleResponse_token = Lens.lens (\AssumeImpersonationRoleResponse' {token} -> token) (\s@AssumeImpersonationRoleResponse' {} a -> s {token = a} :: AssumeImpersonationRoleResponse)

-- | The response's http status code.
assumeImpersonationRoleResponse_httpStatus :: Lens.Lens' AssumeImpersonationRoleResponse Prelude.Int
assumeImpersonationRoleResponse_httpStatus = Lens.lens (\AssumeImpersonationRoleResponse' {httpStatus} -> httpStatus) (\s@AssumeImpersonationRoleResponse' {} a -> s {httpStatus = a} :: AssumeImpersonationRoleResponse)

instance
  Prelude.NFData
    AssumeImpersonationRoleResponse
  where
  rnf AssumeImpersonationRoleResponse' {..} =
    Prelude.rnf expiresIn
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf httpStatus
