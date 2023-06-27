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
-- Module      : Amazonka.WorkMail.GetImpersonationRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the impersonation role details for the given WorkMail organization.
module Amazonka.WorkMail.GetImpersonationRole
  ( -- * Creating a Request
    GetImpersonationRole (..),
    newGetImpersonationRole,

    -- * Request Lenses
    getImpersonationRole_organizationId,
    getImpersonationRole_impersonationRoleId,

    -- * Destructuring the Response
    GetImpersonationRoleResponse (..),
    newGetImpersonationRoleResponse,

    -- * Response Lenses
    getImpersonationRoleResponse_dateCreated,
    getImpersonationRoleResponse_dateModified,
    getImpersonationRoleResponse_description,
    getImpersonationRoleResponse_impersonationRoleId,
    getImpersonationRoleResponse_name,
    getImpersonationRoleResponse_rules,
    getImpersonationRoleResponse_type,
    getImpersonationRoleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newGetImpersonationRole' smart constructor.
data GetImpersonationRole = GetImpersonationRole'
  { -- | The WorkMail organization from which to retrieve the impersonation role.
    organizationId :: Prelude.Text,
    -- | The impersonation role ID to retrieve.
    impersonationRoleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImpersonationRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'getImpersonationRole_organizationId' - The WorkMail organization from which to retrieve the impersonation role.
--
-- 'impersonationRoleId', 'getImpersonationRole_impersonationRoleId' - The impersonation role ID to retrieve.
newGetImpersonationRole ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'impersonationRoleId'
  Prelude.Text ->
  GetImpersonationRole
newGetImpersonationRole
  pOrganizationId_
  pImpersonationRoleId_ =
    GetImpersonationRole'
      { organizationId =
          pOrganizationId_,
        impersonationRoleId = pImpersonationRoleId_
      }

-- | The WorkMail organization from which to retrieve the impersonation role.
getImpersonationRole_organizationId :: Lens.Lens' GetImpersonationRole Prelude.Text
getImpersonationRole_organizationId = Lens.lens (\GetImpersonationRole' {organizationId} -> organizationId) (\s@GetImpersonationRole' {} a -> s {organizationId = a} :: GetImpersonationRole)

-- | The impersonation role ID to retrieve.
getImpersonationRole_impersonationRoleId :: Lens.Lens' GetImpersonationRole Prelude.Text
getImpersonationRole_impersonationRoleId = Lens.lens (\GetImpersonationRole' {impersonationRoleId} -> impersonationRoleId) (\s@GetImpersonationRole' {} a -> s {impersonationRoleId = a} :: GetImpersonationRole)

instance Core.AWSRequest GetImpersonationRole where
  type
    AWSResponse GetImpersonationRole =
      GetImpersonationRoleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImpersonationRoleResponse'
            Prelude.<$> (x Data..?> "DateCreated")
            Prelude.<*> (x Data..?> "DateModified")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "ImpersonationRoleId")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Rules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetImpersonationRole where
  hashWithSalt _salt GetImpersonationRole' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` impersonationRoleId

instance Prelude.NFData GetImpersonationRole where
  rnf GetImpersonationRole' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf impersonationRoleId

instance Data.ToHeaders GetImpersonationRole where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.GetImpersonationRole" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetImpersonationRole where
  toJSON GetImpersonationRole' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just
              ("ImpersonationRoleId" Data..= impersonationRoleId)
          ]
      )

instance Data.ToPath GetImpersonationRole where
  toPath = Prelude.const "/"

instance Data.ToQuery GetImpersonationRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetImpersonationRoleResponse' smart constructor.
data GetImpersonationRoleResponse = GetImpersonationRoleResponse'
  { -- | The date when the impersonation role was created.
    dateCreated :: Prelude.Maybe Data.POSIX,
    -- | The date when the impersonation role was last modified.
    dateModified :: Prelude.Maybe Data.POSIX,
    -- | The impersonation role description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The impersonation role ID.
    impersonationRoleId :: Prelude.Maybe Prelude.Text,
    -- | The impersonation role name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The list of rules for the given impersonation role.
    rules :: Prelude.Maybe [ImpersonationRule],
    -- | The impersonation role type.
    type' :: Prelude.Maybe ImpersonationRoleType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImpersonationRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateCreated', 'getImpersonationRoleResponse_dateCreated' - The date when the impersonation role was created.
--
-- 'dateModified', 'getImpersonationRoleResponse_dateModified' - The date when the impersonation role was last modified.
--
-- 'description', 'getImpersonationRoleResponse_description' - The impersonation role description.
--
-- 'impersonationRoleId', 'getImpersonationRoleResponse_impersonationRoleId' - The impersonation role ID.
--
-- 'name', 'getImpersonationRoleResponse_name' - The impersonation role name.
--
-- 'rules', 'getImpersonationRoleResponse_rules' - The list of rules for the given impersonation role.
--
-- 'type'', 'getImpersonationRoleResponse_type' - The impersonation role type.
--
-- 'httpStatus', 'getImpersonationRoleResponse_httpStatus' - The response's http status code.
newGetImpersonationRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetImpersonationRoleResponse
newGetImpersonationRoleResponse pHttpStatus_ =
  GetImpersonationRoleResponse'
    { dateCreated =
        Prelude.Nothing,
      dateModified = Prelude.Nothing,
      description = Prelude.Nothing,
      impersonationRoleId = Prelude.Nothing,
      name = Prelude.Nothing,
      rules = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date when the impersonation role was created.
getImpersonationRoleResponse_dateCreated :: Lens.Lens' GetImpersonationRoleResponse (Prelude.Maybe Prelude.UTCTime)
getImpersonationRoleResponse_dateCreated = Lens.lens (\GetImpersonationRoleResponse' {dateCreated} -> dateCreated) (\s@GetImpersonationRoleResponse' {} a -> s {dateCreated = a} :: GetImpersonationRoleResponse) Prelude.. Lens.mapping Data._Time

-- | The date when the impersonation role was last modified.
getImpersonationRoleResponse_dateModified :: Lens.Lens' GetImpersonationRoleResponse (Prelude.Maybe Prelude.UTCTime)
getImpersonationRoleResponse_dateModified = Lens.lens (\GetImpersonationRoleResponse' {dateModified} -> dateModified) (\s@GetImpersonationRoleResponse' {} a -> s {dateModified = a} :: GetImpersonationRoleResponse) Prelude.. Lens.mapping Data._Time

-- | The impersonation role description.
getImpersonationRoleResponse_description :: Lens.Lens' GetImpersonationRoleResponse (Prelude.Maybe Prelude.Text)
getImpersonationRoleResponse_description = Lens.lens (\GetImpersonationRoleResponse' {description} -> description) (\s@GetImpersonationRoleResponse' {} a -> s {description = a} :: GetImpersonationRoleResponse)

-- | The impersonation role ID.
getImpersonationRoleResponse_impersonationRoleId :: Lens.Lens' GetImpersonationRoleResponse (Prelude.Maybe Prelude.Text)
getImpersonationRoleResponse_impersonationRoleId = Lens.lens (\GetImpersonationRoleResponse' {impersonationRoleId} -> impersonationRoleId) (\s@GetImpersonationRoleResponse' {} a -> s {impersonationRoleId = a} :: GetImpersonationRoleResponse)

-- | The impersonation role name.
getImpersonationRoleResponse_name :: Lens.Lens' GetImpersonationRoleResponse (Prelude.Maybe Prelude.Text)
getImpersonationRoleResponse_name = Lens.lens (\GetImpersonationRoleResponse' {name} -> name) (\s@GetImpersonationRoleResponse' {} a -> s {name = a} :: GetImpersonationRoleResponse)

-- | The list of rules for the given impersonation role.
getImpersonationRoleResponse_rules :: Lens.Lens' GetImpersonationRoleResponse (Prelude.Maybe [ImpersonationRule])
getImpersonationRoleResponse_rules = Lens.lens (\GetImpersonationRoleResponse' {rules} -> rules) (\s@GetImpersonationRoleResponse' {} a -> s {rules = a} :: GetImpersonationRoleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The impersonation role type.
getImpersonationRoleResponse_type :: Lens.Lens' GetImpersonationRoleResponse (Prelude.Maybe ImpersonationRoleType)
getImpersonationRoleResponse_type = Lens.lens (\GetImpersonationRoleResponse' {type'} -> type') (\s@GetImpersonationRoleResponse' {} a -> s {type' = a} :: GetImpersonationRoleResponse)

-- | The response's http status code.
getImpersonationRoleResponse_httpStatus :: Lens.Lens' GetImpersonationRoleResponse Prelude.Int
getImpersonationRoleResponse_httpStatus = Lens.lens (\GetImpersonationRoleResponse' {httpStatus} -> httpStatus) (\s@GetImpersonationRoleResponse' {} a -> s {httpStatus = a} :: GetImpersonationRoleResponse)

instance Prelude.NFData GetImpersonationRoleResponse where
  rnf GetImpersonationRoleResponse' {..} =
    Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateModified
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf impersonationRoleId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf httpStatus
