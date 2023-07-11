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
-- Module      : Amazonka.WorkMail.UpdateImpersonationRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an impersonation role for the given WorkMail organization.
module Amazonka.WorkMail.UpdateImpersonationRole
  ( -- * Creating a Request
    UpdateImpersonationRole (..),
    newUpdateImpersonationRole,

    -- * Request Lenses
    updateImpersonationRole_description,
    updateImpersonationRole_organizationId,
    updateImpersonationRole_impersonationRoleId,
    updateImpersonationRole_name,
    updateImpersonationRole_type,
    updateImpersonationRole_rules,

    -- * Destructuring the Response
    UpdateImpersonationRoleResponse (..),
    newUpdateImpersonationRoleResponse,

    -- * Response Lenses
    updateImpersonationRoleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newUpdateImpersonationRole' smart constructor.
data UpdateImpersonationRole = UpdateImpersonationRole'
  { -- | The updated impersonation role description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The WorkMail organization that contains the impersonation role to
    -- update.
    organizationId :: Prelude.Text,
    -- | The ID of the impersonation role to update.
    impersonationRoleId :: Prelude.Text,
    -- | The updated impersonation role name.
    name :: Prelude.Text,
    -- | The updated impersonation role type.
    type' :: ImpersonationRoleType,
    -- | The updated list of rules.
    rules :: [ImpersonationRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateImpersonationRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateImpersonationRole_description' - The updated impersonation role description.
--
-- 'organizationId', 'updateImpersonationRole_organizationId' - The WorkMail organization that contains the impersonation role to
-- update.
--
-- 'impersonationRoleId', 'updateImpersonationRole_impersonationRoleId' - The ID of the impersonation role to update.
--
-- 'name', 'updateImpersonationRole_name' - The updated impersonation role name.
--
-- 'type'', 'updateImpersonationRole_type' - The updated impersonation role type.
--
-- 'rules', 'updateImpersonationRole_rules' - The updated list of rules.
newUpdateImpersonationRole ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'impersonationRoleId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  ImpersonationRoleType ->
  UpdateImpersonationRole
newUpdateImpersonationRole
  pOrganizationId_
  pImpersonationRoleId_
  pName_
  pType_ =
    UpdateImpersonationRole'
      { description =
          Prelude.Nothing,
        organizationId = pOrganizationId_,
        impersonationRoleId = pImpersonationRoleId_,
        name = pName_,
        type' = pType_,
        rules = Prelude.mempty
      }

-- | The updated impersonation role description.
updateImpersonationRole_description :: Lens.Lens' UpdateImpersonationRole (Prelude.Maybe Prelude.Text)
updateImpersonationRole_description = Lens.lens (\UpdateImpersonationRole' {description} -> description) (\s@UpdateImpersonationRole' {} a -> s {description = a} :: UpdateImpersonationRole)

-- | The WorkMail organization that contains the impersonation role to
-- update.
updateImpersonationRole_organizationId :: Lens.Lens' UpdateImpersonationRole Prelude.Text
updateImpersonationRole_organizationId = Lens.lens (\UpdateImpersonationRole' {organizationId} -> organizationId) (\s@UpdateImpersonationRole' {} a -> s {organizationId = a} :: UpdateImpersonationRole)

-- | The ID of the impersonation role to update.
updateImpersonationRole_impersonationRoleId :: Lens.Lens' UpdateImpersonationRole Prelude.Text
updateImpersonationRole_impersonationRoleId = Lens.lens (\UpdateImpersonationRole' {impersonationRoleId} -> impersonationRoleId) (\s@UpdateImpersonationRole' {} a -> s {impersonationRoleId = a} :: UpdateImpersonationRole)

-- | The updated impersonation role name.
updateImpersonationRole_name :: Lens.Lens' UpdateImpersonationRole Prelude.Text
updateImpersonationRole_name = Lens.lens (\UpdateImpersonationRole' {name} -> name) (\s@UpdateImpersonationRole' {} a -> s {name = a} :: UpdateImpersonationRole)

-- | The updated impersonation role type.
updateImpersonationRole_type :: Lens.Lens' UpdateImpersonationRole ImpersonationRoleType
updateImpersonationRole_type = Lens.lens (\UpdateImpersonationRole' {type'} -> type') (\s@UpdateImpersonationRole' {} a -> s {type' = a} :: UpdateImpersonationRole)

-- | The updated list of rules.
updateImpersonationRole_rules :: Lens.Lens' UpdateImpersonationRole [ImpersonationRule]
updateImpersonationRole_rules = Lens.lens (\UpdateImpersonationRole' {rules} -> rules) (\s@UpdateImpersonationRole' {} a -> s {rules = a} :: UpdateImpersonationRole) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateImpersonationRole where
  type
    AWSResponse UpdateImpersonationRole =
      UpdateImpersonationRoleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateImpersonationRoleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateImpersonationRole where
  hashWithSalt _salt UpdateImpersonationRole' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` impersonationRoleId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` rules

instance Prelude.NFData UpdateImpersonationRole where
  rnf UpdateImpersonationRole' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf impersonationRoleId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf rules

instance Data.ToHeaders UpdateImpersonationRole where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.UpdateImpersonationRole" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateImpersonationRole where
  toJSON UpdateImpersonationRole' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just
              ("ImpersonationRoleId" Data..= impersonationRoleId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Rules" Data..= rules)
          ]
      )

instance Data.ToPath UpdateImpersonationRole where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateImpersonationRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateImpersonationRoleResponse' smart constructor.
data UpdateImpersonationRoleResponse = UpdateImpersonationRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateImpersonationRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateImpersonationRoleResponse_httpStatus' - The response's http status code.
newUpdateImpersonationRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateImpersonationRoleResponse
newUpdateImpersonationRoleResponse pHttpStatus_ =
  UpdateImpersonationRoleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateImpersonationRoleResponse_httpStatus :: Lens.Lens' UpdateImpersonationRoleResponse Prelude.Int
updateImpersonationRoleResponse_httpStatus = Lens.lens (\UpdateImpersonationRoleResponse' {httpStatus} -> httpStatus) (\s@UpdateImpersonationRoleResponse' {} a -> s {httpStatus = a} :: UpdateImpersonationRoleResponse)

instance
  Prelude.NFData
    UpdateImpersonationRoleResponse
  where
  rnf UpdateImpersonationRoleResponse' {..} =
    Prelude.rnf httpStatus
