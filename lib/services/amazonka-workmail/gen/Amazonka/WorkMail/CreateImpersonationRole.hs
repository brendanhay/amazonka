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
-- Module      : Amazonka.WorkMail.CreateImpersonationRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an impersonation role for the given WorkMail organization.
--
-- /Idempotency/ ensures that an API request completes no more than one
-- time. With an idempotent request, if the original request completes
-- successfully, any subsequent retries also complete successfully without
-- performing any further actions.
module Amazonka.WorkMail.CreateImpersonationRole
  ( -- * Creating a Request
    CreateImpersonationRole (..),
    newCreateImpersonationRole,

    -- * Request Lenses
    createImpersonationRole_clientToken,
    createImpersonationRole_description,
    createImpersonationRole_organizationId,
    createImpersonationRole_name,
    createImpersonationRole_type,
    createImpersonationRole_rules,

    -- * Destructuring the Response
    CreateImpersonationRoleResponse (..),
    newCreateImpersonationRoleResponse,

    -- * Response Lenses
    createImpersonationRoleResponse_impersonationRoleId,
    createImpersonationRoleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newCreateImpersonationRole' smart constructor.
data CreateImpersonationRole = CreateImpersonationRole'
  { -- | The idempotency token for the client request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the new impersonation role.
    description :: Prelude.Maybe Prelude.Text,
    -- | The WorkMail organization to create the new impersonation role within.
    organizationId :: Prelude.Text,
    -- | The name of the new impersonation role.
    name :: Prelude.Text,
    -- | The impersonation role\'s type. The available impersonation role types
    -- are @READ_ONLY@ or @FULL_ACCESS@.
    type' :: ImpersonationRoleType,
    -- | The list of rules for the impersonation role.
    rules :: [ImpersonationRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImpersonationRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createImpersonationRole_clientToken' - The idempotency token for the client request.
--
-- 'description', 'createImpersonationRole_description' - The description of the new impersonation role.
--
-- 'organizationId', 'createImpersonationRole_organizationId' - The WorkMail organization to create the new impersonation role within.
--
-- 'name', 'createImpersonationRole_name' - The name of the new impersonation role.
--
-- 'type'', 'createImpersonationRole_type' - The impersonation role\'s type. The available impersonation role types
-- are @READ_ONLY@ or @FULL_ACCESS@.
--
-- 'rules', 'createImpersonationRole_rules' - The list of rules for the impersonation role.
newCreateImpersonationRole ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  ImpersonationRoleType ->
  CreateImpersonationRole
newCreateImpersonationRole
  pOrganizationId_
  pName_
  pType_ =
    CreateImpersonationRole'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        organizationId = pOrganizationId_,
        name = pName_,
        type' = pType_,
        rules = Prelude.mempty
      }

-- | The idempotency token for the client request.
createImpersonationRole_clientToken :: Lens.Lens' CreateImpersonationRole (Prelude.Maybe Prelude.Text)
createImpersonationRole_clientToken = Lens.lens (\CreateImpersonationRole' {clientToken} -> clientToken) (\s@CreateImpersonationRole' {} a -> s {clientToken = a} :: CreateImpersonationRole)

-- | The description of the new impersonation role.
createImpersonationRole_description :: Lens.Lens' CreateImpersonationRole (Prelude.Maybe Prelude.Text)
createImpersonationRole_description = Lens.lens (\CreateImpersonationRole' {description} -> description) (\s@CreateImpersonationRole' {} a -> s {description = a} :: CreateImpersonationRole)

-- | The WorkMail organization to create the new impersonation role within.
createImpersonationRole_organizationId :: Lens.Lens' CreateImpersonationRole Prelude.Text
createImpersonationRole_organizationId = Lens.lens (\CreateImpersonationRole' {organizationId} -> organizationId) (\s@CreateImpersonationRole' {} a -> s {organizationId = a} :: CreateImpersonationRole)

-- | The name of the new impersonation role.
createImpersonationRole_name :: Lens.Lens' CreateImpersonationRole Prelude.Text
createImpersonationRole_name = Lens.lens (\CreateImpersonationRole' {name} -> name) (\s@CreateImpersonationRole' {} a -> s {name = a} :: CreateImpersonationRole)

-- | The impersonation role\'s type. The available impersonation role types
-- are @READ_ONLY@ or @FULL_ACCESS@.
createImpersonationRole_type :: Lens.Lens' CreateImpersonationRole ImpersonationRoleType
createImpersonationRole_type = Lens.lens (\CreateImpersonationRole' {type'} -> type') (\s@CreateImpersonationRole' {} a -> s {type' = a} :: CreateImpersonationRole)

-- | The list of rules for the impersonation role.
createImpersonationRole_rules :: Lens.Lens' CreateImpersonationRole [ImpersonationRule]
createImpersonationRole_rules = Lens.lens (\CreateImpersonationRole' {rules} -> rules) (\s@CreateImpersonationRole' {} a -> s {rules = a} :: CreateImpersonationRole) Prelude.. Lens.coerced

instance Core.AWSRequest CreateImpersonationRole where
  type
    AWSResponse CreateImpersonationRole =
      CreateImpersonationRoleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImpersonationRoleResponse'
            Prelude.<$> (x Data..?> "ImpersonationRoleId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateImpersonationRole where
  hashWithSalt _salt CreateImpersonationRole' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` rules

instance Prelude.NFData CreateImpersonationRole where
  rnf CreateImpersonationRole' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf rules

instance Data.ToHeaders CreateImpersonationRole where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.CreateImpersonationRole" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateImpersonationRole where
  toJSON CreateImpersonationRole' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Rules" Data..= rules)
          ]
      )

instance Data.ToPath CreateImpersonationRole where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateImpersonationRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateImpersonationRoleResponse' smart constructor.
data CreateImpersonationRoleResponse = CreateImpersonationRoleResponse'
  { -- | The new impersonation role ID.
    impersonationRoleId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImpersonationRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'impersonationRoleId', 'createImpersonationRoleResponse_impersonationRoleId' - The new impersonation role ID.
--
-- 'httpStatus', 'createImpersonationRoleResponse_httpStatus' - The response's http status code.
newCreateImpersonationRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateImpersonationRoleResponse
newCreateImpersonationRoleResponse pHttpStatus_ =
  CreateImpersonationRoleResponse'
    { impersonationRoleId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new impersonation role ID.
createImpersonationRoleResponse_impersonationRoleId :: Lens.Lens' CreateImpersonationRoleResponse (Prelude.Maybe Prelude.Text)
createImpersonationRoleResponse_impersonationRoleId = Lens.lens (\CreateImpersonationRoleResponse' {impersonationRoleId} -> impersonationRoleId) (\s@CreateImpersonationRoleResponse' {} a -> s {impersonationRoleId = a} :: CreateImpersonationRoleResponse)

-- | The response's http status code.
createImpersonationRoleResponse_httpStatus :: Lens.Lens' CreateImpersonationRoleResponse Prelude.Int
createImpersonationRoleResponse_httpStatus = Lens.lens (\CreateImpersonationRoleResponse' {httpStatus} -> httpStatus) (\s@CreateImpersonationRoleResponse' {} a -> s {httpStatus = a} :: CreateImpersonationRoleResponse)

instance
  Prelude.NFData
    CreateImpersonationRoleResponse
  where
  rnf CreateImpersonationRoleResponse' {..} =
    Prelude.rnf impersonationRoleId
      `Prelude.seq` Prelude.rnf httpStatus
