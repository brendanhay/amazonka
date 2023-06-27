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
-- Module      : Amazonka.WorkMail.RegisterToWorkMail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an existing and disabled user, group, or resource for WorkMail
-- use by associating a mailbox and calendaring capabilities. It performs
-- no change if the user, group, or resource is enabled and fails if the
-- user, group, or resource is deleted. This operation results in the
-- accumulation of costs. For more information, see
-- <https://aws.amazon.com/workmail/pricing Pricing>. The equivalent
-- console functionality for this operation is /Enable/.
--
-- Users can either be created by calling the CreateUser API operation or
-- they can be synchronized from your directory. For more information, see
-- DeregisterFromWorkMail.
module Amazonka.WorkMail.RegisterToWorkMail
  ( -- * Creating a Request
    RegisterToWorkMail (..),
    newRegisterToWorkMail,

    -- * Request Lenses
    registerToWorkMail_organizationId,
    registerToWorkMail_entityId,
    registerToWorkMail_email,

    -- * Destructuring the Response
    RegisterToWorkMailResponse (..),
    newRegisterToWorkMailResponse,

    -- * Response Lenses
    registerToWorkMailResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newRegisterToWorkMail' smart constructor.
data RegisterToWorkMail = RegisterToWorkMail'
  { -- | The identifier for the organization under which the user, group, or
    -- resource exists.
    organizationId :: Prelude.Text,
    -- | The identifier for the user, group, or resource to be updated.
    entityId :: Prelude.Text,
    -- | The email for the user, group, or resource to be updated.
    email :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterToWorkMail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'registerToWorkMail_organizationId' - The identifier for the organization under which the user, group, or
-- resource exists.
--
-- 'entityId', 'registerToWorkMail_entityId' - The identifier for the user, group, or resource to be updated.
--
-- 'email', 'registerToWorkMail_email' - The email for the user, group, or resource to be updated.
newRegisterToWorkMail ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  -- | 'email'
  Prelude.Text ->
  RegisterToWorkMail
newRegisterToWorkMail
  pOrganizationId_
  pEntityId_
  pEmail_ =
    RegisterToWorkMail'
      { organizationId =
          pOrganizationId_,
        entityId = pEntityId_,
        email = pEmail_
      }

-- | The identifier for the organization under which the user, group, or
-- resource exists.
registerToWorkMail_organizationId :: Lens.Lens' RegisterToWorkMail Prelude.Text
registerToWorkMail_organizationId = Lens.lens (\RegisterToWorkMail' {organizationId} -> organizationId) (\s@RegisterToWorkMail' {} a -> s {organizationId = a} :: RegisterToWorkMail)

-- | The identifier for the user, group, or resource to be updated.
registerToWorkMail_entityId :: Lens.Lens' RegisterToWorkMail Prelude.Text
registerToWorkMail_entityId = Lens.lens (\RegisterToWorkMail' {entityId} -> entityId) (\s@RegisterToWorkMail' {} a -> s {entityId = a} :: RegisterToWorkMail)

-- | The email for the user, group, or resource to be updated.
registerToWorkMail_email :: Lens.Lens' RegisterToWorkMail Prelude.Text
registerToWorkMail_email = Lens.lens (\RegisterToWorkMail' {email} -> email) (\s@RegisterToWorkMail' {} a -> s {email = a} :: RegisterToWorkMail)

instance Core.AWSRequest RegisterToWorkMail where
  type
    AWSResponse RegisterToWorkMail =
      RegisterToWorkMailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterToWorkMailResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterToWorkMail where
  hashWithSalt _salt RegisterToWorkMail' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` email

instance Prelude.NFData RegisterToWorkMail where
  rnf RegisterToWorkMail' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf email

instance Data.ToHeaders RegisterToWorkMail where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.RegisterToWorkMail" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterToWorkMail where
  toJSON RegisterToWorkMail' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("EntityId" Data..= entityId),
            Prelude.Just ("Email" Data..= email)
          ]
      )

instance Data.ToPath RegisterToWorkMail where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterToWorkMail where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterToWorkMailResponse' smart constructor.
data RegisterToWorkMailResponse = RegisterToWorkMailResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterToWorkMailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerToWorkMailResponse_httpStatus' - The response's http status code.
newRegisterToWorkMailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterToWorkMailResponse
newRegisterToWorkMailResponse pHttpStatus_ =
  RegisterToWorkMailResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
registerToWorkMailResponse_httpStatus :: Lens.Lens' RegisterToWorkMailResponse Prelude.Int
registerToWorkMailResponse_httpStatus = Lens.lens (\RegisterToWorkMailResponse' {httpStatus} -> httpStatus) (\s@RegisterToWorkMailResponse' {} a -> s {httpStatus = a} :: RegisterToWorkMailResponse)

instance Prelude.NFData RegisterToWorkMailResponse where
  rnf RegisterToWorkMailResponse' {..} =
    Prelude.rnf httpStatus
