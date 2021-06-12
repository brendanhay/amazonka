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
-- Module      : Network.AWS.WorkMail.RegisterToWorkMail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an existing and disabled user, group, or resource for Amazon
-- WorkMail use by associating a mailbox and calendaring capabilities. It
-- performs no change if the user, group, or resource is enabled and fails
-- if the user, group, or resource is deleted. This operation results in
-- the accumulation of costs. For more information, see
-- <https://aws.amazon.com/workmail/pricing Pricing>. The equivalent
-- console functionality for this operation is /Enable/.
--
-- Users can either be created by calling the CreateUser API operation or
-- they can be synchronized from your directory. For more information, see
-- DeregisterFromWorkMail.
module Network.AWS.WorkMail.RegisterToWorkMail
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newRegisterToWorkMail' smart constructor.
data RegisterToWorkMail = RegisterToWorkMail'
  { -- | The identifier for the organization under which the user, group, or
    -- resource exists.
    organizationId :: Core.Text,
    -- | The identifier for the user, group, or resource to be updated.
    entityId :: Core.Text,
    -- | The email for the user, group, or resource to be updated.
    email :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'entityId'
  Core.Text ->
  -- | 'email'
  Core.Text ->
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
registerToWorkMail_organizationId :: Lens.Lens' RegisterToWorkMail Core.Text
registerToWorkMail_organizationId = Lens.lens (\RegisterToWorkMail' {organizationId} -> organizationId) (\s@RegisterToWorkMail' {} a -> s {organizationId = a} :: RegisterToWorkMail)

-- | The identifier for the user, group, or resource to be updated.
registerToWorkMail_entityId :: Lens.Lens' RegisterToWorkMail Core.Text
registerToWorkMail_entityId = Lens.lens (\RegisterToWorkMail' {entityId} -> entityId) (\s@RegisterToWorkMail' {} a -> s {entityId = a} :: RegisterToWorkMail)

-- | The email for the user, group, or resource to be updated.
registerToWorkMail_email :: Lens.Lens' RegisterToWorkMail Core.Text
registerToWorkMail_email = Lens.lens (\RegisterToWorkMail' {email} -> email) (\s@RegisterToWorkMail' {} a -> s {email = a} :: RegisterToWorkMail)

instance Core.AWSRequest RegisterToWorkMail where
  type
    AWSResponse RegisterToWorkMail =
      RegisterToWorkMailResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterToWorkMailResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterToWorkMail

instance Core.NFData RegisterToWorkMail

instance Core.ToHeaders RegisterToWorkMail where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.RegisterToWorkMail" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterToWorkMail where
  toJSON RegisterToWorkMail' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId),
            Core.Just ("Email" Core..= email)
          ]
      )

instance Core.ToPath RegisterToWorkMail where
  toPath = Core.const "/"

instance Core.ToQuery RegisterToWorkMail where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterToWorkMailResponse' smart constructor.
data RegisterToWorkMailResponse = RegisterToWorkMailResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RegisterToWorkMailResponse
newRegisterToWorkMailResponse pHttpStatus_ =
  RegisterToWorkMailResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
registerToWorkMailResponse_httpStatus :: Lens.Lens' RegisterToWorkMailResponse Core.Int
registerToWorkMailResponse_httpStatus = Lens.lens (\RegisterToWorkMailResponse' {httpStatus} -> httpStatus) (\s@RegisterToWorkMailResponse' {} a -> s {httpStatus = a} :: RegisterToWorkMailResponse)

instance Core.NFData RegisterToWorkMailResponse
