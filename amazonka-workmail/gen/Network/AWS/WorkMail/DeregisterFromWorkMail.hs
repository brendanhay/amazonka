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
-- Module      : Network.AWS.WorkMail.DeregisterFromWorkMail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Mark a user, group, or resource as no longer used in Amazon WorkMail.
-- This action disassociates the mailbox and schedules it for clean-up.
-- WorkMail keeps mailboxes for 30 days before they are permanently
-- removed. The functionality in the console is /Disable/.
module Network.AWS.WorkMail.DeregisterFromWorkMail
  ( -- * Creating a Request
    DeregisterFromWorkMail (..),
    newDeregisterFromWorkMail,

    -- * Request Lenses
    deregisterFromWorkMail_organizationId,
    deregisterFromWorkMail_entityId,

    -- * Destructuring the Response
    DeregisterFromWorkMailResponse (..),
    newDeregisterFromWorkMailResponse,

    -- * Response Lenses
    deregisterFromWorkMailResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDeregisterFromWorkMail' smart constructor.
data DeregisterFromWorkMail = DeregisterFromWorkMail'
  { -- | The identifier for the organization under which the Amazon WorkMail
    -- entity exists.
    organizationId :: Core.Text,
    -- | The identifier for the member (user or group) to be updated.
    entityId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterFromWorkMail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deregisterFromWorkMail_organizationId' - The identifier for the organization under which the Amazon WorkMail
-- entity exists.
--
-- 'entityId', 'deregisterFromWorkMail_entityId' - The identifier for the member (user or group) to be updated.
newDeregisterFromWorkMail ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'entityId'
  Core.Text ->
  DeregisterFromWorkMail
newDeregisterFromWorkMail pOrganizationId_ pEntityId_ =
  DeregisterFromWorkMail'
    { organizationId =
        pOrganizationId_,
      entityId = pEntityId_
    }

-- | The identifier for the organization under which the Amazon WorkMail
-- entity exists.
deregisterFromWorkMail_organizationId :: Lens.Lens' DeregisterFromWorkMail Core.Text
deregisterFromWorkMail_organizationId = Lens.lens (\DeregisterFromWorkMail' {organizationId} -> organizationId) (\s@DeregisterFromWorkMail' {} a -> s {organizationId = a} :: DeregisterFromWorkMail)

-- | The identifier for the member (user or group) to be updated.
deregisterFromWorkMail_entityId :: Lens.Lens' DeregisterFromWorkMail Core.Text
deregisterFromWorkMail_entityId = Lens.lens (\DeregisterFromWorkMail' {entityId} -> entityId) (\s@DeregisterFromWorkMail' {} a -> s {entityId = a} :: DeregisterFromWorkMail)

instance Core.AWSRequest DeregisterFromWorkMail where
  type
    AWSResponse DeregisterFromWorkMail =
      DeregisterFromWorkMailResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterFromWorkMailResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeregisterFromWorkMail

instance Core.NFData DeregisterFromWorkMail

instance Core.ToHeaders DeregisterFromWorkMail where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.DeregisterFromWorkMail" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterFromWorkMail where
  toJSON DeregisterFromWorkMail' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId)
          ]
      )

instance Core.ToPath DeregisterFromWorkMail where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterFromWorkMail where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterFromWorkMailResponse' smart constructor.
data DeregisterFromWorkMailResponse = DeregisterFromWorkMailResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterFromWorkMailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterFromWorkMailResponse_httpStatus' - The response's http status code.
newDeregisterFromWorkMailResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeregisterFromWorkMailResponse
newDeregisterFromWorkMailResponse pHttpStatus_ =
  DeregisterFromWorkMailResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterFromWorkMailResponse_httpStatus :: Lens.Lens' DeregisterFromWorkMailResponse Core.Int
deregisterFromWorkMailResponse_httpStatus = Lens.lens (\DeregisterFromWorkMailResponse' {httpStatus} -> httpStatus) (\s@DeregisterFromWorkMailResponse' {} a -> s {httpStatus = a} :: DeregisterFromWorkMailResponse)

instance Core.NFData DeregisterFromWorkMailResponse
