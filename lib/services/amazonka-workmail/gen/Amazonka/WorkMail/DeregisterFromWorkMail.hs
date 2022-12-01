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
-- Module      : Amazonka.WorkMail.DeregisterFromWorkMail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Mark a user, group, or resource as no longer used in WorkMail. This
-- action disassociates the mailbox and schedules it for clean-up. WorkMail
-- keeps mailboxes for 30 days before they are permanently removed. The
-- functionality in the console is /Disable/.
module Amazonka.WorkMail.DeregisterFromWorkMail
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDeregisterFromWorkMail' smart constructor.
data DeregisterFromWorkMail = DeregisterFromWorkMail'
  { -- | The identifier for the organization under which the WorkMail entity
    -- exists.
    organizationId :: Prelude.Text,
    -- | The identifier for the member (user or group) to be updated.
    entityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterFromWorkMail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deregisterFromWorkMail_organizationId' - The identifier for the organization under which the WorkMail entity
-- exists.
--
-- 'entityId', 'deregisterFromWorkMail_entityId' - The identifier for the member (user or group) to be updated.
newDeregisterFromWorkMail ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  DeregisterFromWorkMail
newDeregisterFromWorkMail pOrganizationId_ pEntityId_ =
  DeregisterFromWorkMail'
    { organizationId =
        pOrganizationId_,
      entityId = pEntityId_
    }

-- | The identifier for the organization under which the WorkMail entity
-- exists.
deregisterFromWorkMail_organizationId :: Lens.Lens' DeregisterFromWorkMail Prelude.Text
deregisterFromWorkMail_organizationId = Lens.lens (\DeregisterFromWorkMail' {organizationId} -> organizationId) (\s@DeregisterFromWorkMail' {} a -> s {organizationId = a} :: DeregisterFromWorkMail)

-- | The identifier for the member (user or group) to be updated.
deregisterFromWorkMail_entityId :: Lens.Lens' DeregisterFromWorkMail Prelude.Text
deregisterFromWorkMail_entityId = Lens.lens (\DeregisterFromWorkMail' {entityId} -> entityId) (\s@DeregisterFromWorkMail' {} a -> s {entityId = a} :: DeregisterFromWorkMail)

instance Core.AWSRequest DeregisterFromWorkMail where
  type
    AWSResponse DeregisterFromWorkMail =
      DeregisterFromWorkMailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterFromWorkMailResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterFromWorkMail where
  hashWithSalt _salt DeregisterFromWorkMail' {..} =
    _salt `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` entityId

instance Prelude.NFData DeregisterFromWorkMail where
  rnf DeregisterFromWorkMail' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf entityId

instance Core.ToHeaders DeregisterFromWorkMail where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.DeregisterFromWorkMail" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeregisterFromWorkMail where
  toJSON DeregisterFromWorkMail' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Core..= organizationId),
            Prelude.Just ("EntityId" Core..= entityId)
          ]
      )

instance Core.ToPath DeregisterFromWorkMail where
  toPath = Prelude.const "/"

instance Core.ToQuery DeregisterFromWorkMail where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterFromWorkMailResponse' smart constructor.
data DeregisterFromWorkMailResponse = DeregisterFromWorkMailResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeregisterFromWorkMailResponse
newDeregisterFromWorkMailResponse pHttpStatus_ =
  DeregisterFromWorkMailResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterFromWorkMailResponse_httpStatus :: Lens.Lens' DeregisterFromWorkMailResponse Prelude.Int
deregisterFromWorkMailResponse_httpStatus = Lens.lens (\DeregisterFromWorkMailResponse' {httpStatus} -> httpStatus) (\s@DeregisterFromWorkMailResponse' {} a -> s {httpStatus = a} :: DeregisterFromWorkMailResponse)

instance
  Prelude.NFData
    DeregisterFromWorkMailResponse
  where
  rnf DeregisterFromWorkMailResponse' {..} =
    Prelude.rnf httpStatus
