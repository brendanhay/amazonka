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
-- Module      : Network.AWS.Shield.DisassociateDRTRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the DDoS Response Team\'s (DRT) access to your AWS account.
--
-- To make a @DisassociateDRTRole@ request, you must be subscribed to the
-- <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan>
-- or the
-- <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan>.
-- However, if you are not subscribed to one of these support plans, but
-- had been previously and had granted the DRT access to your account, you
-- can submit a @DisassociateDRTRole@ request to remove this access.
module Network.AWS.Shield.DisassociateDRTRole
  ( -- * Creating a Request
    DisassociateDRTRole (..),
    newDisassociateDRTRole,

    -- * Destructuring the Response
    DisassociateDRTRoleResponse (..),
    newDisassociateDRTRoleResponse,

    -- * Response Lenses
    disassociateDRTRoleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDisassociateDRTRole' smart constructor.
data DisassociateDRTRole = DisassociateDRTRole'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateDRTRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateDRTRole ::
  DisassociateDRTRole
newDisassociateDRTRole = DisassociateDRTRole'

instance Core.AWSRequest DisassociateDRTRole where
  type
    AWSResponse DisassociateDRTRole =
      DisassociateDRTRoleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDRTRoleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisassociateDRTRole

instance Core.NFData DisassociateDRTRole

instance Core.ToHeaders DisassociateDRTRole where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.DisassociateDRTRole" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateDRTRole where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DisassociateDRTRole where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateDRTRole where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateDRTRoleResponse' smart constructor.
data DisassociateDRTRoleResponse = DisassociateDRTRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateDRTRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateDRTRoleResponse_httpStatus' - The response's http status code.
newDisassociateDRTRoleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateDRTRoleResponse
newDisassociateDRTRoleResponse pHttpStatus_ =
  DisassociateDRTRoleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateDRTRoleResponse_httpStatus :: Lens.Lens' DisassociateDRTRoleResponse Core.Int
disassociateDRTRoleResponse_httpStatus = Lens.lens (\DisassociateDRTRoleResponse' {httpStatus} -> httpStatus) (\s@DisassociateDRTRoleResponse' {} a -> s {httpStatus = a} :: DisassociateDRTRoleResponse)

instance Core.NFData DisassociateDRTRoleResponse
