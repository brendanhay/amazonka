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
-- Module      : Network.AWS.Greengrass.AssociateServiceRoleToAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a role with your account. AWS IoT Greengrass will use the
-- role to access your Lambda functions and AWS IoT resources. This is
-- necessary for deployments to succeed. The role must have at least
-- minimum permissions in the policy
-- \'\'AWSGreengrassResourceAccessRolePolicy\'\'.
module Network.AWS.Greengrass.AssociateServiceRoleToAccount
  ( -- * Creating a Request
    AssociateServiceRoleToAccount (..),
    newAssociateServiceRoleToAccount,

    -- * Request Lenses
    associateServiceRoleToAccount_roleArn,

    -- * Destructuring the Response
    AssociateServiceRoleToAccountResponse (..),
    newAssociateServiceRoleToAccountResponse,

    -- * Response Lenses
    associateServiceRoleToAccountResponse_associatedAt,
    associateServiceRoleToAccountResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateServiceRoleToAccount' smart constructor.
data AssociateServiceRoleToAccount = AssociateServiceRoleToAccount'
  { -- | The ARN of the service role you wish to associate with your account.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateServiceRoleToAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'associateServiceRoleToAccount_roleArn' - The ARN of the service role you wish to associate with your account.
newAssociateServiceRoleToAccount ::
  -- | 'roleArn'
  Core.Text ->
  AssociateServiceRoleToAccount
newAssociateServiceRoleToAccount pRoleArn_ =
  AssociateServiceRoleToAccount' {roleArn = pRoleArn_}

-- | The ARN of the service role you wish to associate with your account.
associateServiceRoleToAccount_roleArn :: Lens.Lens' AssociateServiceRoleToAccount Core.Text
associateServiceRoleToAccount_roleArn = Lens.lens (\AssociateServiceRoleToAccount' {roleArn} -> roleArn) (\s@AssociateServiceRoleToAccount' {} a -> s {roleArn = a} :: AssociateServiceRoleToAccount)

instance
  Core.AWSRequest
    AssociateServiceRoleToAccount
  where
  type
    AWSResponse AssociateServiceRoleToAccount =
      AssociateServiceRoleToAccountResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateServiceRoleToAccountResponse'
            Core.<$> (x Core..?> "AssociatedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateServiceRoleToAccount

instance Core.NFData AssociateServiceRoleToAccount

instance Core.ToHeaders AssociateServiceRoleToAccount where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateServiceRoleToAccount where
  toJSON AssociateServiceRoleToAccount' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RoleArn" Core..= roleArn)]
      )

instance Core.ToPath AssociateServiceRoleToAccount where
  toPath = Core.const "/greengrass/servicerole"

instance Core.ToQuery AssociateServiceRoleToAccount where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateServiceRoleToAccountResponse' smart constructor.
data AssociateServiceRoleToAccountResponse = AssociateServiceRoleToAccountResponse'
  { -- | The time when the service role was associated with the account.
    associatedAt :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateServiceRoleToAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedAt', 'associateServiceRoleToAccountResponse_associatedAt' - The time when the service role was associated with the account.
--
-- 'httpStatus', 'associateServiceRoleToAccountResponse_httpStatus' - The response's http status code.
newAssociateServiceRoleToAccountResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateServiceRoleToAccountResponse
newAssociateServiceRoleToAccountResponse pHttpStatus_ =
  AssociateServiceRoleToAccountResponse'
    { associatedAt =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time when the service role was associated with the account.
associateServiceRoleToAccountResponse_associatedAt :: Lens.Lens' AssociateServiceRoleToAccountResponse (Core.Maybe Core.Text)
associateServiceRoleToAccountResponse_associatedAt = Lens.lens (\AssociateServiceRoleToAccountResponse' {associatedAt} -> associatedAt) (\s@AssociateServiceRoleToAccountResponse' {} a -> s {associatedAt = a} :: AssociateServiceRoleToAccountResponse)

-- | The response's http status code.
associateServiceRoleToAccountResponse_httpStatus :: Lens.Lens' AssociateServiceRoleToAccountResponse Core.Int
associateServiceRoleToAccountResponse_httpStatus = Lens.lens (\AssociateServiceRoleToAccountResponse' {httpStatus} -> httpStatus) (\s@AssociateServiceRoleToAccountResponse' {} a -> s {httpStatus = a} :: AssociateServiceRoleToAccountResponse)

instance
  Core.NFData
    AssociateServiceRoleToAccountResponse
