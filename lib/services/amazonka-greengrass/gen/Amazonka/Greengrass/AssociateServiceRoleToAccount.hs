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
-- Module      : Amazonka.Greengrass.AssociateServiceRoleToAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Greengrass.AssociateServiceRoleToAccount
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateServiceRoleToAccount' smart constructor.
data AssociateServiceRoleToAccount = AssociateServiceRoleToAccount'
  { -- | The ARN of the service role you wish to associate with your account.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  AssociateServiceRoleToAccount
newAssociateServiceRoleToAccount pRoleArn_ =
  AssociateServiceRoleToAccount' {roleArn = pRoleArn_}

-- | The ARN of the service role you wish to associate with your account.
associateServiceRoleToAccount_roleArn :: Lens.Lens' AssociateServiceRoleToAccount Prelude.Text
associateServiceRoleToAccount_roleArn = Lens.lens (\AssociateServiceRoleToAccount' {roleArn} -> roleArn) (\s@AssociateServiceRoleToAccount' {} a -> s {roleArn = a} :: AssociateServiceRoleToAccount)

instance
  Core.AWSRequest
    AssociateServiceRoleToAccount
  where
  type
    AWSResponse AssociateServiceRoleToAccount =
      AssociateServiceRoleToAccountResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateServiceRoleToAccountResponse'
            Prelude.<$> (x Core..?> "AssociatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateServiceRoleToAccount
  where
  hashWithSalt _salt AssociateServiceRoleToAccount' {..} =
    _salt `Prelude.hashWithSalt` roleArn

instance Prelude.NFData AssociateServiceRoleToAccount where
  rnf AssociateServiceRoleToAccount' {..} =
    Prelude.rnf roleArn

instance Core.ToHeaders AssociateServiceRoleToAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateServiceRoleToAccount where
  toJSON AssociateServiceRoleToAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("RoleArn" Core..= roleArn)]
      )

instance Core.ToPath AssociateServiceRoleToAccount where
  toPath = Prelude.const "/greengrass/servicerole"

instance Core.ToQuery AssociateServiceRoleToAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateServiceRoleToAccountResponse' smart constructor.
data AssociateServiceRoleToAccountResponse = AssociateServiceRoleToAccountResponse'
  { -- | The time when the service role was associated with the account.
    associatedAt :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AssociateServiceRoleToAccountResponse
newAssociateServiceRoleToAccountResponse pHttpStatus_ =
  AssociateServiceRoleToAccountResponse'
    { associatedAt =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time when the service role was associated with the account.
associateServiceRoleToAccountResponse_associatedAt :: Lens.Lens' AssociateServiceRoleToAccountResponse (Prelude.Maybe Prelude.Text)
associateServiceRoleToAccountResponse_associatedAt = Lens.lens (\AssociateServiceRoleToAccountResponse' {associatedAt} -> associatedAt) (\s@AssociateServiceRoleToAccountResponse' {} a -> s {associatedAt = a} :: AssociateServiceRoleToAccountResponse)

-- | The response's http status code.
associateServiceRoleToAccountResponse_httpStatus :: Lens.Lens' AssociateServiceRoleToAccountResponse Prelude.Int
associateServiceRoleToAccountResponse_httpStatus = Lens.lens (\AssociateServiceRoleToAccountResponse' {httpStatus} -> httpStatus) (\s@AssociateServiceRoleToAccountResponse' {} a -> s {httpStatus = a} :: AssociateServiceRoleToAccountResponse)

instance
  Prelude.NFData
    AssociateServiceRoleToAccountResponse
  where
  rnf AssociateServiceRoleToAccountResponse' {..} =
    Prelude.rnf associatedAt
      `Prelude.seq` Prelude.rnf httpStatus
