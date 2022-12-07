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
-- Module      : Amazonka.Shield.AssociateDRTRole
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the Shield Response Team (SRT) using the specified role, to
-- access your Amazon Web Services account to assist with DDoS attack
-- mitigation during potential attacks. This enables the SRT to inspect
-- your WAF configuration and create or update WAF rules and web ACLs.
--
-- You can associate only one @RoleArn@ with your subscription. If you
-- submit an @AssociateDRTRole@ request for an account that already has an
-- associated role, the new @RoleArn@ will replace the existing @RoleArn@.
--
-- Prior to making the @AssociateDRTRole@ request, you must attach the
-- @AWSShieldDRTAccessPolicy@ managed policy to the role that you\'ll
-- specify in the request. You can access this policy in the IAM console at
-- <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy>.
-- For more information see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Adding and removing IAM identity permissions>.
-- The role must also trust the service principal
-- @drt.shield.amazonaws.com@. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html IAM JSON policy elements: Principal>.
--
-- The SRT will have access only to your WAF and Shield resources. By
-- submitting this request, you authorize the SRT to inspect your WAF and
-- Shield configuration and create and update WAF rules and web ACLs on
-- your behalf. The SRT takes these actions only if explicitly authorized
-- by you.
--
-- You must have the @iam:PassRole@ permission to make an
-- @AssociateDRTRole@ request. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_passrole.html Granting a user permissions to pass a role to an Amazon Web Services service>.
--
-- To use the services of the SRT and make an @AssociateDRTRole@ request,
-- you must be subscribed to the
-- <http://aws.amazon.com/premiumsupport/business-support/ Business Support plan>
-- or the
-- <http://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan>.
module Amazonka.Shield.AssociateDRTRole
  ( -- * Creating a Request
    AssociateDRTRole (..),
    newAssociateDRTRole,

    -- * Request Lenses
    associateDRTRole_roleArn,

    -- * Destructuring the Response
    AssociateDRTRoleResponse (..),
    newAssociateDRTRoleResponse,

    -- * Response Lenses
    associateDRTRoleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newAssociateDRTRole' smart constructor.
data AssociateDRTRole = AssociateDRTRole'
  { -- | The Amazon Resource Name (ARN) of the role the SRT will use to access
    -- your Amazon Web Services account.
    --
    -- Prior to making the @AssociateDRTRole@ request, you must attach the
    -- <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy>
    -- managed policy to this role. For more information see
    -- <%20https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies>.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDRTRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'associateDRTRole_roleArn' - The Amazon Resource Name (ARN) of the role the SRT will use to access
-- your Amazon Web Services account.
--
-- Prior to making the @AssociateDRTRole@ request, you must attach the
-- <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy>
-- managed policy to this role. For more information see
-- <%20https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies>.
newAssociateDRTRole ::
  -- | 'roleArn'
  Prelude.Text ->
  AssociateDRTRole
newAssociateDRTRole pRoleArn_ =
  AssociateDRTRole' {roleArn = pRoleArn_}

-- | The Amazon Resource Name (ARN) of the role the SRT will use to access
-- your Amazon Web Services account.
--
-- Prior to making the @AssociateDRTRole@ request, you must attach the
-- <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy>
-- managed policy to this role. For more information see
-- <%20https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies>.
associateDRTRole_roleArn :: Lens.Lens' AssociateDRTRole Prelude.Text
associateDRTRole_roleArn = Lens.lens (\AssociateDRTRole' {roleArn} -> roleArn) (\s@AssociateDRTRole' {} a -> s {roleArn = a} :: AssociateDRTRole)

instance Core.AWSRequest AssociateDRTRole where
  type
    AWSResponse AssociateDRTRole =
      AssociateDRTRoleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDRTRoleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateDRTRole where
  hashWithSalt _salt AssociateDRTRole' {..} =
    _salt `Prelude.hashWithSalt` roleArn

instance Prelude.NFData AssociateDRTRole where
  rnf AssociateDRTRole' {..} = Prelude.rnf roleArn

instance Data.ToHeaders AssociateDRTRole where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.AssociateDRTRole" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateDRTRole where
  toJSON AssociateDRTRole' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RoleArn" Data..= roleArn)]
      )

instance Data.ToPath AssociateDRTRole where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateDRTRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateDRTRoleResponse' smart constructor.
data AssociateDRTRoleResponse = AssociateDRTRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDRTRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateDRTRoleResponse_httpStatus' - The response's http status code.
newAssociateDRTRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateDRTRoleResponse
newAssociateDRTRoleResponse pHttpStatus_ =
  AssociateDRTRoleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateDRTRoleResponse_httpStatus :: Lens.Lens' AssociateDRTRoleResponse Prelude.Int
associateDRTRoleResponse_httpStatus = Lens.lens (\AssociateDRTRoleResponse' {httpStatus} -> httpStatus) (\s@AssociateDRTRoleResponse' {} a -> s {httpStatus = a} :: AssociateDRTRoleResponse)

instance Prelude.NFData AssociateDRTRoleResponse where
  rnf AssociateDRTRoleResponse' {..} =
    Prelude.rnf httpStatus
