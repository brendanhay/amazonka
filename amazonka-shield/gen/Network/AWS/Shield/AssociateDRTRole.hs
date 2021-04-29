{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Shield.AssociateDRTRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the DDoS Response Team (DRT), using the specified role, to
-- access your AWS account to assist with DDoS attack mitigation during
-- potential attacks. This enables the DRT to inspect your AWS WAF
-- configuration and create or update AWS WAF rules and web ACLs.
--
-- You can associate only one @RoleArn@ with your subscription. If you
-- submit an @AssociateDRTRole@ request for an account that already has an
-- associated role, the new @RoleArn@ will replace the existing @RoleArn@.
--
-- Prior to making the @AssociateDRTRole@ request, you must attach the
-- <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy>
-- managed policy to the role you will specify in the request. For more
-- information see
-- <%20https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies>.
-- The role must also trust the service principal
-- @ drt.shield.amazonaws.com@. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html IAM JSON Policy Elements: Principal>.
--
-- The DRT will have access only to your AWS WAF and Shield resources. By
-- submitting this request, you authorize the DRT to inspect your AWS WAF
-- and Shield configuration and create and update AWS WAF rules and web
-- ACLs on your behalf. The DRT takes these actions only if explicitly
-- authorized by you.
--
-- You must have the @iam:PassRole@ permission to make an
-- @AssociateDRTRole@ request. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_passrole.html Granting a User Permissions to Pass a Role to an AWS Service>.
--
-- To use the services of the DRT and make an @AssociateDRTRole@ request,
-- you must be subscribed to the
-- <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan>
-- or the
-- <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan>.
module Network.AWS.Shield.AssociateDRTRole
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newAssociateDRTRole' smart constructor.
data AssociateDRTRole = AssociateDRTRole'
  { -- | The Amazon Resource Name (ARN) of the role the DRT will use to access
    -- your AWS account.
    --
    -- Prior to making the @AssociateDRTRole@ request, you must attach the
    -- <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy>
    -- managed policy to this role. For more information see
    -- <%20https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies>.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateDRTRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'associateDRTRole_roleArn' - The Amazon Resource Name (ARN) of the role the DRT will use to access
-- your AWS account.
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

-- | The Amazon Resource Name (ARN) of the role the DRT will use to access
-- your AWS account.
--
-- Prior to making the @AssociateDRTRole@ request, you must attach the
-- <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy>
-- managed policy to this role. For more information see
-- <%20https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies>.
associateDRTRole_roleArn :: Lens.Lens' AssociateDRTRole Prelude.Text
associateDRTRole_roleArn = Lens.lens (\AssociateDRTRole' {roleArn} -> roleArn) (\s@AssociateDRTRole' {} a -> s {roleArn = a} :: AssociateDRTRole)

instance Prelude.AWSRequest AssociateDRTRole where
  type Rs AssociateDRTRole = AssociateDRTRoleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDRTRoleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateDRTRole

instance Prelude.NFData AssociateDRTRole

instance Prelude.ToHeaders AssociateDRTRole where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.AssociateDRTRole" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateDRTRole where
  toJSON AssociateDRTRole' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("RoleArn" Prelude..= roleArn)]
      )

instance Prelude.ToPath AssociateDRTRole where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateDRTRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateDRTRoleResponse' smart constructor.
data AssociateDRTRoleResponse = AssociateDRTRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData AssociateDRTRoleResponse
