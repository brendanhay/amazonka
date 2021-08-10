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
-- Module      : Network.AWS.SESv2.CreateEmailIdentityPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified sending authorization policy for the given
-- identity (an email address or a domain).
--
-- This API is for the identity owner only. If you have not verified the
-- identity, this API will return an error.
--
-- Sending authorization is a feature that enables an identity owner to
-- authorize other senders to use its identities. For information about
-- using sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SESv2.CreateEmailIdentityPolicy
  ( -- * Creating a Request
    CreateEmailIdentityPolicy (..),
    newCreateEmailIdentityPolicy,

    -- * Request Lenses
    createEmailIdentityPolicy_emailIdentity,
    createEmailIdentityPolicy_policyName,
    createEmailIdentityPolicy_policy,

    -- * Destructuring the Response
    CreateEmailIdentityPolicyResponse (..),
    newCreateEmailIdentityPolicyResponse,

    -- * Response Lenses
    createEmailIdentityPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | Represents a request to create a sending authorization policy for an
-- identity. Sending authorization is an Amazon SES feature that enables
-- you to authorize other senders to use your identities. For information,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-identity-owner-tasks-management.html Amazon SES Developer Guide>.
--
-- /See:/ 'newCreateEmailIdentityPolicy' smart constructor.
data CreateEmailIdentityPolicy = CreateEmailIdentityPolicy'
  { -- | The email identity for which you want to create a policy.
    emailIdentity :: Prelude.Text,
    -- | The name of the policy.
    --
    -- The policy name cannot exceed 64 characters and can only include
    -- alphanumeric characters, dashes, and underscores.
    policyName :: Prelude.Text,
    -- | The text of the policy in JSON format. The policy cannot exceed 4 KB.
    --
    -- For information about the syntax of sending authorization policies, see
    -- the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide>.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEmailIdentityPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailIdentity', 'createEmailIdentityPolicy_emailIdentity' - The email identity for which you want to create a policy.
--
-- 'policyName', 'createEmailIdentityPolicy_policyName' - The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include
-- alphanumeric characters, dashes, and underscores.
--
-- 'policy', 'createEmailIdentityPolicy_policy' - The text of the policy in JSON format. The policy cannot exceed 4 KB.
--
-- For information about the syntax of sending authorization policies, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide>.
newCreateEmailIdentityPolicy ::
  -- | 'emailIdentity'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  CreateEmailIdentityPolicy
newCreateEmailIdentityPolicy
  pEmailIdentity_
  pPolicyName_
  pPolicy_ =
    CreateEmailIdentityPolicy'
      { emailIdentity =
          pEmailIdentity_,
        policyName = pPolicyName_,
        policy = pPolicy_
      }

-- | The email identity for which you want to create a policy.
createEmailIdentityPolicy_emailIdentity :: Lens.Lens' CreateEmailIdentityPolicy Prelude.Text
createEmailIdentityPolicy_emailIdentity = Lens.lens (\CreateEmailIdentityPolicy' {emailIdentity} -> emailIdentity) (\s@CreateEmailIdentityPolicy' {} a -> s {emailIdentity = a} :: CreateEmailIdentityPolicy)

-- | The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include
-- alphanumeric characters, dashes, and underscores.
createEmailIdentityPolicy_policyName :: Lens.Lens' CreateEmailIdentityPolicy Prelude.Text
createEmailIdentityPolicy_policyName = Lens.lens (\CreateEmailIdentityPolicy' {policyName} -> policyName) (\s@CreateEmailIdentityPolicy' {} a -> s {policyName = a} :: CreateEmailIdentityPolicy)

-- | The text of the policy in JSON format. The policy cannot exceed 4 KB.
--
-- For information about the syntax of sending authorization policies, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide>.
createEmailIdentityPolicy_policy :: Lens.Lens' CreateEmailIdentityPolicy Prelude.Text
createEmailIdentityPolicy_policy = Lens.lens (\CreateEmailIdentityPolicy' {policy} -> policy) (\s@CreateEmailIdentityPolicy' {} a -> s {policy = a} :: CreateEmailIdentityPolicy)

instance Core.AWSRequest CreateEmailIdentityPolicy where
  type
    AWSResponse CreateEmailIdentityPolicy =
      CreateEmailIdentityPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateEmailIdentityPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEmailIdentityPolicy

instance Prelude.NFData CreateEmailIdentityPolicy

instance Core.ToHeaders CreateEmailIdentityPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEmailIdentityPolicy where
  toJSON CreateEmailIdentityPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Policy" Core..= policy)]
      )

instance Core.ToPath CreateEmailIdentityPolicy where
  toPath CreateEmailIdentityPolicy' {..} =
    Prelude.mconcat
      [ "/v2/email/identities/",
        Core.toBS emailIdentity,
        "/policies/",
        Core.toBS policyName
      ]

instance Core.ToQuery CreateEmailIdentityPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newCreateEmailIdentityPolicyResponse' smart constructor.
data CreateEmailIdentityPolicyResponse = CreateEmailIdentityPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEmailIdentityPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createEmailIdentityPolicyResponse_httpStatus' - The response's http status code.
newCreateEmailIdentityPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEmailIdentityPolicyResponse
newCreateEmailIdentityPolicyResponse pHttpStatus_ =
  CreateEmailIdentityPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createEmailIdentityPolicyResponse_httpStatus :: Lens.Lens' CreateEmailIdentityPolicyResponse Prelude.Int
createEmailIdentityPolicyResponse_httpStatus = Lens.lens (\CreateEmailIdentityPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateEmailIdentityPolicyResponse' {} a -> s {httpStatus = a} :: CreateEmailIdentityPolicyResponse)

instance
  Prelude.NFData
    CreateEmailIdentityPolicyResponse
