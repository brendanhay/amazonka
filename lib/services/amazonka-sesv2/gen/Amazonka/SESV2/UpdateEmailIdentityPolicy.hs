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
-- Module      : Amazonka.SESV2.UpdateEmailIdentityPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified sending authorization policy for the given
-- identity (an email address or a domain). This API returns successfully
-- even if a policy with the specified name does not exist.
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
module Amazonka.SESV2.UpdateEmailIdentityPolicy
  ( -- * Creating a Request
    UpdateEmailIdentityPolicy (..),
    newUpdateEmailIdentityPolicy,

    -- * Request Lenses
    updateEmailIdentityPolicy_emailIdentity,
    updateEmailIdentityPolicy_policyName,
    updateEmailIdentityPolicy_policy,

    -- * Destructuring the Response
    UpdateEmailIdentityPolicyResponse (..),
    newUpdateEmailIdentityPolicyResponse,

    -- * Response Lenses
    updateEmailIdentityPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | Represents a request to update a sending authorization policy for an
-- identity. Sending authorization is an Amazon SES feature that enables
-- you to authorize other senders to use your identities. For information,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-identity-owner-tasks-management.html Amazon SES Developer Guide>.
--
-- /See:/ 'newUpdateEmailIdentityPolicy' smart constructor.
data UpdateEmailIdentityPolicy = UpdateEmailIdentityPolicy'
  { -- | The email identity.
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
-- Create a value of 'UpdateEmailIdentityPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailIdentity', 'updateEmailIdentityPolicy_emailIdentity' - The email identity.
--
-- 'policyName', 'updateEmailIdentityPolicy_policyName' - The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include
-- alphanumeric characters, dashes, and underscores.
--
-- 'policy', 'updateEmailIdentityPolicy_policy' - The text of the policy in JSON format. The policy cannot exceed 4 KB.
--
-- For information about the syntax of sending authorization policies, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide>.
newUpdateEmailIdentityPolicy ::
  -- | 'emailIdentity'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  UpdateEmailIdentityPolicy
newUpdateEmailIdentityPolicy
  pEmailIdentity_
  pPolicyName_
  pPolicy_ =
    UpdateEmailIdentityPolicy'
      { emailIdentity =
          pEmailIdentity_,
        policyName = pPolicyName_,
        policy = pPolicy_
      }

-- | The email identity.
updateEmailIdentityPolicy_emailIdentity :: Lens.Lens' UpdateEmailIdentityPolicy Prelude.Text
updateEmailIdentityPolicy_emailIdentity = Lens.lens (\UpdateEmailIdentityPolicy' {emailIdentity} -> emailIdentity) (\s@UpdateEmailIdentityPolicy' {} a -> s {emailIdentity = a} :: UpdateEmailIdentityPolicy)

-- | The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include
-- alphanumeric characters, dashes, and underscores.
updateEmailIdentityPolicy_policyName :: Lens.Lens' UpdateEmailIdentityPolicy Prelude.Text
updateEmailIdentityPolicy_policyName = Lens.lens (\UpdateEmailIdentityPolicy' {policyName} -> policyName) (\s@UpdateEmailIdentityPolicy' {} a -> s {policyName = a} :: UpdateEmailIdentityPolicy)

-- | The text of the policy in JSON format. The policy cannot exceed 4 KB.
--
-- For information about the syntax of sending authorization policies, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide>.
updateEmailIdentityPolicy_policy :: Lens.Lens' UpdateEmailIdentityPolicy Prelude.Text
updateEmailIdentityPolicy_policy = Lens.lens (\UpdateEmailIdentityPolicy' {policy} -> policy) (\s@UpdateEmailIdentityPolicy' {} a -> s {policy = a} :: UpdateEmailIdentityPolicy)

instance Core.AWSRequest UpdateEmailIdentityPolicy where
  type
    AWSResponse UpdateEmailIdentityPolicy =
      UpdateEmailIdentityPolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEmailIdentityPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEmailIdentityPolicy where
  hashWithSalt _salt UpdateEmailIdentityPolicy' {..} =
    _salt `Prelude.hashWithSalt` emailIdentity
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policy

instance Prelude.NFData UpdateEmailIdentityPolicy where
  rnf UpdateEmailIdentityPolicy' {..} =
    Prelude.rnf emailIdentity
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policy

instance Core.ToHeaders UpdateEmailIdentityPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateEmailIdentityPolicy where
  toJSON UpdateEmailIdentityPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Policy" Core..= policy)]
      )

instance Core.ToPath UpdateEmailIdentityPolicy where
  toPath UpdateEmailIdentityPolicy' {..} =
    Prelude.mconcat
      [ "/v2/email/identities/",
        Core.toBS emailIdentity,
        "/policies/",
        Core.toBS policyName
      ]

instance Core.ToQuery UpdateEmailIdentityPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newUpdateEmailIdentityPolicyResponse' smart constructor.
data UpdateEmailIdentityPolicyResponse = UpdateEmailIdentityPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEmailIdentityPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEmailIdentityPolicyResponse_httpStatus' - The response's http status code.
newUpdateEmailIdentityPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEmailIdentityPolicyResponse
newUpdateEmailIdentityPolicyResponse pHttpStatus_ =
  UpdateEmailIdentityPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateEmailIdentityPolicyResponse_httpStatus :: Lens.Lens' UpdateEmailIdentityPolicyResponse Prelude.Int
updateEmailIdentityPolicyResponse_httpStatus = Lens.lens (\UpdateEmailIdentityPolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateEmailIdentityPolicyResponse' {} a -> s {httpStatus = a} :: UpdateEmailIdentityPolicyResponse)

instance
  Prelude.NFData
    UpdateEmailIdentityPolicyResponse
  where
  rnf UpdateEmailIdentityPolicyResponse' {..} =
    Prelude.rnf httpStatus
