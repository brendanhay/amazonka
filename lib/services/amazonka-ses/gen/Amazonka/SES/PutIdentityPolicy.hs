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
-- Module      : Amazonka.SES.PutIdentityPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a sending authorization policy for the specified
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
module Amazonka.SES.PutIdentityPolicy
  ( -- * Creating a Request
    PutIdentityPolicy (..),
    newPutIdentityPolicy,

    -- * Request Lenses
    putIdentityPolicy_identity,
    putIdentityPolicy_policyName,
    putIdentityPolicy_policy,

    -- * Destructuring the Response
    PutIdentityPolicyResponse (..),
    newPutIdentityPolicyResponse,

    -- * Response Lenses
    putIdentityPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to add or update a sending authorization policy for
-- an identity. Sending authorization is an Amazon SES feature that enables
-- you to authorize other senders to use your identities. For information,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- /See:/ 'newPutIdentityPolicy' smart constructor.
data PutIdentityPolicy = PutIdentityPolicy'
  { -- | The identity that the policy will apply to. You can specify an identity
    -- by using its name or by using its Amazon Resource Name (ARN). Examples:
    -- @user\@example.com@, @example.com@,
    -- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
    --
    -- To successfully call this API, you must own the identity.
    identity :: Prelude.Text,
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
-- Create a value of 'PutIdentityPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'putIdentityPolicy_identity' - The identity that the policy will apply to. You can specify an identity
-- by using its name or by using its Amazon Resource Name (ARN). Examples:
-- @user\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- To successfully call this API, you must own the identity.
--
-- 'policyName', 'putIdentityPolicy_policyName' - The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include
-- alphanumeric characters, dashes, and underscores.
--
-- 'policy', 'putIdentityPolicy_policy' - The text of the policy in JSON format. The policy cannot exceed 4 KB.
--
-- For information about the syntax of sending authorization policies, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide>.
newPutIdentityPolicy ::
  -- | 'identity'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutIdentityPolicy
newPutIdentityPolicy pIdentity_ pPolicyName_ pPolicy_ =
  PutIdentityPolicy'
    { identity = pIdentity_,
      policyName = pPolicyName_,
      policy = pPolicy_
    }

-- | The identity that the policy will apply to. You can specify an identity
-- by using its name or by using its Amazon Resource Name (ARN). Examples:
-- @user\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- To successfully call this API, you must own the identity.
putIdentityPolicy_identity :: Lens.Lens' PutIdentityPolicy Prelude.Text
putIdentityPolicy_identity = Lens.lens (\PutIdentityPolicy' {identity} -> identity) (\s@PutIdentityPolicy' {} a -> s {identity = a} :: PutIdentityPolicy)

-- | The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include
-- alphanumeric characters, dashes, and underscores.
putIdentityPolicy_policyName :: Lens.Lens' PutIdentityPolicy Prelude.Text
putIdentityPolicy_policyName = Lens.lens (\PutIdentityPolicy' {policyName} -> policyName) (\s@PutIdentityPolicy' {} a -> s {policyName = a} :: PutIdentityPolicy)

-- | The text of the policy in JSON format. The policy cannot exceed 4 KB.
--
-- For information about the syntax of sending authorization policies, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide>.
putIdentityPolicy_policy :: Lens.Lens' PutIdentityPolicy Prelude.Text
putIdentityPolicy_policy = Lens.lens (\PutIdentityPolicy' {policy} -> policy) (\s@PutIdentityPolicy' {} a -> s {policy = a} :: PutIdentityPolicy)

instance Core.AWSRequest PutIdentityPolicy where
  type
    AWSResponse PutIdentityPolicy =
      PutIdentityPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PutIdentityPolicyResult"
      ( \s h x ->
          PutIdentityPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutIdentityPolicy where
  hashWithSalt _salt PutIdentityPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` identity
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutIdentityPolicy where
  rnf PutIdentityPolicy' {..} =
    Prelude.rnf identity
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policy

instance Data.ToHeaders PutIdentityPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutIdentityPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutIdentityPolicy where
  toQuery PutIdentityPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PutIdentityPolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "Identity" Data.=: identity,
        "PolicyName" Data.=: policyName,
        "Policy" Data.=: policy
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newPutIdentityPolicyResponse' smart constructor.
data PutIdentityPolicyResponse = PutIdentityPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutIdentityPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putIdentityPolicyResponse_httpStatus' - The response's http status code.
newPutIdentityPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutIdentityPolicyResponse
newPutIdentityPolicyResponse pHttpStatus_ =
  PutIdentityPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putIdentityPolicyResponse_httpStatus :: Lens.Lens' PutIdentityPolicyResponse Prelude.Int
putIdentityPolicyResponse_httpStatus = Lens.lens (\PutIdentityPolicyResponse' {httpStatus} -> httpStatus) (\s@PutIdentityPolicyResponse' {} a -> s {httpStatus = a} :: PutIdentityPolicyResponse)

instance Prelude.NFData PutIdentityPolicyResponse where
  rnf PutIdentityPolicyResponse' {..} =
    Prelude.rnf httpStatus
