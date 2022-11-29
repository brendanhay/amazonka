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
-- Module      : Amazonka.SES.GetIdentityPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the requested sending authorization policies for the given
-- identity (an email address or a domain). The policies are returned as a
-- map of policy names to policy contents. You can retrieve a maximum of 20
-- policies at a time.
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
module Amazonka.SES.GetIdentityPolicies
  ( -- * Creating a Request
    GetIdentityPolicies (..),
    newGetIdentityPolicies,

    -- * Request Lenses
    getIdentityPolicies_identity,
    getIdentityPolicies_policyNames,

    -- * Destructuring the Response
    GetIdentityPoliciesResponse (..),
    newGetIdentityPoliciesResponse,

    -- * Response Lenses
    getIdentityPoliciesResponse_httpStatus,
    getIdentityPoliciesResponse_policies,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to return the requested sending authorization
-- policies for an identity. Sending authorization is an Amazon SES feature
-- that enables you to authorize other senders to use your identities. For
-- information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- /See:/ 'newGetIdentityPolicies' smart constructor.
data GetIdentityPolicies = GetIdentityPolicies'
  { -- | The identity for which the policies will be retrieved. You can specify
    -- an identity by using its name or by using its Amazon Resource Name
    -- (ARN). Examples: @user\@example.com@, @example.com@,
    -- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
    --
    -- To successfully call this API, you must own the identity.
    identity :: Prelude.Text,
    -- | A list of the names of policies to be retrieved. You can retrieve a
    -- maximum of 20 policies at a time. If you do not know the names of the
    -- policies that are attached to the identity, you can use
    -- @ListIdentityPolicies@.
    policyNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdentityPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'getIdentityPolicies_identity' - The identity for which the policies will be retrieved. You can specify
-- an identity by using its name or by using its Amazon Resource Name
-- (ARN). Examples: @user\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- To successfully call this API, you must own the identity.
--
-- 'policyNames', 'getIdentityPolicies_policyNames' - A list of the names of policies to be retrieved. You can retrieve a
-- maximum of 20 policies at a time. If you do not know the names of the
-- policies that are attached to the identity, you can use
-- @ListIdentityPolicies@.
newGetIdentityPolicies ::
  -- | 'identity'
  Prelude.Text ->
  GetIdentityPolicies
newGetIdentityPolicies pIdentity_ =
  GetIdentityPolicies'
    { identity = pIdentity_,
      policyNames = Prelude.mempty
    }

-- | The identity for which the policies will be retrieved. You can specify
-- an identity by using its name or by using its Amazon Resource Name
-- (ARN). Examples: @user\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- To successfully call this API, you must own the identity.
getIdentityPolicies_identity :: Lens.Lens' GetIdentityPolicies Prelude.Text
getIdentityPolicies_identity = Lens.lens (\GetIdentityPolicies' {identity} -> identity) (\s@GetIdentityPolicies' {} a -> s {identity = a} :: GetIdentityPolicies)

-- | A list of the names of policies to be retrieved. You can retrieve a
-- maximum of 20 policies at a time. If you do not know the names of the
-- policies that are attached to the identity, you can use
-- @ListIdentityPolicies@.
getIdentityPolicies_policyNames :: Lens.Lens' GetIdentityPolicies [Prelude.Text]
getIdentityPolicies_policyNames = Lens.lens (\GetIdentityPolicies' {policyNames} -> policyNames) (\s@GetIdentityPolicies' {} a -> s {policyNames = a} :: GetIdentityPolicies) Prelude.. Lens.coerced

instance Core.AWSRequest GetIdentityPolicies where
  type
    AWSResponse GetIdentityPolicies =
      GetIdentityPoliciesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetIdentityPoliciesResult"
      ( \s h x ->
          GetIdentityPoliciesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "Policies" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLMap "entry" "key" "value"
                        )
      )

instance Prelude.Hashable GetIdentityPolicies where
  hashWithSalt _salt GetIdentityPolicies' {..} =
    _salt `Prelude.hashWithSalt` identity
      `Prelude.hashWithSalt` policyNames

instance Prelude.NFData GetIdentityPolicies where
  rnf GetIdentityPolicies' {..} =
    Prelude.rnf identity
      `Prelude.seq` Prelude.rnf policyNames

instance Core.ToHeaders GetIdentityPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetIdentityPolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery GetIdentityPolicies where
  toQuery GetIdentityPolicies' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetIdentityPolicies" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "Identity" Core.=: identity,
        "PolicyNames"
          Core.=: Core.toQueryList "member" policyNames
      ]

-- | Represents the requested sending authorization policies.
--
-- /See:/ 'newGetIdentityPoliciesResponse' smart constructor.
data GetIdentityPoliciesResponse = GetIdentityPoliciesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A map of policy names to policies.
    policies :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdentityPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getIdentityPoliciesResponse_httpStatus' - The response's http status code.
--
-- 'policies', 'getIdentityPoliciesResponse_policies' - A map of policy names to policies.
newGetIdentityPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIdentityPoliciesResponse
newGetIdentityPoliciesResponse pHttpStatus_ =
  GetIdentityPoliciesResponse'
    { httpStatus =
        pHttpStatus_,
      policies = Prelude.mempty
    }

-- | The response's http status code.
getIdentityPoliciesResponse_httpStatus :: Lens.Lens' GetIdentityPoliciesResponse Prelude.Int
getIdentityPoliciesResponse_httpStatus = Lens.lens (\GetIdentityPoliciesResponse' {httpStatus} -> httpStatus) (\s@GetIdentityPoliciesResponse' {} a -> s {httpStatus = a} :: GetIdentityPoliciesResponse)

-- | A map of policy names to policies.
getIdentityPoliciesResponse_policies :: Lens.Lens' GetIdentityPoliciesResponse (Prelude.HashMap Prelude.Text Prelude.Text)
getIdentityPoliciesResponse_policies = Lens.lens (\GetIdentityPoliciesResponse' {policies} -> policies) (\s@GetIdentityPoliciesResponse' {} a -> s {policies = a} :: GetIdentityPoliciesResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetIdentityPoliciesResponse where
  rnf GetIdentityPoliciesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policies
