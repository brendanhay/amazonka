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
-- Module      : Network.AWS.SES.GetIdentityPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SES.GetIdentityPolicies
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

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
    identity :: Core.Text,
    -- | A list of the names of policies to be retrieved. You can retrieve a
    -- maximum of 20 policies at a time. If you do not know the names of the
    -- policies that are attached to the identity, you can use
    -- @ListIdentityPolicies@.
    policyNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetIdentityPolicies
newGetIdentityPolicies pIdentity_ =
  GetIdentityPolicies'
    { identity = pIdentity_,
      policyNames = Core.mempty
    }

-- | The identity for which the policies will be retrieved. You can specify
-- an identity by using its name or by using its Amazon Resource Name
-- (ARN). Examples: @user\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- To successfully call this API, you must own the identity.
getIdentityPolicies_identity :: Lens.Lens' GetIdentityPolicies Core.Text
getIdentityPolicies_identity = Lens.lens (\GetIdentityPolicies' {identity} -> identity) (\s@GetIdentityPolicies' {} a -> s {identity = a} :: GetIdentityPolicies)

-- | A list of the names of policies to be retrieved. You can retrieve a
-- maximum of 20 policies at a time. If you do not know the names of the
-- policies that are attached to the identity, you can use
-- @ListIdentityPolicies@.
getIdentityPolicies_policyNames :: Lens.Lens' GetIdentityPolicies [Core.Text]
getIdentityPolicies_policyNames = Lens.lens (\GetIdentityPolicies' {policyNames} -> policyNames) (\s@GetIdentityPolicies' {} a -> s {policyNames = a} :: GetIdentityPolicies) Core.. Lens._Coerce

instance Core.AWSRequest GetIdentityPolicies where
  type
    AWSResponse GetIdentityPolicies =
      GetIdentityPoliciesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetIdentityPoliciesResult"
      ( \s h x ->
          GetIdentityPoliciesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "Policies" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLMap "entry" "key" "value"
                     )
      )

instance Core.Hashable GetIdentityPolicies

instance Core.NFData GetIdentityPolicies

instance Core.ToHeaders GetIdentityPolicies where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetIdentityPolicies where
  toPath = Core.const "/"

instance Core.ToQuery GetIdentityPolicies where
  toQuery GetIdentityPolicies' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetIdentityPolicies" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "Identity" Core.=: identity,
        "PolicyNames"
          Core.=: Core.toQueryList "member" policyNames
      ]

-- | Represents the requested sending authorization policies.
--
-- /See:/ 'newGetIdentityPoliciesResponse' smart constructor.
data GetIdentityPoliciesResponse = GetIdentityPoliciesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A map of policy names to policies.
    policies :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetIdentityPoliciesResponse
newGetIdentityPoliciesResponse pHttpStatus_ =
  GetIdentityPoliciesResponse'
    { httpStatus =
        pHttpStatus_,
      policies = Core.mempty
    }

-- | The response's http status code.
getIdentityPoliciesResponse_httpStatus :: Lens.Lens' GetIdentityPoliciesResponse Core.Int
getIdentityPoliciesResponse_httpStatus = Lens.lens (\GetIdentityPoliciesResponse' {httpStatus} -> httpStatus) (\s@GetIdentityPoliciesResponse' {} a -> s {httpStatus = a} :: GetIdentityPoliciesResponse)

-- | A map of policy names to policies.
getIdentityPoliciesResponse_policies :: Lens.Lens' GetIdentityPoliciesResponse (Core.HashMap Core.Text Core.Text)
getIdentityPoliciesResponse_policies = Lens.lens (\GetIdentityPoliciesResponse' {policies} -> policies) (\s@GetIdentityPoliciesResponse' {} a -> s {policies = a} :: GetIdentityPoliciesResponse) Core.. Lens._Coerce

instance Core.NFData GetIdentityPoliciesResponse
