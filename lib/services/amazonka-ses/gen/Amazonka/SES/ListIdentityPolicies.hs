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
-- Module      : Amazonka.SES.ListIdentityPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of sending authorization policies that are attached to
-- the given identity (an email address or a domain). This API returns only
-- a list. If you want the actual policy content, you can use
-- @GetIdentityPolicies@.
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
module Amazonka.SES.ListIdentityPolicies
  ( -- * Creating a Request
    ListIdentityPolicies (..),
    newListIdentityPolicies,

    -- * Request Lenses
    listIdentityPolicies_identity,

    -- * Destructuring the Response
    ListIdentityPoliciesResponse (..),
    newListIdentityPoliciesResponse,

    -- * Response Lenses
    listIdentityPoliciesResponse_httpStatus,
    listIdentityPoliciesResponse_policyNames,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to return a list of sending authorization policies
-- that are attached to an identity. Sending authorization is an Amazon SES
-- feature that enables you to authorize other senders to use your
-- identities. For information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- /See:/ 'newListIdentityPolicies' smart constructor.
data ListIdentityPolicies = ListIdentityPolicies'
  { -- | The identity that is associated with the policy for which the policies
    -- will be listed. You can specify an identity by using its name or by
    -- using its Amazon Resource Name (ARN). Examples: @user\@example.com@,
    -- @example.com@,
    -- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
    --
    -- To successfully call this API, you must own the identity.
    identity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'listIdentityPolicies_identity' - The identity that is associated with the policy for which the policies
-- will be listed. You can specify an identity by using its name or by
-- using its Amazon Resource Name (ARN). Examples: @user\@example.com@,
-- @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- To successfully call this API, you must own the identity.
newListIdentityPolicies ::
  -- | 'identity'
  Prelude.Text ->
  ListIdentityPolicies
newListIdentityPolicies pIdentity_ =
  ListIdentityPolicies' {identity = pIdentity_}

-- | The identity that is associated with the policy for which the policies
-- will be listed. You can specify an identity by using its name or by
-- using its Amazon Resource Name (ARN). Examples: @user\@example.com@,
-- @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- To successfully call this API, you must own the identity.
listIdentityPolicies_identity :: Lens.Lens' ListIdentityPolicies Prelude.Text
listIdentityPolicies_identity = Lens.lens (\ListIdentityPolicies' {identity} -> identity) (\s@ListIdentityPolicies' {} a -> s {identity = a} :: ListIdentityPolicies)

instance Core.AWSRequest ListIdentityPolicies where
  type
    AWSResponse ListIdentityPolicies =
      ListIdentityPoliciesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListIdentityPoliciesResult"
      ( \s h x ->
          ListIdentityPoliciesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "PolicyNames" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListIdentityPolicies where
  hashWithSalt _salt ListIdentityPolicies' {..} =
    _salt `Prelude.hashWithSalt` identity

instance Prelude.NFData ListIdentityPolicies where
  rnf ListIdentityPolicies' {..} = Prelude.rnf identity

instance Core.ToHeaders ListIdentityPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListIdentityPolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery ListIdentityPolicies where
  toQuery ListIdentityPolicies' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListIdentityPolicies" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "Identity" Core.=: identity
      ]

-- | A list of names of sending authorization policies that apply to an
-- identity.
--
-- /See:/ 'newListIdentityPoliciesResponse' smart constructor.
data ListIdentityPoliciesResponse = ListIdentityPoliciesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of names of policies that apply to the specified identity.
    policyNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listIdentityPoliciesResponse_httpStatus' - The response's http status code.
--
-- 'policyNames', 'listIdentityPoliciesResponse_policyNames' - A list of names of policies that apply to the specified identity.
newListIdentityPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIdentityPoliciesResponse
newListIdentityPoliciesResponse pHttpStatus_ =
  ListIdentityPoliciesResponse'
    { httpStatus =
        pHttpStatus_,
      policyNames = Prelude.mempty
    }

-- | The response's http status code.
listIdentityPoliciesResponse_httpStatus :: Lens.Lens' ListIdentityPoliciesResponse Prelude.Int
listIdentityPoliciesResponse_httpStatus = Lens.lens (\ListIdentityPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListIdentityPoliciesResponse' {} a -> s {httpStatus = a} :: ListIdentityPoliciesResponse)

-- | A list of names of policies that apply to the specified identity.
listIdentityPoliciesResponse_policyNames :: Lens.Lens' ListIdentityPoliciesResponse [Prelude.Text]
listIdentityPoliciesResponse_policyNames = Lens.lens (\ListIdentityPoliciesResponse' {policyNames} -> policyNames) (\s@ListIdentityPoliciesResponse' {} a -> s {policyNames = a} :: ListIdentityPoliciesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListIdentityPoliciesResponse where
  rnf ListIdentityPoliciesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyNames
