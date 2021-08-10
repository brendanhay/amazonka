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
-- Module      : Network.AWS.SESv2.GetEmailIdentityPolicies
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
module Network.AWS.SESv2.GetEmailIdentityPolicies
  ( -- * Creating a Request
    GetEmailIdentityPolicies (..),
    newGetEmailIdentityPolicies,

    -- * Request Lenses
    getEmailIdentityPolicies_emailIdentity,

    -- * Destructuring the Response
    GetEmailIdentityPoliciesResponse (..),
    newGetEmailIdentityPoliciesResponse,

    -- * Response Lenses
    getEmailIdentityPoliciesResponse_policies,
    getEmailIdentityPoliciesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to return the policies of an email identity.
--
-- /See:/ 'newGetEmailIdentityPolicies' smart constructor.
data GetEmailIdentityPolicies = GetEmailIdentityPolicies'
  { -- | The email identity that you want to retrieve policies for.
    emailIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEmailIdentityPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailIdentity', 'getEmailIdentityPolicies_emailIdentity' - The email identity that you want to retrieve policies for.
newGetEmailIdentityPolicies ::
  -- | 'emailIdentity'
  Prelude.Text ->
  GetEmailIdentityPolicies
newGetEmailIdentityPolicies pEmailIdentity_ =
  GetEmailIdentityPolicies'
    { emailIdentity =
        pEmailIdentity_
    }

-- | The email identity that you want to retrieve policies for.
getEmailIdentityPolicies_emailIdentity :: Lens.Lens' GetEmailIdentityPolicies Prelude.Text
getEmailIdentityPolicies_emailIdentity = Lens.lens (\GetEmailIdentityPolicies' {emailIdentity} -> emailIdentity) (\s@GetEmailIdentityPolicies' {} a -> s {emailIdentity = a} :: GetEmailIdentityPolicies)

instance Core.AWSRequest GetEmailIdentityPolicies where
  type
    AWSResponse GetEmailIdentityPolicies =
      GetEmailIdentityPoliciesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEmailIdentityPoliciesResponse'
            Prelude.<$> (x Core..?> "Policies" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEmailIdentityPolicies

instance Prelude.NFData GetEmailIdentityPolicies

instance Core.ToHeaders GetEmailIdentityPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetEmailIdentityPolicies where
  toPath GetEmailIdentityPolicies' {..} =
    Prelude.mconcat
      [ "/v2/email/identities/",
        Core.toBS emailIdentity,
        "/policies"
      ]

instance Core.ToQuery GetEmailIdentityPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | Identity policies associated with email identity.
--
-- /See:/ 'newGetEmailIdentityPoliciesResponse' smart constructor.
data GetEmailIdentityPoliciesResponse = GetEmailIdentityPoliciesResponse'
  { -- | A map of policy names to policies.
    policies :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEmailIdentityPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policies', 'getEmailIdentityPoliciesResponse_policies' - A map of policy names to policies.
--
-- 'httpStatus', 'getEmailIdentityPoliciesResponse_httpStatus' - The response's http status code.
newGetEmailIdentityPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEmailIdentityPoliciesResponse
newGetEmailIdentityPoliciesResponse pHttpStatus_ =
  GetEmailIdentityPoliciesResponse'
    { policies =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of policy names to policies.
getEmailIdentityPoliciesResponse_policies :: Lens.Lens' GetEmailIdentityPoliciesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getEmailIdentityPoliciesResponse_policies = Lens.lens (\GetEmailIdentityPoliciesResponse' {policies} -> policies) (\s@GetEmailIdentityPoliciesResponse' {} a -> s {policies = a} :: GetEmailIdentityPoliciesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getEmailIdentityPoliciesResponse_httpStatus :: Lens.Lens' GetEmailIdentityPoliciesResponse Prelude.Int
getEmailIdentityPoliciesResponse_httpStatus = Lens.lens (\GetEmailIdentityPoliciesResponse' {httpStatus} -> httpStatus) (\s@GetEmailIdentityPoliciesResponse' {} a -> s {httpStatus = a} :: GetEmailIdentityPoliciesResponse)

instance
  Prelude.NFData
    GetEmailIdentityPoliciesResponse
