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
-- Module      : Amazonka.SESV2.DeleteEmailIdentityPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified sending authorization policy for the given
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
module Amazonka.SESV2.DeleteEmailIdentityPolicy
  ( -- * Creating a Request
    DeleteEmailIdentityPolicy (..),
    newDeleteEmailIdentityPolicy,

    -- * Request Lenses
    deleteEmailIdentityPolicy_emailIdentity,
    deleteEmailIdentityPolicy_policyName,

    -- * Destructuring the Response
    DeleteEmailIdentityPolicyResponse (..),
    newDeleteEmailIdentityPolicyResponse,

    -- * Response Lenses
    deleteEmailIdentityPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | Represents a request to delete a sending authorization policy for an
-- identity. Sending authorization is an Amazon SES feature that enables
-- you to authorize other senders to use your identities. For information,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-identity-owner-tasks-management.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDeleteEmailIdentityPolicy' smart constructor.
data DeleteEmailIdentityPolicy = DeleteEmailIdentityPolicy'
  { -- | The email identity.
    emailIdentity :: Prelude.Text,
    -- | The name of the policy.
    --
    -- The policy name cannot exceed 64 characters and can only include
    -- alphanumeric characters, dashes, and underscores.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEmailIdentityPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailIdentity', 'deleteEmailIdentityPolicy_emailIdentity' - The email identity.
--
-- 'policyName', 'deleteEmailIdentityPolicy_policyName' - The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include
-- alphanumeric characters, dashes, and underscores.
newDeleteEmailIdentityPolicy ::
  -- | 'emailIdentity'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  DeleteEmailIdentityPolicy
newDeleteEmailIdentityPolicy
  pEmailIdentity_
  pPolicyName_ =
    DeleteEmailIdentityPolicy'
      { emailIdentity =
          pEmailIdentity_,
        policyName = pPolicyName_
      }

-- | The email identity.
deleteEmailIdentityPolicy_emailIdentity :: Lens.Lens' DeleteEmailIdentityPolicy Prelude.Text
deleteEmailIdentityPolicy_emailIdentity = Lens.lens (\DeleteEmailIdentityPolicy' {emailIdentity} -> emailIdentity) (\s@DeleteEmailIdentityPolicy' {} a -> s {emailIdentity = a} :: DeleteEmailIdentityPolicy)

-- | The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include
-- alphanumeric characters, dashes, and underscores.
deleteEmailIdentityPolicy_policyName :: Lens.Lens' DeleteEmailIdentityPolicy Prelude.Text
deleteEmailIdentityPolicy_policyName = Lens.lens (\DeleteEmailIdentityPolicy' {policyName} -> policyName) (\s@DeleteEmailIdentityPolicy' {} a -> s {policyName = a} :: DeleteEmailIdentityPolicy)

instance Core.AWSRequest DeleteEmailIdentityPolicy where
  type
    AWSResponse DeleteEmailIdentityPolicy =
      DeleteEmailIdentityPolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEmailIdentityPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEmailIdentityPolicy where
  hashWithSalt _salt DeleteEmailIdentityPolicy' {..} =
    _salt `Prelude.hashWithSalt` emailIdentity
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData DeleteEmailIdentityPolicy where
  rnf DeleteEmailIdentityPolicy' {..} =
    Prelude.rnf emailIdentity
      `Prelude.seq` Prelude.rnf policyName

instance Data.ToHeaders DeleteEmailIdentityPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteEmailIdentityPolicy where
  toPath DeleteEmailIdentityPolicy' {..} =
    Prelude.mconcat
      [ "/v2/email/identities/",
        Data.toBS emailIdentity,
        "/policies/",
        Data.toBS policyName
      ]

instance Data.ToQuery DeleteEmailIdentityPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newDeleteEmailIdentityPolicyResponse' smart constructor.
data DeleteEmailIdentityPolicyResponse = DeleteEmailIdentityPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEmailIdentityPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEmailIdentityPolicyResponse_httpStatus' - The response's http status code.
newDeleteEmailIdentityPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEmailIdentityPolicyResponse
newDeleteEmailIdentityPolicyResponse pHttpStatus_ =
  DeleteEmailIdentityPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEmailIdentityPolicyResponse_httpStatus :: Lens.Lens' DeleteEmailIdentityPolicyResponse Prelude.Int
deleteEmailIdentityPolicyResponse_httpStatus = Lens.lens (\DeleteEmailIdentityPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteEmailIdentityPolicyResponse' {} a -> s {httpStatus = a} :: DeleteEmailIdentityPolicyResponse)

instance
  Prelude.NFData
    DeleteEmailIdentityPolicyResponse
  where
  rnf DeleteEmailIdentityPolicyResponse' {..} =
    Prelude.rnf httpStatus
