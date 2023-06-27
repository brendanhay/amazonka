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
-- Module      : Amazonka.SES.DeleteIdentityPolicy
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
module Amazonka.SES.DeleteIdentityPolicy
  ( -- * Creating a Request
    DeleteIdentityPolicy (..),
    newDeleteIdentityPolicy,

    -- * Request Lenses
    deleteIdentityPolicy_identity,
    deleteIdentityPolicy_policyName,

    -- * Destructuring the Response
    DeleteIdentityPolicyResponse (..),
    newDeleteIdentityPolicyResponse,

    -- * Response Lenses
    deleteIdentityPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to delete a sending authorization policy for an
-- identity. Sending authorization is an Amazon SES feature that enables
-- you to authorize other senders to use your identities. For information,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDeleteIdentityPolicy' smart constructor.
data DeleteIdentityPolicy = DeleteIdentityPolicy'
  { -- | The identity that is associated with the policy that you want to delete.
    -- You can specify the identity by using its name or by using its Amazon
    -- Resource Name (ARN). Examples: @user\@example.com@, @example.com@,
    -- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
    --
    -- To successfully call this API, you must own the identity.
    identity :: Prelude.Text,
    -- | The name of the policy to be deleted.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIdentityPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'deleteIdentityPolicy_identity' - The identity that is associated with the policy that you want to delete.
-- You can specify the identity by using its name or by using its Amazon
-- Resource Name (ARN). Examples: @user\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- To successfully call this API, you must own the identity.
--
-- 'policyName', 'deleteIdentityPolicy_policyName' - The name of the policy to be deleted.
newDeleteIdentityPolicy ::
  -- | 'identity'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  DeleteIdentityPolicy
newDeleteIdentityPolicy pIdentity_ pPolicyName_ =
  DeleteIdentityPolicy'
    { identity = pIdentity_,
      policyName = pPolicyName_
    }

-- | The identity that is associated with the policy that you want to delete.
-- You can specify the identity by using its name or by using its Amazon
-- Resource Name (ARN). Examples: @user\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- To successfully call this API, you must own the identity.
deleteIdentityPolicy_identity :: Lens.Lens' DeleteIdentityPolicy Prelude.Text
deleteIdentityPolicy_identity = Lens.lens (\DeleteIdentityPolicy' {identity} -> identity) (\s@DeleteIdentityPolicy' {} a -> s {identity = a} :: DeleteIdentityPolicy)

-- | The name of the policy to be deleted.
deleteIdentityPolicy_policyName :: Lens.Lens' DeleteIdentityPolicy Prelude.Text
deleteIdentityPolicy_policyName = Lens.lens (\DeleteIdentityPolicy' {policyName} -> policyName) (\s@DeleteIdentityPolicy' {} a -> s {policyName = a} :: DeleteIdentityPolicy)

instance Core.AWSRequest DeleteIdentityPolicy where
  type
    AWSResponse DeleteIdentityPolicy =
      DeleteIdentityPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteIdentityPolicyResult"
      ( \s h x ->
          DeleteIdentityPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIdentityPolicy where
  hashWithSalt _salt DeleteIdentityPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` identity
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData DeleteIdentityPolicy where
  rnf DeleteIdentityPolicy' {..} =
    Prelude.rnf identity
      `Prelude.seq` Prelude.rnf policyName

instance Data.ToHeaders DeleteIdentityPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteIdentityPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteIdentityPolicy where
  toQuery DeleteIdentityPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteIdentityPolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "Identity" Data.=: identity,
        "PolicyName" Data.=: policyName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newDeleteIdentityPolicyResponse' smart constructor.
data DeleteIdentityPolicyResponse = DeleteIdentityPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIdentityPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteIdentityPolicyResponse_httpStatus' - The response's http status code.
newDeleteIdentityPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIdentityPolicyResponse
newDeleteIdentityPolicyResponse pHttpStatus_ =
  DeleteIdentityPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteIdentityPolicyResponse_httpStatus :: Lens.Lens' DeleteIdentityPolicyResponse Prelude.Int
deleteIdentityPolicyResponse_httpStatus = Lens.lens (\DeleteIdentityPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteIdentityPolicyResponse' {} a -> s {httpStatus = a} :: DeleteIdentityPolicyResponse)

instance Prelude.NFData DeleteIdentityPolicyResponse where
  rnf DeleteIdentityPolicyResponse' {..} =
    Prelude.rnf httpStatus
