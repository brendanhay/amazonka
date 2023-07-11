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
-- Module      : Amazonka.CertificateManagerPCA.DeletePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the resource-based policy attached to a private CA. Deletion
-- will remove any access that the policy has granted. If there is no
-- policy attached to the private CA, this action will return successful.
--
-- If you delete a policy that was applied through Amazon Web Services
-- Resource Access Manager (RAM), the CA will be removed from all shares in
-- which it was included.
--
-- The Certificate Manager Service Linked Role that the policy supports is
-- not affected when you delete the policy.
--
-- The current policy can be shown with
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_GetPolicy.html GetPolicy>
-- and updated with
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_PutPolicy.html PutPolicy>.
--
-- __About Policies__
--
-- -   A policy grants access on a private CA to an Amazon Web Services
--     customer account, to Amazon Web Services Organizations, or to an
--     Amazon Web Services Organizations unit. Policies are under the
--     control of a CA administrator. For more information, see
--     <https://docs.aws.amazon.com/privateca/latest/userguide/pca-rbp.html Using a Resource Based Policy with Amazon Web Services Private CA>.
--
-- -   A policy permits a user of Certificate Manager (ACM) to issue ACM
--     certificates signed by a CA in another account.
--
-- -   For ACM to manage automatic renewal of these certificates, the ACM
--     user must configure a Service Linked Role (SLR). The SLR allows the
--     ACM service to assume the identity of the user, subject to
--     confirmation against the Amazon Web Services Private CA policy. For
--     more information, see
--     <https://docs.aws.amazon.com/acm/latest/userguide/acm-slr.html Using a Service Linked Role with ACM>.
--
-- -   Updates made in Amazon Web Services Resource Manager (RAM) are
--     reflected in policies. For more information, see
--     <https://docs.aws.amazon.com/privateca/latest/userguide/pca-ram.html Attach a Policy for Cross-Account Access>.
module Amazonka.CertificateManagerPCA.DeletePolicy
  ( -- * Creating a Request
    DeletePolicy (..),
    newDeletePolicy,

    -- * Request Lenses
    deletePolicy_resourceArn,

    -- * Destructuring the Response
    DeletePolicyResponse (..),
    newDeletePolicyResponse,
  )
where

import Amazonka.CertificateManagerPCA.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { -- | The Amazon Resource Number (ARN) of the private CA that will have its
    -- policy deleted. You can find the CA\'s ARN by calling the
    -- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
    -- action. The ARN value must have the form
    -- @arn:aws:acm-pca:region:account:certificate-authority\/01234567-89ab-cdef-0123-0123456789ab@.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'deletePolicy_resourceArn' - The Amazon Resource Number (ARN) of the private CA that will have its
-- policy deleted. You can find the CA\'s ARN by calling the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
-- action. The ARN value must have the form
-- @arn:aws:acm-pca:region:account:certificate-authority\/01234567-89ab-cdef-0123-0123456789ab@.
newDeletePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  DeletePolicy
newDeletePolicy pResourceArn_ =
  DeletePolicy' {resourceArn = pResourceArn_}

-- | The Amazon Resource Number (ARN) of the private CA that will have its
-- policy deleted. You can find the CA\'s ARN by calling the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
-- action. The ARN value must have the form
-- @arn:aws:acm-pca:region:account:certificate-authority\/01234567-89ab-cdef-0123-0123456789ab@.
deletePolicy_resourceArn :: Lens.Lens' DeletePolicy Prelude.Text
deletePolicy_resourceArn = Lens.lens (\DeletePolicy' {resourceArn} -> resourceArn) (\s@DeletePolicy' {} a -> s {resourceArn = a} :: DeletePolicy)

instance Core.AWSRequest DeletePolicy where
  type AWSResponse DeletePolicy = DeletePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeletePolicyResponse'

instance Prelude.Hashable DeletePolicy where
  hashWithSalt _salt DeletePolicy' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DeletePolicy where
  rnf DeletePolicy' {..} = Prelude.rnf resourceArn

instance Data.ToHeaders DeletePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("ACMPrivateCA.DeletePolicy" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePolicy where
  toJSON DeletePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath DeletePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePolicyResponse ::
  DeletePolicyResponse
newDeletePolicyResponse = DeletePolicyResponse'

instance Prelude.NFData DeletePolicyResponse where
  rnf _ = ()
