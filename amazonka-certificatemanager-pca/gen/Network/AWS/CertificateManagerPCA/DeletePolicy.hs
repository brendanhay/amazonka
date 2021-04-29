{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CertificateManagerPCA.DeletePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the resource-based policy attached to a private CA. Deletion
-- will remove any access that the policy has granted. If there is no
-- policy attached to the private CA, this action will return successful.
--
-- If you delete a policy that was applied through AWS Resource Access
-- Manager (RAM), the CA will be removed from all shares in which it was
-- included.
--
-- The AWS Certificate Manager Service Linked Role that the policy supports
-- is not affected when you delete the policy.
--
-- The current policy can be shown with
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetPolicy.html GetPolicy>
-- and updated with
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_PutPolicy.html PutPolicy>.
--
-- __About Policies__
--
-- -   A policy grants access on a private CA to an AWS customer account,
--     to AWS Organizations, or to an AWS Organizations unit. Policies are
--     under the control of a CA administrator. For more information, see
--     <https://docs.aws.amazon.com/acm-pca/latest/userguide/pca-rbp.html Using a Resource Based Policy with ACM Private CA>.
--
-- -   A policy permits a user of AWS Certificate Manager (ACM) to issue
--     ACM certificates signed by a CA in another account.
--
-- -   For ACM to manage automatic renewal of these certificates, the ACM
--     user must configure a Service Linked Role (SLR). The SLR allows the
--     ACM service to assume the identity of the user, subject to
--     confirmation against the ACM Private CA policy. For more
--     information, see
--     <https://docs.aws.amazon.com/acm/latest/userguide/acm-slr.html Using a Service Linked Role with ACM>.
--
-- -   Updates made in AWS Resource Manager (RAM) are reflected in
--     policies. For more information, see
--     <https://docs.aws.amazon.com/acm-pca/latest/userguide/pca-ram.html Attach a Policy for Cross-Account Access>.
module Network.AWS.CertificateManagerPCA.DeletePolicy
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

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { -- | The Amazon Resource Number (ARN) of the private CA that will have its
    -- policy deleted. You can find the CA\'s ARN by calling the
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
    -- action. The ARN value must have the form
    -- @arn:aws:acm-pca:region:account:certificate-authority\/01234567-89ab-cdef-0123-0123456789ab@.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
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
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
-- action. The ARN value must have the form
-- @arn:aws:acm-pca:region:account:certificate-authority\/01234567-89ab-cdef-0123-0123456789ab@.
deletePolicy_resourceArn :: Lens.Lens' DeletePolicy Prelude.Text
deletePolicy_resourceArn = Lens.lens (\DeletePolicy' {resourceArn} -> resourceArn) (\s@DeletePolicy' {} a -> s {resourceArn = a} :: DeletePolicy)

instance Prelude.AWSRequest DeletePolicy where
  type Rs DeletePolicy = DeletePolicyResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeletePolicyResponse'

instance Prelude.Hashable DeletePolicy

instance Prelude.NFData DeletePolicy

instance Prelude.ToHeaders DeletePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("ACMPrivateCA.DeletePolicy" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeletePolicy where
  toJSON DeletePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceArn" Prelude..= resourceArn)
          ]
      )

instance Prelude.ToPath DeletePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeletePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePolicyResponse ::
  DeletePolicyResponse
newDeletePolicyResponse = DeletePolicyResponse'

instance Prelude.NFData DeletePolicyResponse
