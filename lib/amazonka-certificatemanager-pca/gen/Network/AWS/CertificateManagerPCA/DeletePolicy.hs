{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the resource-based policy attached to a private CA. Deletion will remove any access that the policy has granted. If there is no policy attached to the private CA, this action will return successful.
--
-- If you delete a policy that was applied through AWS Resource Access Manager (RAM), the CA will be removed from all shares in which it was included.
-- The AWS Certificate Manager Service Linked Role that the policy supports is not affected when you delete the policy.
-- The current policy can be shown with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetPolicy.html GetPolicy> and updated with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_PutPolicy.html PutPolicy> .
-- __About Policies__
--
--     * A policy grants access on a private CA to an AWS customer account, to AWS Organizations, or to an AWS Organizations unit. Policies are under the control of a CA administrator. For more information, see <acm-pca/latest/userguide/pca-rbp.html Using a Resource Based Policy with ACM Private CA> .
--
--
--     * A policy permits a user of AWS Certificate Manager (ACM) to issue ACM certificates signed by a CA in another account.
--
--
--     * For ACM to manage automatic renewal of these certificates, the ACM user must configure a Service Linked Role (SLR). The SLR allows the ACM service to assume the identity of the user, subject to confirmation against the ACM Private CA policy. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-slr.html Using a Service Linked Role with ACM> .
--
--
--     * Updates made in AWS Resource Manager (RAM) are reflected in policies. For more information, see <acm-pca/latest/userguide/pca-ram.html Using AWS Resource Access Manager (RAM) with ACM Private CA> .
module Network.AWS.CertificateManagerPCA.DeletePolicy
  ( -- * Creating a request
    DeletePolicy (..),
    mkDeletePolicy,

    -- ** Request lenses
    dpResourceARN,

    -- * Destructuring the response
    DeletePolicyResponse (..),
    mkDeletePolicyResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePolicy' smart constructor.
newtype DeletePolicy = DeletePolicy'
  { -- | The Amazon Resource Number (ARN) of the private CA that will have its policy deleted. You can find the CA's ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. The ARN value must have the form @arn:aws:acm-pca:region:account:certificate-authority/01234567-89ab-cdef-0123-0123456789ab@ .
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicy' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Number (ARN) of the private CA that will have its policy deleted. You can find the CA's ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. The ARN value must have the form @arn:aws:acm-pca:region:account:certificate-authority/01234567-89ab-cdef-0123-0123456789ab@ .
mkDeletePolicy ::
  -- | 'resourceARN'
  Lude.Text ->
  DeletePolicy
mkDeletePolicy pResourceARN_ =
  DeletePolicy' {resourceARN = pResourceARN_}

-- | The Amazon Resource Number (ARN) of the private CA that will have its policy deleted. You can find the CA's ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action. The ARN value must have the form @arn:aws:acm-pca:region:account:certificate-authority/01234567-89ab-cdef-0123-0123456789ab@ .
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpResourceARN :: Lens.Lens' DeletePolicy Lude.Text
dpResourceARN = Lens.lens (resourceARN :: DeletePolicy -> Lude.Text) (\s a -> s {resourceARN = a} :: DeletePolicy)
{-# DEPRECATED dpResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest DeletePolicy where
  type Rs DeletePolicy = DeletePolicyResponse
  request = Req.postJSON certificateManagerPCAService
  response = Res.receiveNull DeletePolicyResponse'

instance Lude.ToHeaders DeletePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.DeletePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePolicy where
  toJSON DeletePolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ResourceArn" Lude..= resourceARN)])

instance Lude.ToPath DeletePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicyResponse' with the minimum fields required to make a request.
mkDeletePolicyResponse ::
  DeletePolicyResponse
mkDeletePolicyResponse = DeletePolicyResponse'
