{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.GetPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource-based policy attached to a private CA. If either the private CA resource or the policy cannot be found, this action returns a @ResourceNotFoundException@ .
--
-- The policy can be attached or updated with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_PutPolicy.html PutPolicy> and removed with <acm-pca/latest/APIReference/API_DeletePolicy.html DeletePolicy> .
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
module Network.AWS.CertificateManagerPCA.GetPolicy
  ( -- * Creating a request
    GetPolicy (..),
    mkGetPolicy,

    -- ** Request lenses
    gpResourceARN,

    -- * Destructuring the response
    GetPolicyResponse (..),
    mkGetPolicyResponse,

    -- ** Response lenses
    gprsPolicy,
    gprsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPolicy' smart constructor.
newtype GetPolicy = GetPolicy' {resourceARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicy' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Number (ARN) of the private CA that will have its policy retrieved. You can find the CA's ARN by calling the ListCertificateAuthorities action.
mkGetPolicy ::
  -- | 'resourceARN'
  Lude.Text ->
  GetPolicy
mkGetPolicy pResourceARN_ = GetPolicy' {resourceARN = pResourceARN_}

-- | The Amazon Resource Number (ARN) of the private CA that will have its policy retrieved. You can find the CA's ARN by calling the ListCertificateAuthorities action.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpResourceARN :: Lens.Lens' GetPolicy Lude.Text
gpResourceARN = Lens.lens (resourceARN :: GetPolicy -> Lude.Text) (\s a -> s {resourceARN = a} :: GetPolicy)
{-# DEPRECATED gpResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest GetPolicy where
  type Rs GetPolicy = GetPolicyResponse
  request = Req.postJSON certificateManagerPCAService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Lude.<$> (x Lude..?> "Policy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.GetPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPolicy where
  toJSON GetPolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ResourceArn" Lude..= resourceARN)])

instance Lude.ToPath GetPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { policy ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - The policy attached to the private CA as a JSON document.
-- * 'responseStatus' - The response status code.
mkGetPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPolicyResponse
mkGetPolicyResponse pResponseStatus_ =
  GetPolicyResponse'
    { policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The policy attached to the private CA as a JSON document.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsPolicy :: Lens.Lens' GetPolicyResponse (Lude.Maybe Lude.Text)
gprsPolicy = Lens.lens (policy :: GetPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: GetPolicyResponse)
{-# DEPRECATED gprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetPolicyResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPolicyResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
