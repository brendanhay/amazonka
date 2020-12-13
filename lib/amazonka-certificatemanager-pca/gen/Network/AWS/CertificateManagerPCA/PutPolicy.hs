{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.PutPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a resource-based policy to a private CA.
--
-- A policy can also be applied by <https://docs.aws.amazon.com/acm-pca/latest/userguide/pca-ram.html sharing> a private CA through AWS Resource Access Manager (RAM).
-- The policy can be displayed with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetPolicy.html GetPolicy> and removed with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeletePolicy.html DeletePolicy> .
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
module Network.AWS.CertificateManagerPCA.PutPolicy
  ( -- * Creating a request
    PutPolicy (..),
    mkPutPolicy,

    -- ** Request lenses
    ppResourceARN,
    ppPolicy,

    -- * Destructuring the response
    PutPolicyResponse (..),
    mkPutPolicyResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutPolicy' smart constructor.
data PutPolicy = PutPolicy'
  { -- | The Amazon Resource Number (ARN) of the private CA to associate with the policy. The ARN of the CA can be found by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
    resourceARN :: Lude.Text,
    -- | The path and filename of a JSON-formatted IAM policy to attach to the specified private CA resource. If this policy does not contain all required statements or if it includes any statement that is not allowed, the @PutPolicy@ action returns an @InvalidPolicyException@ . For information about IAM policy and statement structure, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies> .
    policy :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPolicy' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Number (ARN) of the private CA to associate with the policy. The ARN of the CA can be found by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
--
--
-- * 'policy' - The path and filename of a JSON-formatted IAM policy to attach to the specified private CA resource. If this policy does not contain all required statements or if it includes any statement that is not allowed, the @PutPolicy@ action returns an @InvalidPolicyException@ . For information about IAM policy and statement structure, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies> .
mkPutPolicy ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'policy'
  Lude.Text ->
  PutPolicy
mkPutPolicy pResourceARN_ pPolicy_ =
  PutPolicy' {resourceARN = pResourceARN_, policy = pPolicy_}

-- | The Amazon Resource Number (ARN) of the private CA to associate with the policy. The ARN of the CA can be found by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
--
--
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppResourceARN :: Lens.Lens' PutPolicy Lude.Text
ppResourceARN = Lens.lens (resourceARN :: PutPolicy -> Lude.Text) (\s a -> s {resourceARN = a} :: PutPolicy)
{-# DEPRECATED ppResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The path and filename of a JSON-formatted IAM policy to attach to the specified private CA resource. If this policy does not contain all required statements or if it includes any statement that is not allowed, the @PutPolicy@ action returns an @InvalidPolicyException@ . For information about IAM policy and statement structure, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies> .
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPolicy :: Lens.Lens' PutPolicy Lude.Text
ppPolicy = Lens.lens (policy :: PutPolicy -> Lude.Text) (\s a -> s {policy = a} :: PutPolicy)
{-# DEPRECATED ppPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.AWSRequest PutPolicy where
  type Rs PutPolicy = PutPolicyResponse
  request = Req.postJSON certificateManagerPCAService
  response = Res.receiveNull PutPolicyResponse'

instance Lude.ToHeaders PutPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.PutPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutPolicy where
  toJSON PutPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceArn" Lude..= resourceARN),
            Lude.Just ("Policy" Lude..= policy)
          ]
      )

instance Lude.ToPath PutPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutPolicyResponse' smart constructor.
data PutPolicyResponse = PutPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPolicyResponse' with the minimum fields required to make a request.
mkPutPolicyResponse ::
  PutPolicyResponse
mkPutPolicyResponse = PutPolicyResponse'
