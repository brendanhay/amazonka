{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.RestoreCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a certificate authority (CA) that is in the @DELETED@ state. You can restore a CA during the period that you defined in the __PermanentDeletionTimeInDays__ parameter of the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthority.html DeleteCertificateAuthority> action. Currently, you can specify 7 to 30 days. If you did not specify a __PermanentDeletionTimeInDays__ value, by default you can restore the CA at any time in a 30 day period. You can check the time remaining in the restoration period of a private CA in the @DELETED@ state by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DescribeCertificateAuthority.html DescribeCertificateAuthority> or <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> actions. The status of a restored CA is set to its pre-deletion status when the __RestoreCertificateAuthority__ action returns. To change its status to @ACTIVE@ , call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority> action. If the private CA was in the @PENDING_CERTIFICATE@ state at deletion, you must use the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ImportCertificateAuthorityCertificate.html ImportCertificateAuthorityCertificate> action to import a certificate authority into the private CA before it can be activated. You cannot restore a CA after the restoration period has ended.
module Network.AWS.CertificateManagerPCA.RestoreCertificateAuthority
  ( -- * Creating a request
    RestoreCertificateAuthority (..),
    mkRestoreCertificateAuthority,

    -- ** Request lenses
    rcaCertificateAuthorityARN,

    -- * Destructuring the response
    RestoreCertificateAuthorityResponse (..),
    mkRestoreCertificateAuthorityResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRestoreCertificateAuthority' smart constructor.
newtype RestoreCertificateAuthority = RestoreCertificateAuthority'
  { -- | The Amazon Resource Name (ARN) that was returned when you called the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. This must be of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
    certificateAuthorityARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreCertificateAuthority' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
mkRestoreCertificateAuthority ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  RestoreCertificateAuthority
mkRestoreCertificateAuthority pCertificateAuthorityARN_ =
  RestoreCertificateAuthority'
    { certificateAuthorityARN =
        pCertificateAuthorityARN_
    }

-- | The Amazon Resource Name (ARN) that was returned when you called the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcaCertificateAuthorityARN :: Lens.Lens' RestoreCertificateAuthority Lude.Text
rcaCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: RestoreCertificateAuthority -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: RestoreCertificateAuthority)
{-# DEPRECATED rcaCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.AWSRequest RestoreCertificateAuthority where
  type
    Rs RestoreCertificateAuthority =
      RestoreCertificateAuthorityResponse
  request = Req.postJSON certificateManagerPCAService
  response = Res.receiveNull RestoreCertificateAuthorityResponse'

instance Lude.ToHeaders RestoreCertificateAuthority where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.RestoreCertificateAuthority" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RestoreCertificateAuthority where
  toJSON RestoreCertificateAuthority' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN)
          ]
      )

instance Lude.ToPath RestoreCertificateAuthority where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreCertificateAuthority where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRestoreCertificateAuthorityResponse' smart constructor.
data RestoreCertificateAuthorityResponse = RestoreCertificateAuthorityResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreCertificateAuthorityResponse' with the minimum fields required to make a request.
mkRestoreCertificateAuthorityResponse ::
  RestoreCertificateAuthorityResponse
mkRestoreCertificateAuthorityResponse =
  RestoreCertificateAuthorityResponse'
