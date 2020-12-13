{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.DeleteCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a private certificate authority (CA). You must provide the Amazon Resource Name (ARN) of the private CA that you want to delete. You can find the ARN by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
--
-- Before you can delete a CA that you have created and activated, you must disable it. To do this, call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority> action and set the __CertificateAuthorityStatus__ parameter to @DISABLED@ .
-- Additionally, you can delete a CA if you are waiting for it to be created (that is, the status of the CA is @CREATING@ ). You can also delete it if the CA has been created but you haven't yet imported the signed certificate into ACM Private CA (that is, the status of the CA is @PENDING_CERTIFICATE@ ).
-- When you successfully call <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthority.html DeleteCertificateAuthority> , the CA's status changes to @DELETED@ . However, the CA won't be permanently deleted until the restoration period has passed. By default, if you do not set the @PermanentDeletionTimeInDays@ parameter, the CA remains restorable for 30 days. You can set the parameter from 7 to 30 days. The <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DescribeCertificateAuthority.html DescribeCertificateAuthority> action returns the time remaining in the restoration window of a private CA in the @DELETED@ state. To restore an eligible CA, call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_RestoreCertificateAuthority.html RestoreCertificateAuthority> action.
module Network.AWS.CertificateManagerPCA.DeleteCertificateAuthority
  ( -- * Creating a request
    DeleteCertificateAuthority (..),
    mkDeleteCertificateAuthority,

    -- ** Request lenses
    dcaPermanentDeletionTimeInDays,
    dcaCertificateAuthorityARN,

    -- * Destructuring the response
    DeleteCertificateAuthorityResponse (..),
    mkDeleteCertificateAuthorityResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCertificateAuthority' smart constructor.
data DeleteCertificateAuthority = DeleteCertificateAuthority'
  { -- | The number of days to make a CA restorable after it has been deleted. This can be anywhere from 7 to 30 days, with 30 being the default.
    permanentDeletionTimeInDays :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must have the following form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
    certificateAuthorityARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCertificateAuthority' with the minimum fields required to make a request.
--
-- * 'permanentDeletionTimeInDays' - The number of days to make a CA restorable after it has been deleted. This can be anywhere from 7 to 30 days, with 30 being the default.
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must have the following form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
mkDeleteCertificateAuthority ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  DeleteCertificateAuthority
mkDeleteCertificateAuthority pCertificateAuthorityARN_ =
  DeleteCertificateAuthority'
    { permanentDeletionTimeInDays =
        Lude.Nothing,
      certificateAuthorityARN = pCertificateAuthorityARN_
    }

-- | The number of days to make a CA restorable after it has been deleted. This can be anywhere from 7 to 30 days, with 30 being the default.
--
-- /Note:/ Consider using 'permanentDeletionTimeInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaPermanentDeletionTimeInDays :: Lens.Lens' DeleteCertificateAuthority (Lude.Maybe Lude.Natural)
dcaPermanentDeletionTimeInDays = Lens.lens (permanentDeletionTimeInDays :: DeleteCertificateAuthority -> Lude.Maybe Lude.Natural) (\s a -> s {permanentDeletionTimeInDays = a} :: DeleteCertificateAuthority)
{-# DEPRECATED dcaPermanentDeletionTimeInDays "Use generic-lens or generic-optics with 'permanentDeletionTimeInDays' instead." #-}

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must have the following form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaCertificateAuthorityARN :: Lens.Lens' DeleteCertificateAuthority Lude.Text
dcaCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: DeleteCertificateAuthority -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: DeleteCertificateAuthority)
{-# DEPRECATED dcaCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.AWSRequest DeleteCertificateAuthority where
  type
    Rs DeleteCertificateAuthority =
      DeleteCertificateAuthorityResponse
  request = Req.postJSON certificateManagerPCAService
  response = Res.receiveNull DeleteCertificateAuthorityResponse'

instance Lude.ToHeaders DeleteCertificateAuthority where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.DeleteCertificateAuthority" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCertificateAuthority where
  toJSON DeleteCertificateAuthority' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PermanentDeletionTimeInDays" Lude..=)
              Lude.<$> permanentDeletionTimeInDays,
            Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN)
          ]
      )

instance Lude.ToPath DeleteCertificateAuthority where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCertificateAuthority where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCertificateAuthorityResponse' smart constructor.
data DeleteCertificateAuthorityResponse = DeleteCertificateAuthorityResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCertificateAuthorityResponse' with the minimum fields required to make a request.
mkDeleteCertificateAuthorityResponse ::
  DeleteCertificateAuthorityResponse
mkDeleteCertificateAuthorityResponse =
  DeleteCertificateAuthorityResponse'
