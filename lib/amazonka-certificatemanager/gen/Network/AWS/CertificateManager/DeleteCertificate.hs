{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.DeleteCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a certificate and its associated private key. If this action succeeds, the certificate no longer appears in the list that can be displayed by calling the 'ListCertificates' action or be retrieved by calling the 'GetCertificate' action. The certificate will not be available for use by AWS services integrated with ACM.
module Network.AWS.CertificateManager.DeleteCertificate
  ( -- * Creating a request
    DeleteCertificate (..),
    mkDeleteCertificate,

    -- ** Request lenses
    dcCertificateARN,

    -- * Destructuring the response
    DeleteCertificateResponse (..),
    mkDeleteCertificateResponse,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCertificate' smart constructor.
newtype DeleteCertificate = DeleteCertificate'
  { -- | String that contains the ARN of the ACM certificate to be deleted. This must be of the form:
    --
    -- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    certificateARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCertificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - String that contains the ARN of the ACM certificate to be deleted. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
mkDeleteCertificate ::
  -- | 'certificateARN'
  Lude.Text ->
  DeleteCertificate
mkDeleteCertificate pCertificateARN_ =
  DeleteCertificate' {certificateARN = pCertificateARN_}

-- | String that contains the ARN of the ACM certificate to be deleted. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCertificateARN :: Lens.Lens' DeleteCertificate Lude.Text
dcCertificateARN = Lens.lens (certificateARN :: DeleteCertificate -> Lude.Text) (\s a -> s {certificateARN = a} :: DeleteCertificate)
{-# DEPRECATED dcCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

instance Lude.AWSRequest DeleteCertificate where
  type Rs DeleteCertificate = DeleteCertificateResponse
  request = Req.postJSON certificateManagerService
  response = Res.receiveNull DeleteCertificateResponse'

instance Lude.ToHeaders DeleteCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CertificateManager.DeleteCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCertificate where
  toJSON DeleteCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CertificateArn" Lude..= certificateARN)]
      )

instance Lude.ToPath DeleteCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCertificateResponse' with the minimum fields required to make a request.
mkDeleteCertificateResponse ::
  DeleteCertificateResponse
mkDeleteCertificateResponse = DeleteCertificateResponse'
