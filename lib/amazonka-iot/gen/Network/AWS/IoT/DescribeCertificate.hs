{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified certificate.
module Network.AWS.IoT.DescribeCertificate
  ( -- * Creating a request
    DescribeCertificate (..),
    mkDescribeCertificate,

    -- ** Request lenses
    desCertificateId,

    -- * Destructuring the response
    DescribeCertificateResponse (..),
    mkDescribeCertificateResponse,

    -- ** Response lenses
    dcrsCertificateDescription,
    dcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DescribeCertificate operation.
--
-- /See:/ 'mkDescribeCertificate' smart constructor.
newtype DescribeCertificate = DescribeCertificate'
  { certificateId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCertificate' with the minimum fields required to make a request.
--
-- * 'certificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
mkDescribeCertificate ::
  -- | 'certificateId'
  Lude.Text ->
  DescribeCertificate
mkDescribeCertificate pCertificateId_ =
  DescribeCertificate' {certificateId = pCertificateId_}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desCertificateId :: Lens.Lens' DescribeCertificate Lude.Text
desCertificateId = Lens.lens (certificateId :: DescribeCertificate -> Lude.Text) (\s a -> s {certificateId = a} :: DescribeCertificate)
{-# DEPRECATED desCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Lude.AWSRequest DescribeCertificate where
  type Rs DescribeCertificate = DescribeCertificateResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCertificateResponse'
            Lude.<$> (x Lude..?> "certificateDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeCertificate where
  toPath DescribeCertificate' {..} =
    Lude.mconcat ["/certificates/", Lude.toBS certificateId]

instance Lude.ToQuery DescribeCertificate where
  toQuery = Lude.const Lude.mempty

-- | The output of the DescribeCertificate operation.
--
-- /See:/ 'mkDescribeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
  { certificateDescription ::
      Lude.Maybe CertificateDescription,
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

-- | Creates a value of 'DescribeCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificateDescription' - The description of the certificate.
-- * 'responseStatus' - The response status code.
mkDescribeCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCertificateResponse
mkDescribeCertificateResponse pResponseStatus_ =
  DescribeCertificateResponse'
    { certificateDescription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The description of the certificate.
--
-- /Note:/ Consider using 'certificateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCertificateDescription :: Lens.Lens' DescribeCertificateResponse (Lude.Maybe CertificateDescription)
dcrsCertificateDescription = Lens.lens (certificateDescription :: DescribeCertificateResponse -> Lude.Maybe CertificateDescription) (\s a -> s {certificateDescription = a} :: DescribeCertificateResponse)
{-# DEPRECATED dcrsCertificateDescription "Use generic-lens or generic-optics with 'certificateDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeCertificateResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCertificateResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
