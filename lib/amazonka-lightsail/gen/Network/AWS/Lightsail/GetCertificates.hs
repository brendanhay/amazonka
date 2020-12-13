{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more Amazon Lightsail SSL/TLS certificates.
module Network.AWS.Lightsail.GetCertificates
  ( -- * Creating a request
    GetCertificates (..),
    mkGetCertificates,

    -- ** Request lenses
    gcCertificateStatuses,
    gcCertificateName,
    gcIncludeCertificateDetails,

    -- * Destructuring the response
    GetCertificatesResponse (..),
    mkGetCertificatesResponse,

    -- ** Response lenses
    gcrsCertificates,
    gcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCertificates' smart constructor.
data GetCertificates = GetCertificates'
  { -- | The status of the certificates for which to return information.
    --
    -- For example, specify @ISSUED@ to return only certificates with an @ISSUED@ status.
    -- When omitted, the response includes all of your certificates in the AWS Region where the request is made, regardless of their current status.
    certificateStatuses :: Lude.Maybe [CertificateStatus],
    -- | The name for the certificate for which to return information.
    --
    -- When omitted, the response includes all of your certificates in the AWS Region where the request is made.
    certificateName :: Lude.Maybe Lude.Text,
    -- | Indicates whether to include detailed information about the certificates in the response.
    --
    -- When omitted, the response includes only the certificate names, Amazon Resource Names (ARNs), domain names, and tags.
    includeCertificateDetails :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCertificates' with the minimum fields required to make a request.
--
-- * 'certificateStatuses' - The status of the certificates for which to return information.
--
-- For example, specify @ISSUED@ to return only certificates with an @ISSUED@ status.
-- When omitted, the response includes all of your certificates in the AWS Region where the request is made, regardless of their current status.
-- * 'certificateName' - The name for the certificate for which to return information.
--
-- When omitted, the response includes all of your certificates in the AWS Region where the request is made.
-- * 'includeCertificateDetails' - Indicates whether to include detailed information about the certificates in the response.
--
-- When omitted, the response includes only the certificate names, Amazon Resource Names (ARNs), domain names, and tags.
mkGetCertificates ::
  GetCertificates
mkGetCertificates =
  GetCertificates'
    { certificateStatuses = Lude.Nothing,
      certificateName = Lude.Nothing,
      includeCertificateDetails = Lude.Nothing
    }

-- | The status of the certificates for which to return information.
--
-- For example, specify @ISSUED@ to return only certificates with an @ISSUED@ status.
-- When omitted, the response includes all of your certificates in the AWS Region where the request is made, regardless of their current status.
--
-- /Note:/ Consider using 'certificateStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCertificateStatuses :: Lens.Lens' GetCertificates (Lude.Maybe [CertificateStatus])
gcCertificateStatuses = Lens.lens (certificateStatuses :: GetCertificates -> Lude.Maybe [CertificateStatus]) (\s a -> s {certificateStatuses = a} :: GetCertificates)
{-# DEPRECATED gcCertificateStatuses "Use generic-lens or generic-optics with 'certificateStatuses' instead." #-}

-- | The name for the certificate for which to return information.
--
-- When omitted, the response includes all of your certificates in the AWS Region where the request is made.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCertificateName :: Lens.Lens' GetCertificates (Lude.Maybe Lude.Text)
gcCertificateName = Lens.lens (certificateName :: GetCertificates -> Lude.Maybe Lude.Text) (\s a -> s {certificateName = a} :: GetCertificates)
{-# DEPRECATED gcCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | Indicates whether to include detailed information about the certificates in the response.
--
-- When omitted, the response includes only the certificate names, Amazon Resource Names (ARNs), domain names, and tags.
--
-- /Note:/ Consider using 'includeCertificateDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcIncludeCertificateDetails :: Lens.Lens' GetCertificates (Lude.Maybe Lude.Bool)
gcIncludeCertificateDetails = Lens.lens (includeCertificateDetails :: GetCertificates -> Lude.Maybe Lude.Bool) (\s a -> s {includeCertificateDetails = a} :: GetCertificates)
{-# DEPRECATED gcIncludeCertificateDetails "Use generic-lens or generic-optics with 'includeCertificateDetails' instead." #-}

instance Lude.AWSRequest GetCertificates where
  type Rs GetCertificates = GetCertificatesResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCertificatesResponse'
            Lude.<$> (x Lude..?> "certificates" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCertificates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetCertificates" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCertificates where
  toJSON GetCertificates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("certificateStatuses" Lude..=) Lude.<$> certificateStatuses,
            ("certificateName" Lude..=) Lude.<$> certificateName,
            ("includeCertificateDetails" Lude..=)
              Lude.<$> includeCertificateDetails
          ]
      )

instance Lude.ToPath GetCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCertificates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCertificatesResponse' smart constructor.
data GetCertificatesResponse = GetCertificatesResponse'
  { -- | An object that describes certificates.
    certificates :: Lude.Maybe [CertificateSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'certificates' - An object that describes certificates.
-- * 'responseStatus' - The response status code.
mkGetCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCertificatesResponse
mkGetCertificatesResponse pResponseStatus_ =
  GetCertificatesResponse'
    { certificates = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes certificates.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsCertificates :: Lens.Lens' GetCertificatesResponse (Lude.Maybe [CertificateSummary])
gcrsCertificates = Lens.lens (certificates :: GetCertificatesResponse -> Lude.Maybe [CertificateSummary]) (\s a -> s {certificates = a} :: GetCertificatesResponse)
{-# DEPRECATED gcrsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetCertificatesResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCertificatesResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
