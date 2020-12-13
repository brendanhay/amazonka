{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays information about the certificate registered for a secured LDAP connection.
module Network.AWS.DirectoryService.DescribeCertificate
  ( -- * Creating a request
    DescribeCertificate (..),
    mkDescribeCertificate,

    -- ** Request lenses
    dcgDirectoryId,
    dcgCertificateId,

    -- * Destructuring the response
    DescribeCertificateResponse (..),
    mkDescribeCertificateResponse,

    -- ** Response lenses
    dcgrsCertificate,
    dcgrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCertificate' smart constructor.
data DescribeCertificate = DescribeCertificate'
  { -- | The identifier of the directory.
    directoryId :: Lude.Text,
    -- | The identifier of the certificate.
    certificateId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCertificate' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory.
-- * 'certificateId' - The identifier of the certificate.
mkDescribeCertificate ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'certificateId'
  Lude.Text ->
  DescribeCertificate
mkDescribeCertificate pDirectoryId_ pCertificateId_ =
  DescribeCertificate'
    { directoryId = pDirectoryId_,
      certificateId = pCertificateId_
    }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgDirectoryId :: Lens.Lens' DescribeCertificate Lude.Text
dcgDirectoryId = Lens.lens (directoryId :: DescribeCertificate -> Lude.Text) (\s a -> s {directoryId = a} :: DescribeCertificate)
{-# DEPRECATED dcgDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The identifier of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgCertificateId :: Lens.Lens' DescribeCertificate Lude.Text
dcgCertificateId = Lens.lens (certificateId :: DescribeCertificate -> Lude.Text) (\s a -> s {certificateId = a} :: DescribeCertificate)
{-# DEPRECATED dcgCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Lude.AWSRequest DescribeCertificate where
  type Rs DescribeCertificate = DescribeCertificateResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCertificateResponse'
            Lude.<$> (x Lude..?> "Certificate") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.DescribeCertificate" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCertificate where
  toJSON DescribeCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("CertificateId" Lude..= certificateId)
          ]
      )

instance Lude.ToPath DescribeCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
  { -- | Information about the certificate, including registered date time, certificate state, the reason for the state, expiration date time, and certificate common name.
    certificate :: Lude.Maybe Certificate,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificate' - Information about the certificate, including registered date time, certificate state, the reason for the state, expiration date time, and certificate common name.
-- * 'responseStatus' - The response status code.
mkDescribeCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCertificateResponse
mkDescribeCertificateResponse pResponseStatus_ =
  DescribeCertificateResponse'
    { certificate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the certificate, including registered date time, certificate state, the reason for the state, expiration date time, and certificate common name.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrsCertificate :: Lens.Lens' DescribeCertificateResponse (Lude.Maybe Certificate)
dcgrsCertificate = Lens.lens (certificate :: DescribeCertificateResponse -> Lude.Maybe Certificate) (\s a -> s {certificate = a} :: DescribeCertificateResponse)
{-# DEPRECATED dcgrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrsResponseStatus :: Lens.Lens' DescribeCertificateResponse Lude.Int
dcgrsResponseStatus = Lens.lens (responseStatus :: DescribeCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCertificateResponse)
{-# DEPRECATED dcgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
