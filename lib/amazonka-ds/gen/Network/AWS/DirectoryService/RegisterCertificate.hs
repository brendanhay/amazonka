{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RegisterCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a certificate for secured LDAP connection.
module Network.AWS.DirectoryService.RegisterCertificate
  ( -- * Creating a request
    RegisterCertificate (..),
    mkRegisterCertificate,

    -- ** Request lenses
    rcDirectoryId,
    rcCertificateData,

    -- * Destructuring the response
    RegisterCertificateResponse (..),
    mkRegisterCertificateResponse,

    -- ** Response lenses
    rcrsCertificateId,
    rcrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterCertificate' smart constructor.
data RegisterCertificate = RegisterCertificate'
  { -- | The identifier of the directory.
    directoryId :: Lude.Text,
    -- | The certificate PEM string that needs to be registered.
    certificateData :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterCertificate' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory.
-- * 'certificateData' - The certificate PEM string that needs to be registered.
mkRegisterCertificate ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'certificateData'
  Lude.Text ->
  RegisterCertificate
mkRegisterCertificate pDirectoryId_ pCertificateData_ =
  RegisterCertificate'
    { directoryId = pDirectoryId_,
      certificateData = pCertificateData_
    }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcDirectoryId :: Lens.Lens' RegisterCertificate Lude.Text
rcDirectoryId = Lens.lens (directoryId :: RegisterCertificate -> Lude.Text) (\s a -> s {directoryId = a} :: RegisterCertificate)
{-# DEPRECATED rcDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The certificate PEM string that needs to be registered.
--
-- /Note:/ Consider using 'certificateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificateData :: Lens.Lens' RegisterCertificate Lude.Text
rcCertificateData = Lens.lens (certificateData :: RegisterCertificate -> Lude.Text) (\s a -> s {certificateData = a} :: RegisterCertificate)
{-# DEPRECATED rcCertificateData "Use generic-lens or generic-optics with 'certificateData' instead." #-}

instance Lude.AWSRequest RegisterCertificate where
  type Rs RegisterCertificate = RegisterCertificateResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterCertificateResponse'
            Lude.<$> (x Lude..?> "CertificateId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.RegisterCertificate" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterCertificate where
  toJSON RegisterCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("CertificateData" Lude..= certificateData)
          ]
      )

instance Lude.ToPath RegisterCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterCertificateResponse' smart constructor.
data RegisterCertificateResponse = RegisterCertificateResponse'
  { -- | The identifier of the certificate.
    certificateId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificateId' - The identifier of the certificate.
-- * 'responseStatus' - The response status code.
mkRegisterCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterCertificateResponse
mkRegisterCertificateResponse pResponseStatus_ =
  RegisterCertificateResponse'
    { certificateId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsCertificateId :: Lens.Lens' RegisterCertificateResponse (Lude.Maybe Lude.Text)
rcrsCertificateId = Lens.lens (certificateId :: RegisterCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: RegisterCertificateResponse)
{-# DEPRECATED rcrsCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsResponseStatus :: Lens.Lens' RegisterCertificateResponse Lude.Int
rcrsResponseStatus = Lens.lens (responseStatus :: RegisterCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterCertificateResponse)
{-# DEPRECATED rcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
