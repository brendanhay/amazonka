{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeregisterCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes from the system the certificate that was registered for a secured LDAP connection.
module Network.AWS.DirectoryService.DeregisterCertificate
  ( -- * Creating a request
    DeregisterCertificate (..),
    mkDeregisterCertificate,

    -- ** Request lenses
    derDirectoryId,
    derCertificateId,

    -- * Destructuring the response
    DeregisterCertificateResponse (..),
    mkDeregisterCertificateResponse,

    -- ** Response lenses
    dcrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterCertificate' smart constructor.
data DeregisterCertificate = DeregisterCertificate'
  { directoryId ::
      Lude.Text,
    certificateId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterCertificate' with the minimum fields required to make a request.
--
-- * 'certificateId' - The identifier of the certificate.
-- * 'directoryId' - The identifier of the directory.
mkDeregisterCertificate ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'certificateId'
  Lude.Text ->
  DeregisterCertificate
mkDeregisterCertificate pDirectoryId_ pCertificateId_ =
  DeregisterCertificate'
    { directoryId = pDirectoryId_,
      certificateId = pCertificateId_
    }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derDirectoryId :: Lens.Lens' DeregisterCertificate Lude.Text
derDirectoryId = Lens.lens (directoryId :: DeregisterCertificate -> Lude.Text) (\s a -> s {directoryId = a} :: DeregisterCertificate)
{-# DEPRECATED derDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The identifier of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derCertificateId :: Lens.Lens' DeregisterCertificate Lude.Text
derCertificateId = Lens.lens (certificateId :: DeregisterCertificate -> Lude.Text) (\s a -> s {certificateId = a} :: DeregisterCertificate)
{-# DEPRECATED derCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Lude.AWSRequest DeregisterCertificate where
  type Rs DeregisterCertificate = DeregisterCertificateResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeregisterCertificateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.DeregisterCertificate" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterCertificate where
  toJSON DeregisterCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("CertificateId" Lude..= certificateId)
          ]
      )

instance Lude.ToPath DeregisterCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterCertificateResponse' smart constructor.
newtype DeregisterCertificateResponse = DeregisterCertificateResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterCertificateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeregisterCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterCertificateResponse
mkDeregisterCertificateResponse pResponseStatus_ =
  DeregisterCertificateResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DeregisterCertificateResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DeregisterCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterCertificateResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
