{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetServerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified server certificate stored in IAM.
--
-- For more information about working with server certificates, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic includes a list of AWS services that can use the server certificates that you manage with IAM.
module Network.AWS.IAM.GetServerCertificate
  ( -- * Creating a request
    GetServerCertificate (..),
    mkGetServerCertificate,

    -- ** Request lenses
    gscServerCertificateName,

    -- * Destructuring the response
    GetServerCertificateResponse (..),
    mkGetServerCertificateResponse,

    -- ** Response lenses
    gscrsResponseStatus,
    gscrsServerCertificate,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetServerCertificate' smart constructor.
newtype GetServerCertificate = GetServerCertificate'
  { serverCertificateName ::
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

-- | Creates a value of 'GetServerCertificate' with the minimum fields required to make a request.
--
-- * 'serverCertificateName' - The name of the server certificate you want to retrieve information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkGetServerCertificate ::
  -- | 'serverCertificateName'
  Lude.Text ->
  GetServerCertificate
mkGetServerCertificate pServerCertificateName_ =
  GetServerCertificate'
    { serverCertificateName =
        pServerCertificateName_
    }

-- | The name of the server certificate you want to retrieve information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'serverCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscServerCertificateName :: Lens.Lens' GetServerCertificate Lude.Text
gscServerCertificateName = Lens.lens (serverCertificateName :: GetServerCertificate -> Lude.Text) (\s a -> s {serverCertificateName = a} :: GetServerCertificate)
{-# DEPRECATED gscServerCertificateName "Use generic-lens or generic-optics with 'serverCertificateName' instead." #-}

instance Lude.AWSRequest GetServerCertificate where
  type Rs GetServerCertificate = GetServerCertificateResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetServerCertificateResult"
      ( \s h x ->
          GetServerCertificateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "ServerCertificate")
      )

instance Lude.ToHeaders GetServerCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetServerCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery GetServerCertificate where
  toQuery GetServerCertificate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetServerCertificate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "ServerCertificateName" Lude.=: serverCertificateName
      ]

-- | Contains the response to a successful 'GetServerCertificate' request.
--
-- /See:/ 'mkGetServerCertificateResponse' smart constructor.
data GetServerCertificateResponse = GetServerCertificateResponse'
  { responseStatus ::
      Lude.Int,
    serverCertificate ::
      ServerCertificate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetServerCertificateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'serverCertificate' - A structure containing details about the server certificate.
mkGetServerCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'serverCertificate'
  ServerCertificate ->
  GetServerCertificateResponse
mkGetServerCertificateResponse pResponseStatus_ pServerCertificate_ =
  GetServerCertificateResponse'
    { responseStatus = pResponseStatus_,
      serverCertificate = pServerCertificate_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrsResponseStatus :: Lens.Lens' GetServerCertificateResponse Lude.Int
gscrsResponseStatus = Lens.lens (responseStatus :: GetServerCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetServerCertificateResponse)
{-# DEPRECATED gscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A structure containing details about the server certificate.
--
-- /Note:/ Consider using 'serverCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrsServerCertificate :: Lens.Lens' GetServerCertificateResponse ServerCertificate
gscrsServerCertificate = Lens.lens (serverCertificate :: GetServerCertificateResponse -> ServerCertificate) (\s a -> s {serverCertificate = a} :: GetServerCertificateResponse)
{-# DEPRECATED gscrsServerCertificate "Use generic-lens or generic-optics with 'serverCertificate' instead." #-}
