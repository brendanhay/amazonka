{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteServerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified server certificate.
--
-- For more information about working with server certificates, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic also includes a list of AWS services that can use the server certificates that you manage with IAM.
-- /Important:/ If you are using a server certificate with Elastic Load Balancing, deleting the certificate could have implications for your application. If Elastic Load Balancing doesn't detect the deletion of bound certificates, it may continue to use the certificates. This could cause Elastic Load Balancing to stop accepting traffic. We recommend that you remove the reference to the certificate from Elastic Load Balancing before using this command to delete the certificate. For more information, go to <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerListeners.html DeleteLoadBalancerListeners> in the /Elastic Load Balancing API Reference/ .
module Network.AWS.IAM.DeleteServerCertificate
  ( -- * Creating a request
    DeleteServerCertificate (..),
    mkDeleteServerCertificate,

    -- ** Request lenses
    dscServerCertificateName,

    -- * Destructuring the response
    DeleteServerCertificateResponse (..),
    mkDeleteServerCertificateResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteServerCertificate' smart constructor.
newtype DeleteServerCertificate = DeleteServerCertificate'
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

-- | Creates a value of 'DeleteServerCertificate' with the minimum fields required to make a request.
--
-- * 'serverCertificateName' - The name of the server certificate you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDeleteServerCertificate ::
  -- | 'serverCertificateName'
  Lude.Text ->
  DeleteServerCertificate
mkDeleteServerCertificate pServerCertificateName_ =
  DeleteServerCertificate'
    { serverCertificateName =
        pServerCertificateName_
    }

-- | The name of the server certificate you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'serverCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscServerCertificateName :: Lens.Lens' DeleteServerCertificate Lude.Text
dscServerCertificateName = Lens.lens (serverCertificateName :: DeleteServerCertificate -> Lude.Text) (\s a -> s {serverCertificateName = a} :: DeleteServerCertificate)
{-# DEPRECATED dscServerCertificateName "Use generic-lens or generic-optics with 'serverCertificateName' instead." #-}

instance Lude.AWSRequest DeleteServerCertificate where
  type Rs DeleteServerCertificate = DeleteServerCertificateResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteServerCertificateResponse'

instance Lude.ToHeaders DeleteServerCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteServerCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteServerCertificate where
  toQuery DeleteServerCertificate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteServerCertificate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "ServerCertificateName" Lude.=: serverCertificateName
      ]

-- | /See:/ 'mkDeleteServerCertificateResponse' smart constructor.
data DeleteServerCertificateResponse = DeleteServerCertificateResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteServerCertificateResponse' with the minimum fields required to make a request.
mkDeleteServerCertificateResponse ::
  DeleteServerCertificateResponse
mkDeleteServerCertificateResponse =
  DeleteServerCertificateResponse'
