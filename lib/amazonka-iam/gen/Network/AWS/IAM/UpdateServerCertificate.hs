{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateServerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and/or the path of the specified server certificate stored in IAM.
--
-- For more information about working with server certificates, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic also includes a list of AWS services that can use the server certificates that you manage with IAM.
-- /Important:/ You should understand the implications of changing a server certificate's path or name. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs_manage.html#RenamingServerCerts Renaming a Server Certificate> in the /IAM User Guide/ .
module Network.AWS.IAM.UpdateServerCertificate
  ( -- * Creating a request
    UpdateServerCertificate (..),
    mkUpdateServerCertificate,

    -- ** Request lenses
    uServerCertificateName,
    uNewServerCertificateName,
    uNewPath,

    -- * Destructuring the response
    UpdateServerCertificateResponse (..),
    mkUpdateServerCertificateResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateServerCertificate' smart constructor.
data UpdateServerCertificate = UpdateServerCertificate'
  { -- | The name of the server certificate that you want to update.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    serverCertificateName :: Lude.Text,
    -- | The new name for the server certificate. Include this only if you are updating the server certificate's name. The name of the certificate cannot contain any spaces.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    newServerCertificateName :: Lude.Maybe Lude.Text,
    -- | The new path for the server certificate. Include this only if you are updating the server certificate's path.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    newPath :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServerCertificate' with the minimum fields required to make a request.
--
-- * 'serverCertificateName' - The name of the server certificate that you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'newServerCertificateName' - The new name for the server certificate. Include this only if you are updating the server certificate's name. The name of the certificate cannot contain any spaces.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'newPath' - The new path for the server certificate. Include this only if you are updating the server certificate's path.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
mkUpdateServerCertificate ::
  -- | 'serverCertificateName'
  Lude.Text ->
  UpdateServerCertificate
mkUpdateServerCertificate pServerCertificateName_ =
  UpdateServerCertificate'
    { serverCertificateName =
        pServerCertificateName_,
      newServerCertificateName = Lude.Nothing,
      newPath = Lude.Nothing
    }

-- | The name of the server certificate that you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'serverCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uServerCertificateName :: Lens.Lens' UpdateServerCertificate Lude.Text
uServerCertificateName = Lens.lens (serverCertificateName :: UpdateServerCertificate -> Lude.Text) (\s a -> s {serverCertificateName = a} :: UpdateServerCertificate)
{-# DEPRECATED uServerCertificateName "Use generic-lens or generic-optics with 'serverCertificateName' instead." #-}

-- | The new name for the server certificate. Include this only if you are updating the server certificate's name. The name of the certificate cannot contain any spaces.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'newServerCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uNewServerCertificateName :: Lens.Lens' UpdateServerCertificate (Lude.Maybe Lude.Text)
uNewServerCertificateName = Lens.lens (newServerCertificateName :: UpdateServerCertificate -> Lude.Maybe Lude.Text) (\s a -> s {newServerCertificateName = a} :: UpdateServerCertificate)
{-# DEPRECATED uNewServerCertificateName "Use generic-lens or generic-optics with 'newServerCertificateName' instead." #-}

-- | The new path for the server certificate. Include this only if you are updating the server certificate's path.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'newPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uNewPath :: Lens.Lens' UpdateServerCertificate (Lude.Maybe Lude.Text)
uNewPath = Lens.lens (newPath :: UpdateServerCertificate -> Lude.Maybe Lude.Text) (\s a -> s {newPath = a} :: UpdateServerCertificate)
{-# DEPRECATED uNewPath "Use generic-lens or generic-optics with 'newPath' instead." #-}

instance Lude.AWSRequest UpdateServerCertificate where
  type Rs UpdateServerCertificate = UpdateServerCertificateResponse
  request = Req.postQuery iamService
  response = Res.receiveNull UpdateServerCertificateResponse'

instance Lude.ToHeaders UpdateServerCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateServerCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateServerCertificate where
  toQuery UpdateServerCertificate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateServerCertificate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "ServerCertificateName" Lude.=: serverCertificateName,
        "NewServerCertificateName" Lude.=: newServerCertificateName,
        "NewPath" Lude.=: newPath
      ]

-- | /See:/ 'mkUpdateServerCertificateResponse' smart constructor.
data UpdateServerCertificateResponse = UpdateServerCertificateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServerCertificateResponse' with the minimum fields required to make a request.
mkUpdateServerCertificateResponse ::
  UpdateServerCertificateResponse
mkUpdateServerCertificateResponse =
  UpdateServerCertificateResponse'
