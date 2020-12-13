{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteSigningCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a signing certificate associated with the specified IAM user.
--
-- If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated IAM users.
module Network.AWS.IAM.DeleteSigningCertificate
  ( -- * Creating a request
    DeleteSigningCertificate (..),
    mkDeleteSigningCertificate,

    -- ** Request lenses
    dscCertificateId,
    dscUserName,

    -- * Destructuring the response
    DeleteSigningCertificateResponse (..),
    mkDeleteSigningCertificateResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSigningCertificate' smart constructor.
data DeleteSigningCertificate = DeleteSigningCertificate'
  { -- | The ID of the signing certificate to delete.
    --
    -- The format of this parameter, as described by its <http://wikipedia.org/wiki/regex regex> pattern, is a string of characters that can be upper- or lower-cased letters or digits.
    certificateId :: Lude.Text,
    -- | The name of the user the signing certificate belongs to.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSigningCertificate' with the minimum fields required to make a request.
--
-- * 'certificateId' - The ID of the signing certificate to delete.
--
-- The format of this parameter, as described by its <http://wikipedia.org/wiki/regex regex> pattern, is a string of characters that can be upper- or lower-cased letters or digits.
-- * 'userName' - The name of the user the signing certificate belongs to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDeleteSigningCertificate ::
  -- | 'certificateId'
  Lude.Text ->
  DeleteSigningCertificate
mkDeleteSigningCertificate pCertificateId_ =
  DeleteSigningCertificate'
    { certificateId = pCertificateId_,
      userName = Lude.Nothing
    }

-- | The ID of the signing certificate to delete.
--
-- The format of this parameter, as described by its <http://wikipedia.org/wiki/regex regex> pattern, is a string of characters that can be upper- or lower-cased letters or digits.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscCertificateId :: Lens.Lens' DeleteSigningCertificate Lude.Text
dscCertificateId = Lens.lens (certificateId :: DeleteSigningCertificate -> Lude.Text) (\s a -> s {certificateId = a} :: DeleteSigningCertificate)
{-# DEPRECATED dscCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The name of the user the signing certificate belongs to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscUserName :: Lens.Lens' DeleteSigningCertificate (Lude.Maybe Lude.Text)
dscUserName = Lens.lens (userName :: DeleteSigningCertificate -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: DeleteSigningCertificate)
{-# DEPRECATED dscUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest DeleteSigningCertificate where
  type Rs DeleteSigningCertificate = DeleteSigningCertificateResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteSigningCertificateResponse'

instance Lude.ToHeaders DeleteSigningCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteSigningCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSigningCertificate where
  toQuery DeleteSigningCertificate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteSigningCertificate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "CertificateId" Lude.=: certificateId,
        "UserName" Lude.=: userName
      ]

-- | /See:/ 'mkDeleteSigningCertificateResponse' smart constructor.
data DeleteSigningCertificateResponse = DeleteSigningCertificateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSigningCertificateResponse' with the minimum fields required to make a request.
mkDeleteSigningCertificateResponse ::
  DeleteSigningCertificateResponse
mkDeleteSigningCertificateResponse =
  DeleteSigningCertificateResponse'
