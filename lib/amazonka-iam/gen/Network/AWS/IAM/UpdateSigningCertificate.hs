{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateSigningCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the specified user signing certificate from active to disabled, or vice versa. This operation can be used to disable an IAM user's signing certificate as part of a certificate rotation work flow.
--
-- If the @UserName@ field is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
module Network.AWS.IAM.UpdateSigningCertificate
  ( -- * Creating a request
    UpdateSigningCertificate (..),
    mkUpdateSigningCertificate,

    -- ** Request lenses
    uscStatus,
    uscCertificateId,
    uscUserName,

    -- * Destructuring the response
    UpdateSigningCertificateResponse (..),
    mkUpdateSigningCertificateResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSigningCertificate' smart constructor.
data UpdateSigningCertificate = UpdateSigningCertificate'
  { -- | The status you want to assign to the certificate. @Active@ means that the certificate can be used for API calls to AWS @Inactive@ means that the certificate cannot be used.
    status :: StatusType,
    -- | The ID of the signing certificate you want to update.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
    certificateId :: Lude.Text,
    -- | The name of the IAM user the signing certificate belongs to.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSigningCertificate' with the minimum fields required to make a request.
--
-- * 'status' - The status you want to assign to the certificate. @Active@ means that the certificate can be used for API calls to AWS @Inactive@ means that the certificate cannot be used.
-- * 'certificateId' - The ID of the signing certificate you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
-- * 'userName' - The name of the IAM user the signing certificate belongs to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkUpdateSigningCertificate ::
  -- | 'status'
  StatusType ->
  -- | 'certificateId'
  Lude.Text ->
  UpdateSigningCertificate
mkUpdateSigningCertificate pStatus_ pCertificateId_ =
  UpdateSigningCertificate'
    { status = pStatus_,
      certificateId = pCertificateId_,
      userName = Lude.Nothing
    }

-- | The status you want to assign to the certificate. @Active@ means that the certificate can be used for API calls to AWS @Inactive@ means that the certificate cannot be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscStatus :: Lens.Lens' UpdateSigningCertificate StatusType
uscStatus = Lens.lens (status :: UpdateSigningCertificate -> StatusType) (\s a -> s {status = a} :: UpdateSigningCertificate)
{-# DEPRECATED uscStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the signing certificate you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscCertificateId :: Lens.Lens' UpdateSigningCertificate Lude.Text
uscCertificateId = Lens.lens (certificateId :: UpdateSigningCertificate -> Lude.Text) (\s a -> s {certificateId = a} :: UpdateSigningCertificate)
{-# DEPRECATED uscCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The name of the IAM user the signing certificate belongs to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscUserName :: Lens.Lens' UpdateSigningCertificate (Lude.Maybe Lude.Text)
uscUserName = Lens.lens (userName :: UpdateSigningCertificate -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: UpdateSigningCertificate)
{-# DEPRECATED uscUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest UpdateSigningCertificate where
  type Rs UpdateSigningCertificate = UpdateSigningCertificateResponse
  request = Req.postQuery iamService
  response = Res.receiveNull UpdateSigningCertificateResponse'

instance Lude.ToHeaders UpdateSigningCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateSigningCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSigningCertificate where
  toQuery UpdateSigningCertificate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateSigningCertificate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Status" Lude.=: status,
        "CertificateId" Lude.=: certificateId,
        "UserName" Lude.=: userName
      ]

-- | /See:/ 'mkUpdateSigningCertificateResponse' smart constructor.
data UpdateSigningCertificateResponse = UpdateSigningCertificateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSigningCertificateResponse' with the minimum fields required to make a request.
mkUpdateSigningCertificateResponse ::
  UpdateSigningCertificateResponse
mkUpdateSigningCertificateResponse =
  UpdateSigningCertificateResponse'
