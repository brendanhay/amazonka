{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UploadSigningCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an X.509 signing certificate and associates it with the specified IAM user. Some AWS services use X.509 signing certificates to validate requests that are signed with a corresponding private key. When you upload the certificate, its default status is @Active@ .
--
-- If the @UserName@ is not specified, the IAM user name is determined implicitly based on the AWS access key ID used to sign the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
module Network.AWS.IAM.UploadSigningCertificate
  ( -- * Creating a request
    UploadSigningCertificate (..),
    mkUploadSigningCertificate,

    -- ** Request lenses
    uscfUserName,
    uscfCertificateBody,

    -- * Destructuring the response
    UploadSigningCertificateResponse (..),
    mkUploadSigningCertificateResponse,

    -- ** Response lenses
    uscrsCertificate,
    uscrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUploadSigningCertificate' smart constructor.
data UploadSigningCertificate = UploadSigningCertificate'
  { -- | The name of the user the signing certificate is for.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Maybe Lude.Text,
    -- | The contents of the signing certificate.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
    --
    --     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
    --
    --
    --     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
    --
    --
    --     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
    certificateBody :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadSigningCertificate' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the user the signing certificate is for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'certificateBody' - The contents of the signing certificate.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
mkUploadSigningCertificate ::
  -- | 'certificateBody'
  Lude.Text ->
  UploadSigningCertificate
mkUploadSigningCertificate pCertificateBody_ =
  UploadSigningCertificate'
    { userName = Lude.Nothing,
      certificateBody = pCertificateBody_
    }

-- | The name of the user the signing certificate is for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscfUserName :: Lens.Lens' UploadSigningCertificate (Lude.Maybe Lude.Text)
uscfUserName = Lens.lens (userName :: UploadSigningCertificate -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: UploadSigningCertificate)
{-# DEPRECATED uscfUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The contents of the signing certificate.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'certificateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscfCertificateBody :: Lens.Lens' UploadSigningCertificate Lude.Text
uscfCertificateBody = Lens.lens (certificateBody :: UploadSigningCertificate -> Lude.Text) (\s a -> s {certificateBody = a} :: UploadSigningCertificate)
{-# DEPRECATED uscfCertificateBody "Use generic-lens or generic-optics with 'certificateBody' instead." #-}

instance Lude.AWSRequest UploadSigningCertificate where
  type Rs UploadSigningCertificate = UploadSigningCertificateResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "UploadSigningCertificateResult"
      ( \s h x ->
          UploadSigningCertificateResponse'
            Lude.<$> (x Lude..@ "Certificate") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UploadSigningCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UploadSigningCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery UploadSigningCertificate where
  toQuery UploadSigningCertificate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UploadSigningCertificate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "CertificateBody" Lude.=: certificateBody
      ]

-- | Contains the response to a successful 'UploadSigningCertificate' request.
--
-- /See:/ 'mkUploadSigningCertificateResponse' smart constructor.
data UploadSigningCertificateResponse = UploadSigningCertificateResponse'
  { -- | Information about the certificate.
    certificate :: SigningCertificate,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadSigningCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificate' - Information about the certificate.
-- * 'responseStatus' - The response status code.
mkUploadSigningCertificateResponse ::
  -- | 'certificate'
  SigningCertificate ->
  -- | 'responseStatus'
  Lude.Int ->
  UploadSigningCertificateResponse
mkUploadSigningCertificateResponse pCertificate_ pResponseStatus_ =
  UploadSigningCertificateResponse'
    { certificate = pCertificate_,
      responseStatus = pResponseStatus_
    }

-- | Information about the certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrsCertificate :: Lens.Lens' UploadSigningCertificateResponse SigningCertificate
uscrsCertificate = Lens.lens (certificate :: UploadSigningCertificateResponse -> SigningCertificate) (\s a -> s {certificate = a} :: UploadSigningCertificateResponse)
{-# DEPRECATED uscrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrsResponseStatus :: Lens.Lens' UploadSigningCertificateResponse Lude.Int
uscrsResponseStatus = Lens.lens (responseStatus :: UploadSigningCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UploadSigningCertificateResponse)
{-# DEPRECATED uscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
