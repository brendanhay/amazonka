{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UploadServerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a server certificate entity for the AWS account. The server certificate entity includes a public key certificate, a private key, and an optional certificate chain, which should all be PEM-encoded.
--
-- We recommend that you use <https://docs.aws.amazon.com/acm/ AWS Certificate Manager> to provision, manage, and deploy your server certificates. With ACM you can request a certificate, deploy it to AWS resources, and let ACM handle certificate renewals for you. Certificates provided by ACM are free. For more information about using ACM, see the <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide> .
-- For more information about working with server certificates, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic includes a list of AWS services that can use the server certificates that you manage with IAM.
-- For information about the number of server certificates you can upload, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html Limitations on IAM Entities and Objects> in the /IAM User Guide/ .
module Network.AWS.IAM.UploadServerCertificate
  ( -- * Creating a request
    UploadServerCertificate (..),
    mkUploadServerCertificate,

    -- ** Request lenses
    uscServerCertificateName,
    uscPrivateKey,
    uscPath,
    uscCertificateBody,
    uscCertificateChain,

    -- * Destructuring the response
    UploadServerCertificateResponse (..),
    mkUploadServerCertificateResponse,

    -- ** Response lenses
    ursServerCertificateMetadata,
    ursResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUploadServerCertificate' smart constructor.
data UploadServerCertificate = UploadServerCertificate'
  { -- | The name for the server certificate. Do not include the path in this value. The name of the certificate cannot contain any spaces.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    serverCertificateName :: Lude.Text,
    -- | The contents of the private key in PEM-encoded format.
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
    privateKey :: Lude.Sensitive Lude.Text,
    -- | The path for the server certificate. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    --
    -- This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    path :: Lude.Maybe Lude.Text,
    -- | The contents of the public key certificate in PEM-encoded format.
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
    certificateBody :: Lude.Text,
    -- | The contents of the certificate chain. This is typically a concatenation of the PEM-encoded public key certificates of the chain.
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
    certificateChain :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadServerCertificate' with the minimum fields required to make a request.
--
-- * 'serverCertificateName' - The name for the server certificate. Do not include the path in this value. The name of the certificate cannot contain any spaces.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'privateKey' - The contents of the private key in PEM-encoded format.
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
-- * 'path' - The path for the server certificate. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'certificateBody' - The contents of the public key certificate in PEM-encoded format.
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
-- * 'certificateChain' - The contents of the certificate chain. This is typically a concatenation of the PEM-encoded public key certificates of the chain.
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
mkUploadServerCertificate ::
  -- | 'serverCertificateName'
  Lude.Text ->
  -- | 'privateKey'
  Lude.Sensitive Lude.Text ->
  -- | 'certificateBody'
  Lude.Text ->
  UploadServerCertificate
mkUploadServerCertificate
  pServerCertificateName_
  pPrivateKey_
  pCertificateBody_ =
    UploadServerCertificate'
      { serverCertificateName =
          pServerCertificateName_,
        privateKey = pPrivateKey_,
        path = Lude.Nothing,
        certificateBody = pCertificateBody_,
        certificateChain = Lude.Nothing
      }

-- | The name for the server certificate. Do not include the path in this value. The name of the certificate cannot contain any spaces.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'serverCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscServerCertificateName :: Lens.Lens' UploadServerCertificate Lude.Text
uscServerCertificateName = Lens.lens (serverCertificateName :: UploadServerCertificate -> Lude.Text) (\s a -> s {serverCertificateName = a} :: UploadServerCertificate)
{-# DEPRECATED uscServerCertificateName "Use generic-lens or generic-optics with 'serverCertificateName' instead." #-}

-- | The contents of the private key in PEM-encoded format.
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
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscPrivateKey :: Lens.Lens' UploadServerCertificate (Lude.Sensitive Lude.Text)
uscPrivateKey = Lens.lens (privateKey :: UploadServerCertificate -> Lude.Sensitive Lude.Text) (\s a -> s {privateKey = a} :: UploadServerCertificate)
{-# DEPRECATED uscPrivateKey "Use generic-lens or generic-optics with 'privateKey' instead." #-}

-- | The path for the server certificate. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscPath :: Lens.Lens' UploadServerCertificate (Lude.Maybe Lude.Text)
uscPath = Lens.lens (path :: UploadServerCertificate -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: UploadServerCertificate)
{-# DEPRECATED uscPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The contents of the public key certificate in PEM-encoded format.
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
uscCertificateBody :: Lens.Lens' UploadServerCertificate Lude.Text
uscCertificateBody = Lens.lens (certificateBody :: UploadServerCertificate -> Lude.Text) (\s a -> s {certificateBody = a} :: UploadServerCertificate)
{-# DEPRECATED uscCertificateBody "Use generic-lens or generic-optics with 'certificateBody' instead." #-}

-- | The contents of the certificate chain. This is typically a concatenation of the PEM-encoded public key certificates of the chain.
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
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscCertificateChain :: Lens.Lens' UploadServerCertificate (Lude.Maybe Lude.Text)
uscCertificateChain = Lens.lens (certificateChain :: UploadServerCertificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateChain = a} :: UploadServerCertificate)
{-# DEPRECATED uscCertificateChain "Use generic-lens or generic-optics with 'certificateChain' instead." #-}

instance Lude.AWSRequest UploadServerCertificate where
  type Rs UploadServerCertificate = UploadServerCertificateResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "UploadServerCertificateResult"
      ( \s h x ->
          UploadServerCertificateResponse'
            Lude.<$> (x Lude..@? "ServerCertificateMetadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UploadServerCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UploadServerCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery UploadServerCertificate where
  toQuery UploadServerCertificate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UploadServerCertificate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "ServerCertificateName" Lude.=: serverCertificateName,
        "PrivateKey" Lude.=: privateKey,
        "Path" Lude.=: path,
        "CertificateBody" Lude.=: certificateBody,
        "CertificateChain" Lude.=: certificateChain
      ]

-- | Contains the response to a successful 'UploadServerCertificate' request.
--
-- /See:/ 'mkUploadServerCertificateResponse' smart constructor.
data UploadServerCertificateResponse = UploadServerCertificateResponse'
  { -- | The meta information of the uploaded server certificate without its certificate body, certificate chain, and private key.
    serverCertificateMetadata :: Lude.Maybe ServerCertificateMetadata,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadServerCertificateResponse' with the minimum fields required to make a request.
--
-- * 'serverCertificateMetadata' - The meta information of the uploaded server certificate without its certificate body, certificate chain, and private key.
-- * 'responseStatus' - The response status code.
mkUploadServerCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UploadServerCertificateResponse
mkUploadServerCertificateResponse pResponseStatus_ =
  UploadServerCertificateResponse'
    { serverCertificateMetadata =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The meta information of the uploaded server certificate without its certificate body, certificate chain, and private key.
--
-- /Note:/ Consider using 'serverCertificateMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursServerCertificateMetadata :: Lens.Lens' UploadServerCertificateResponse (Lude.Maybe ServerCertificateMetadata)
ursServerCertificateMetadata = Lens.lens (serverCertificateMetadata :: UploadServerCertificateResponse -> Lude.Maybe ServerCertificateMetadata) (\s a -> s {serverCertificateMetadata = a} :: UploadServerCertificateResponse)
{-# DEPRECATED ursServerCertificateMetadata "Use generic-lens or generic-optics with 'serverCertificateMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UploadServerCertificateResponse Lude.Int
ursResponseStatus = Lens.lens (responseStatus :: UploadServerCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UploadServerCertificateResponse)
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
