{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ServerCertificateMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ServerCertificateMetadata
  ( ServerCertificateMetadata (..),

    -- * Smart constructor
    mkServerCertificateMetadata,

    -- * Lenses
    scmServerCertificateName,
    scmUploadDate,
    scmServerCertificateId,
    scmARN,
    scmPath,
    scmExpiration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a server certificate without its certificate body, certificate chain, and private key.
--
-- This data type is used as a response element in the 'UploadServerCertificate' and 'ListServerCertificates' operations.
--
-- /See:/ 'mkServerCertificateMetadata' smart constructor.
data ServerCertificateMetadata = ServerCertificateMetadata'
  { -- | The name that identifies the server certificate.
    serverCertificateName :: Lude.Text,
    -- | The date when the server certificate was uploaded.
    uploadDate :: Lude.Maybe Lude.DateTime,
    -- | The stable and unique string identifying the server certificate. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    serverCertificateId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) specifying the server certificate. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    arn :: Lude.Text,
    -- | The path to the server certificate. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    path :: Lude.Text,
    -- | The date on which the certificate is set to expire.
    expiration :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerCertificateMetadata' with the minimum fields required to make a request.
--
-- * 'serverCertificateName' - The name that identifies the server certificate.
-- * 'uploadDate' - The date when the server certificate was uploaded.
-- * 'serverCertificateId' - The stable and unique string identifying the server certificate. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'arn' - The Amazon Resource Name (ARN) specifying the server certificate. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'path' - The path to the server certificate. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'expiration' - The date on which the certificate is set to expire.
mkServerCertificateMetadata ::
  -- | 'serverCertificateName'
  Lude.Text ->
  -- | 'serverCertificateId'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  -- | 'path'
  Lude.Text ->
  ServerCertificateMetadata
mkServerCertificateMetadata
  pServerCertificateName_
  pServerCertificateId_
  pARN_
  pPath_ =
    ServerCertificateMetadata'
      { serverCertificateName =
          pServerCertificateName_,
        uploadDate = Lude.Nothing,
        serverCertificateId = pServerCertificateId_,
        arn = pARN_,
        path = pPath_,
        expiration = Lude.Nothing
      }

-- | The name that identifies the server certificate.
--
-- /Note:/ Consider using 'serverCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmServerCertificateName :: Lens.Lens' ServerCertificateMetadata Lude.Text
scmServerCertificateName = Lens.lens (serverCertificateName :: ServerCertificateMetadata -> Lude.Text) (\s a -> s {serverCertificateName = a} :: ServerCertificateMetadata)
{-# DEPRECATED scmServerCertificateName "Use generic-lens or generic-optics with 'serverCertificateName' instead." #-}

-- | The date when the server certificate was uploaded.
--
-- /Note:/ Consider using 'uploadDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmUploadDate :: Lens.Lens' ServerCertificateMetadata (Lude.Maybe Lude.DateTime)
scmUploadDate = Lens.lens (uploadDate :: ServerCertificateMetadata -> Lude.Maybe Lude.DateTime) (\s a -> s {uploadDate = a} :: ServerCertificateMetadata)
{-# DEPRECATED scmUploadDate "Use generic-lens or generic-optics with 'uploadDate' instead." #-}

-- | The stable and unique string identifying the server certificate. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'serverCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmServerCertificateId :: Lens.Lens' ServerCertificateMetadata Lude.Text
scmServerCertificateId = Lens.lens (serverCertificateId :: ServerCertificateMetadata -> Lude.Text) (\s a -> s {serverCertificateId = a} :: ServerCertificateMetadata)
{-# DEPRECATED scmServerCertificateId "Use generic-lens or generic-optics with 'serverCertificateId' instead." #-}

-- | The Amazon Resource Name (ARN) specifying the server certificate. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmARN :: Lens.Lens' ServerCertificateMetadata Lude.Text
scmARN = Lens.lens (arn :: ServerCertificateMetadata -> Lude.Text) (\s a -> s {arn = a} :: ServerCertificateMetadata)
{-# DEPRECATED scmARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The path to the server certificate. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmPath :: Lens.Lens' ServerCertificateMetadata Lude.Text
scmPath = Lens.lens (path :: ServerCertificateMetadata -> Lude.Text) (\s a -> s {path = a} :: ServerCertificateMetadata)
{-# DEPRECATED scmPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The date on which the certificate is set to expire.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmExpiration :: Lens.Lens' ServerCertificateMetadata (Lude.Maybe Lude.DateTime)
scmExpiration = Lens.lens (expiration :: ServerCertificateMetadata -> Lude.Maybe Lude.DateTime) (\s a -> s {expiration = a} :: ServerCertificateMetadata)
{-# DEPRECATED scmExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

instance Lude.FromXML ServerCertificateMetadata where
  parseXML x =
    ServerCertificateMetadata'
      Lude.<$> (x Lude..@ "ServerCertificateName")
      Lude.<*> (x Lude..@? "UploadDate")
      Lude.<*> (x Lude..@ "ServerCertificateId")
      Lude.<*> (x Lude..@ "Arn")
      Lude.<*> (x Lude..@ "Path")
      Lude.<*> (x Lude..@? "Expiration")
