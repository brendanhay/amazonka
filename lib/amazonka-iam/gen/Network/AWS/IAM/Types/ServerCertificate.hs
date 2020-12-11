-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ServerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ServerCertificate
  ( ServerCertificate (..),

    -- * Smart constructor
    mkServerCertificate,

    -- * Lenses
    sCertificateChain,
    sServerCertificateMetadata,
    sCertificateBody,
  )
where

import Network.AWS.IAM.Types.ServerCertificateMetadata
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a server certificate.
--
-- This data type is used as a response element in the 'GetServerCertificate' operation.
--
-- /See:/ 'mkServerCertificate' smart constructor.
data ServerCertificate = ServerCertificate'
  { certificateChain ::
      Lude.Maybe Lude.Text,
    serverCertificateMetadata :: ServerCertificateMetadata,
    certificateBody :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerCertificate' with the minimum fields required to make a request.
--
-- * 'certificateBody' - The contents of the public key certificate.
-- * 'certificateChain' - The contents of the public key certificate chain.
-- * 'serverCertificateMetadata' - The meta information of the server certificate, such as its name, path, ID, and ARN.
mkServerCertificate ::
  -- | 'serverCertificateMetadata'
  ServerCertificateMetadata ->
  -- | 'certificateBody'
  Lude.Text ->
  ServerCertificate
mkServerCertificate pServerCertificateMetadata_ pCertificateBody_ =
  ServerCertificate'
    { certificateChain = Lude.Nothing,
      serverCertificateMetadata = pServerCertificateMetadata_,
      certificateBody = pCertificateBody_
    }

-- | The contents of the public key certificate chain.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCertificateChain :: Lens.Lens' ServerCertificate (Lude.Maybe Lude.Text)
sCertificateChain = Lens.lens (certificateChain :: ServerCertificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateChain = a} :: ServerCertificate)
{-# DEPRECATED sCertificateChain "Use generic-lens or generic-optics with 'certificateChain' instead." #-}

-- | The meta information of the server certificate, such as its name, path, ID, and ARN.
--
-- /Note:/ Consider using 'serverCertificateMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sServerCertificateMetadata :: Lens.Lens' ServerCertificate ServerCertificateMetadata
sServerCertificateMetadata = Lens.lens (serverCertificateMetadata :: ServerCertificate -> ServerCertificateMetadata) (\s a -> s {serverCertificateMetadata = a} :: ServerCertificate)
{-# DEPRECATED sServerCertificateMetadata "Use generic-lens or generic-optics with 'serverCertificateMetadata' instead." #-}

-- | The contents of the public key certificate.
--
-- /Note:/ Consider using 'certificateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCertificateBody :: Lens.Lens' ServerCertificate Lude.Text
sCertificateBody = Lens.lens (certificateBody :: ServerCertificate -> Lude.Text) (\s a -> s {certificateBody = a} :: ServerCertificate)
{-# DEPRECATED sCertificateBody "Use generic-lens or generic-optics with 'certificateBody' instead." #-}

instance Lude.FromXML ServerCertificate where
  parseXML x =
    ServerCertificate'
      Lude.<$> (x Lude..@? "CertificateChain")
      Lude.<*> (x Lude..@ "ServerCertificateMetadata")
      Lude.<*> (x Lude..@ "CertificateBody")
