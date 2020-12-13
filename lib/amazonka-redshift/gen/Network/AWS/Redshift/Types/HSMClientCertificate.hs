{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.HSMClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.HSMClientCertificate
  ( HSMClientCertificate (..),

    -- * Smart constructor
    mkHSMClientCertificate,

    -- * Lenses
    hccHSMClientCertificateIdentifier,
    hccHSMClientCertificatePublicKey,
    hccTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Returns information about an HSM client certificate. The certificate is stored in a secure Hardware Storage Module (HSM), and used by the Amazon Redshift cluster to encrypt data files.
--
-- /See:/ 'mkHSMClientCertificate' smart constructor.
data HSMClientCertificate = HSMClientCertificate'
  { -- | The identifier of the HSM client certificate.
    hsmClientCertificateIdentifier :: Lude.Maybe Lude.Text,
    -- | The public key that the Amazon Redshift cluster will use to connect to the HSM. You must register the public key in the HSM.
    hsmClientCertificatePublicKey :: Lude.Maybe Lude.Text,
    -- | The list of tags for the HSM client certificate.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HSMClientCertificate' with the minimum fields required to make a request.
--
-- * 'hsmClientCertificateIdentifier' - The identifier of the HSM client certificate.
-- * 'hsmClientCertificatePublicKey' - The public key that the Amazon Redshift cluster will use to connect to the HSM. You must register the public key in the HSM.
-- * 'tags' - The list of tags for the HSM client certificate.
mkHSMClientCertificate ::
  HSMClientCertificate
mkHSMClientCertificate =
  HSMClientCertificate'
    { hsmClientCertificateIdentifier =
        Lude.Nothing,
      hsmClientCertificatePublicKey = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The identifier of the HSM client certificate.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccHSMClientCertificateIdentifier :: Lens.Lens' HSMClientCertificate (Lude.Maybe Lude.Text)
hccHSMClientCertificateIdentifier = Lens.lens (hsmClientCertificateIdentifier :: HSMClientCertificate -> Lude.Maybe Lude.Text) (\s a -> s {hsmClientCertificateIdentifier = a} :: HSMClientCertificate)
{-# DEPRECATED hccHSMClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead." #-}

-- | The public key that the Amazon Redshift cluster will use to connect to the HSM. You must register the public key in the HSM.
--
-- /Note:/ Consider using 'hsmClientCertificatePublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccHSMClientCertificatePublicKey :: Lens.Lens' HSMClientCertificate (Lude.Maybe Lude.Text)
hccHSMClientCertificatePublicKey = Lens.lens (hsmClientCertificatePublicKey :: HSMClientCertificate -> Lude.Maybe Lude.Text) (\s a -> s {hsmClientCertificatePublicKey = a} :: HSMClientCertificate)
{-# DEPRECATED hccHSMClientCertificatePublicKey "Use generic-lens or generic-optics with 'hsmClientCertificatePublicKey' instead." #-}

-- | The list of tags for the HSM client certificate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccTags :: Lens.Lens' HSMClientCertificate (Lude.Maybe [Tag])
hccTags = Lens.lens (tags :: HSMClientCertificate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: HSMClientCertificate)
{-# DEPRECATED hccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML HSMClientCertificate where
  parseXML x =
    HSMClientCertificate'
      Lude.<$> (x Lude..@? "HsmClientCertificateIdentifier")
      Lude.<*> (x Lude..@? "HsmClientCertificatePublicKey")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
