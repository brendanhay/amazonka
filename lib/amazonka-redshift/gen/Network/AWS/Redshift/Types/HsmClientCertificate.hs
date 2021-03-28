{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.HsmClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.HsmClientCertificate
  ( HsmClientCertificate (..)
  -- * Smart constructor
  , mkHsmClientCertificate
  -- * Lenses
  , hccHsmClientCertificateIdentifier
  , hccHsmClientCertificatePublicKey
  , hccTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | Returns information about an HSM client certificate. The certificate is stored in a secure Hardware Storage Module (HSM), and used by the Amazon Redshift cluster to encrypt data files.
--
-- /See:/ 'mkHsmClientCertificate' smart constructor.
data HsmClientCertificate = HsmClientCertificate'
  { hsmClientCertificateIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the HSM client certificate.
  , hsmClientCertificatePublicKey :: Core.Maybe Core.Text
    -- ^ The public key that the Amazon Redshift cluster will use to connect to the HSM. You must register the public key in the HSM.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags for the HSM client certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HsmClientCertificate' value with any optional fields omitted.
mkHsmClientCertificate
    :: HsmClientCertificate
mkHsmClientCertificate
  = HsmClientCertificate'{hsmClientCertificateIdentifier =
                            Core.Nothing,
                          hsmClientCertificatePublicKey = Core.Nothing, tags = Core.Nothing}

-- | The identifier of the HSM client certificate.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccHsmClientCertificateIdentifier :: Lens.Lens' HsmClientCertificate (Core.Maybe Core.Text)
hccHsmClientCertificateIdentifier = Lens.field @"hsmClientCertificateIdentifier"
{-# INLINEABLE hccHsmClientCertificateIdentifier #-}
{-# DEPRECATED hsmClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead"  #-}

-- | The public key that the Amazon Redshift cluster will use to connect to the HSM. You must register the public key in the HSM.
--
-- /Note:/ Consider using 'hsmClientCertificatePublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccHsmClientCertificatePublicKey :: Lens.Lens' HsmClientCertificate (Core.Maybe Core.Text)
hccHsmClientCertificatePublicKey = Lens.field @"hsmClientCertificatePublicKey"
{-# INLINEABLE hccHsmClientCertificatePublicKey #-}
{-# DEPRECATED hsmClientCertificatePublicKey "Use generic-lens or generic-optics with 'hsmClientCertificatePublicKey' instead"  #-}

-- | The list of tags for the HSM client certificate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccTags :: Lens.Lens' HsmClientCertificate (Core.Maybe [Types.Tag])
hccTags = Lens.field @"tags"
{-# INLINEABLE hccTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML HsmClientCertificate where
        parseXML x
          = HsmClientCertificate' Core.<$>
              (x Core..@? "HsmClientCertificateIdentifier") Core.<*>
                x Core..@? "HsmClientCertificatePublicKey"
                Core.<*> x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag"
