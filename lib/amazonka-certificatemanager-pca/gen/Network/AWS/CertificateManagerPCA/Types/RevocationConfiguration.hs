{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration
  ( RevocationConfiguration (..)
  -- * Smart constructor
  , mkRevocationConfiguration
  -- * Lenses
  , rcCrlConfiguration
  ) where

import qualified Network.AWS.CertificateManagerPCA.Types.CrlConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Certificate revocation information used by the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> and <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority> actions. Your private certificate authority (CA) can create and maintain a certificate revocation list (CRL). A CRL contains information about certificates revoked by your CA. For more information, see <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_RevokeCertificate.html RevokeCertificate> .
--
-- /See:/ 'mkRevocationConfiguration' smart constructor.
newtype RevocationConfiguration = RevocationConfiguration'
  { crlConfiguration :: Core.Maybe Types.CrlConfiguration
    -- ^ Configuration of the certificate revocation list (CRL), if any, maintained by your private CA.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RevocationConfiguration' value with any optional fields omitted.
mkRevocationConfiguration
    :: RevocationConfiguration
mkRevocationConfiguration
  = RevocationConfiguration'{crlConfiguration = Core.Nothing}

-- | Configuration of the certificate revocation list (CRL), if any, maintained by your private CA.
--
-- /Note:/ Consider using 'crlConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCrlConfiguration :: Lens.Lens' RevocationConfiguration (Core.Maybe Types.CrlConfiguration)
rcCrlConfiguration = Lens.field @"crlConfiguration"
{-# INLINEABLE rcCrlConfiguration #-}
{-# DEPRECATED crlConfiguration "Use generic-lens or generic-optics with 'crlConfiguration' instead"  #-}

instance Core.FromJSON RevocationConfiguration where
        toJSON RevocationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("CrlConfiguration" Core..=) Core.<$> crlConfiguration])

instance Core.FromJSON RevocationConfiguration where
        parseJSON
          = Core.withObject "RevocationConfiguration" Core.$
              \ x ->
                RevocationConfiguration' Core.<$> (x Core..:? "CrlConfiguration")
