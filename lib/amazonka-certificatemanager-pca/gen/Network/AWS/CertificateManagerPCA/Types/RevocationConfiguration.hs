-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration
  ( RevocationConfiguration (..),

    -- * Smart constructor
    mkRevocationConfiguration,

    -- * Lenses
    rcCrlConfiguration,
  )
where

import Network.AWS.CertificateManagerPCA.Types.CrlConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Certificate revocation information used by the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> and <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority> actions. Your private certificate authority (CA) can create and maintain a certificate revocation list (CRL). A CRL contains information about certificates revoked by your CA. For more information, see <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_RevokeCertificate.html RevokeCertificate> .
--
-- /See:/ 'mkRevocationConfiguration' smart constructor.
newtype RevocationConfiguration = RevocationConfiguration'
  { crlConfiguration ::
      Lude.Maybe CrlConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevocationConfiguration' with the minimum fields required to make a request.
--
-- * 'crlConfiguration' - Configuration of the certificate revocation list (CRL), if any, maintained by your private CA.
mkRevocationConfiguration ::
  RevocationConfiguration
mkRevocationConfiguration =
  RevocationConfiguration' {crlConfiguration = Lude.Nothing}

-- | Configuration of the certificate revocation list (CRL), if any, maintained by your private CA.
--
-- /Note:/ Consider using 'crlConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCrlConfiguration :: Lens.Lens' RevocationConfiguration (Lude.Maybe CrlConfiguration)
rcCrlConfiguration = Lens.lens (crlConfiguration :: RevocationConfiguration -> Lude.Maybe CrlConfiguration) (\s a -> s {crlConfiguration = a} :: RevocationConfiguration)
{-# DEPRECATED rcCrlConfiguration "Use generic-lens or generic-optics with 'crlConfiguration' instead." #-}

instance Lude.FromJSON RevocationConfiguration where
  parseJSON =
    Lude.withObject
      "RevocationConfiguration"
      ( \x ->
          RevocationConfiguration' Lude.<$> (x Lude..:? "CrlConfiguration")
      )

instance Lude.ToJSON RevocationConfiguration where
  toJSON RevocationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [("CrlConfiguration" Lude..=) Lude.<$> crlConfiguration]
      )
