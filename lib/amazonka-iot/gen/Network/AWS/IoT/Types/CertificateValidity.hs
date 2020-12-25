{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CertificateValidity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CertificateValidity
  ( CertificateValidity (..),

    -- * Smart constructor
    mkCertificateValidity,

    -- * Lenses
    cvNotAfter,
    cvNotBefore,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When the certificate is valid.
--
-- /See:/ 'mkCertificateValidity' smart constructor.
data CertificateValidity = CertificateValidity'
  { -- | The certificate is not valid after this date.
    notAfter :: Core.Maybe Core.NominalDiffTime,
    -- | The certificate is not valid before this date.
    notBefore :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CertificateValidity' value with any optional fields omitted.
mkCertificateValidity ::
  CertificateValidity
mkCertificateValidity =
  CertificateValidity'
    { notAfter = Core.Nothing,
      notBefore = Core.Nothing
    }

-- | The certificate is not valid after this date.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvNotAfter :: Lens.Lens' CertificateValidity (Core.Maybe Core.NominalDiffTime)
cvNotAfter = Lens.field @"notAfter"
{-# DEPRECATED cvNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

-- | The certificate is not valid before this date.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvNotBefore :: Lens.Lens' CertificateValidity (Core.Maybe Core.NominalDiffTime)
cvNotBefore = Lens.field @"notBefore"
{-# DEPRECATED cvNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

instance Core.FromJSON CertificateValidity where
  parseJSON =
    Core.withObject "CertificateValidity" Core.$
      \x ->
        CertificateValidity'
          Core.<$> (x Core..:? "notAfter") Core.<*> (x Core..:? "notBefore")
