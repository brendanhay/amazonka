{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.Certificate
  ( Certificate (..)
  -- * Smart constructor
  , mkCertificate
  -- * Lenses
  , cCertificateArn
  , cIsDefault
  ) where

import qualified Network.AWS.ELBv2.Types.CertificateArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an SSL server certificate.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { certificateArn :: Core.Maybe Types.CertificateArn
    -- ^ The Amazon Resource Name (ARN) of the certificate.
  , isDefault :: Core.Maybe Core.Bool
    -- ^ Indicates whether the certificate is the default certificate. Do not set this value when specifying a certificate as an input. This value is not included in the output when describing a listener, but is included when describing listener certificates.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Certificate' value with any optional fields omitted.
mkCertificate
    :: Certificate
mkCertificate
  = Certificate'{certificateArn = Core.Nothing,
                 isDefault = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateArn :: Lens.Lens' Certificate (Core.Maybe Types.CertificateArn)
cCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE cCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | Indicates whether the certificate is the default certificate. Do not set this value when specifying a certificate as an input. This value is not included in the output when describing a listener, but is included when describing listener certificates.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIsDefault :: Lens.Lens' Certificate (Core.Maybe Core.Bool)
cIsDefault = Lens.field @"isDefault"
{-# INLINEABLE cIsDefault #-}
{-# DEPRECATED isDefault "Use generic-lens or generic-optics with 'isDefault' instead"  #-}

instance Core.ToQuery Certificate where
        toQuery Certificate{..}
          = Core.maybe Core.mempty (Core.toQueryPair "CertificateArn")
              certificateArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IsDefault") isDefault

instance Core.FromXML Certificate where
        parseXML x
          = Certificate' Core.<$>
              (x Core..@? "CertificateArn") Core.<*> x Core..@? "IsDefault"
