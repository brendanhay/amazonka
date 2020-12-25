{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Certificate
  ( Certificate (..),

    -- * Smart constructor
    mkCertificate,

    -- * Lenses
    cCertificateArn,
    cCertificateIdentifier,
    cCertificateType,
    cCustomerOverride,
    cCustomerOverrideValidTill,
    cThumbprint,
    cValidFrom,
    cValidTill,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | A CA certificate for an AWS account.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The Amazon Resource Name (ARN) for the certificate.
    certificateArn :: Core.Maybe Types.String,
    -- | The unique key that identifies a certificate.
    certificateIdentifier :: Core.Maybe Types.String,
    -- | The type of the certificate.
    certificateType :: Core.Maybe Types.String,
    -- | Whether there is an override for the default certificate identifier.
    customerOverride :: Core.Maybe Core.Bool,
    -- | If there is an override for the default certificate identifier, when the override expires.
    customerOverrideValidTill :: Core.Maybe Core.UTCTime,
    -- | The thumbprint of the certificate.
    thumbprint :: Core.Maybe Types.String,
    -- | The starting date from which the certificate is valid.
    validFrom :: Core.Maybe Core.UTCTime,
    -- | The final date that the certificate continues to be valid.
    validTill :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Certificate' value with any optional fields omitted.
mkCertificate ::
  Certificate
mkCertificate =
  Certificate'
    { certificateArn = Core.Nothing,
      certificateIdentifier = Core.Nothing,
      certificateType = Core.Nothing,
      customerOverride = Core.Nothing,
      customerOverrideValidTill = Core.Nothing,
      thumbprint = Core.Nothing,
      validFrom = Core.Nothing,
      validTill = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for the certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateArn :: Lens.Lens' Certificate (Core.Maybe Types.String)
cCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED cCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | The unique key that identifies a certificate.
--
-- /Note:/ Consider using 'certificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateIdentifier :: Lens.Lens' Certificate (Core.Maybe Types.String)
cCertificateIdentifier = Lens.field @"certificateIdentifier"
{-# DEPRECATED cCertificateIdentifier "Use generic-lens or generic-optics with 'certificateIdentifier' instead." #-}

-- | The type of the certificate.
--
-- /Note:/ Consider using 'certificateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateType :: Lens.Lens' Certificate (Core.Maybe Types.String)
cCertificateType = Lens.field @"certificateType"
{-# DEPRECATED cCertificateType "Use generic-lens or generic-optics with 'certificateType' instead." #-}

-- | Whether there is an override for the default certificate identifier.
--
-- /Note:/ Consider using 'customerOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCustomerOverride :: Lens.Lens' Certificate (Core.Maybe Core.Bool)
cCustomerOverride = Lens.field @"customerOverride"
{-# DEPRECATED cCustomerOverride "Use generic-lens or generic-optics with 'customerOverride' instead." #-}

-- | If there is an override for the default certificate identifier, when the override expires.
--
-- /Note:/ Consider using 'customerOverrideValidTill' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCustomerOverrideValidTill :: Lens.Lens' Certificate (Core.Maybe Core.UTCTime)
cCustomerOverrideValidTill = Lens.field @"customerOverrideValidTill"
{-# DEPRECATED cCustomerOverrideValidTill "Use generic-lens or generic-optics with 'customerOverrideValidTill' instead." #-}

-- | The thumbprint of the certificate.
--
-- /Note:/ Consider using 'thumbprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cThumbprint :: Lens.Lens' Certificate (Core.Maybe Types.String)
cThumbprint = Lens.field @"thumbprint"
{-# DEPRECATED cThumbprint "Use generic-lens or generic-optics with 'thumbprint' instead." #-}

-- | The starting date from which the certificate is valid.
--
-- /Note:/ Consider using 'validFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValidFrom :: Lens.Lens' Certificate (Core.Maybe Core.UTCTime)
cValidFrom = Lens.field @"validFrom"
{-# DEPRECATED cValidFrom "Use generic-lens or generic-optics with 'validFrom' instead." #-}

-- | The final date that the certificate continues to be valid.
--
-- /Note:/ Consider using 'validTill' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValidTill :: Lens.Lens' Certificate (Core.Maybe Core.UTCTime)
cValidTill = Lens.field @"validTill"
{-# DEPRECATED cValidTill "Use generic-lens or generic-optics with 'validTill' instead." #-}

instance Core.FromXML Certificate where
  parseXML x =
    Certificate'
      Core.<$> (x Core..@? "CertificateArn")
      Core.<*> (x Core..@? "CertificateIdentifier")
      Core.<*> (x Core..@? "CertificateType")
      Core.<*> (x Core..@? "CustomerOverride")
      Core.<*> (x Core..@? "CustomerOverrideValidTill")
      Core.<*> (x Core..@? "Thumbprint")
      Core.<*> (x Core..@? "ValidFrom")
      Core.<*> (x Core..@? "ValidTill")
