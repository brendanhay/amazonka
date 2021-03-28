{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.Certificate
  ( Certificate (..)
  -- * Smart constructor
  , mkCertificate
  -- * Lenses
  , cCertificateArn
  , cCertificateId
  , cCertificateMode
  , cCreationDate
  , cStatus
  ) where

import qualified Network.AWS.IoT.Types.CertificateArn as Types
import qualified Network.AWS.IoT.Types.CertificateId as Types
import qualified Network.AWS.IoT.Types.CertificateMode as Types
import qualified Network.AWS.IoT.Types.CertificateStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a certificate.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { certificateArn :: Core.Maybe Types.CertificateArn
    -- ^ The ARN of the certificate.
  , certificateId :: Core.Maybe Types.CertificateId
    -- ^ The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
  , certificateMode :: Core.Maybe Types.CertificateMode
    -- ^ The mode of the certificate.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the certificate was created.
  , status :: Core.Maybe Types.CertificateStatus
    -- ^ The status of the certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Certificate' value with any optional fields omitted.
mkCertificate
    :: Certificate
mkCertificate
  = Certificate'{certificateArn = Core.Nothing,
                 certificateId = Core.Nothing, certificateMode = Core.Nothing,
                 creationDate = Core.Nothing, status = Core.Nothing}

-- | The ARN of the certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateArn :: Lens.Lens' Certificate (Core.Maybe Types.CertificateArn)
cCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE cCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateId :: Lens.Lens' Certificate (Core.Maybe Types.CertificateId)
cCertificateId = Lens.field @"certificateId"
{-# INLINEABLE cCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The mode of the certificate.
--
-- /Note:/ Consider using 'certificateMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateMode :: Lens.Lens' Certificate (Core.Maybe Types.CertificateMode)
cCertificateMode = Lens.field @"certificateMode"
{-# INLINEABLE cCertificateMode #-}
{-# DEPRECATED certificateMode "Use generic-lens or generic-optics with 'certificateMode' instead"  #-}

-- | The date and time the certificate was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreationDate :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cCreationDate = Lens.field @"creationDate"
{-# INLINEABLE cCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The status of the certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Certificate (Core.Maybe Types.CertificateStatus)
cStatus = Lens.field @"status"
{-# INLINEABLE cStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON Certificate where
        parseJSON
          = Core.withObject "Certificate" Core.$
              \ x ->
                Certificate' Core.<$>
                  (x Core..:? "certificateArn") Core.<*> x Core..:? "certificateId"
                    Core.<*> x Core..:? "certificateMode"
                    Core.<*> x Core..:? "creationDate"
                    Core.<*> x Core..:? "status"
