{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CACertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CACertificate
  ( CACertificate (..)
  -- * Smart constructor
  , mkCACertificate
  -- * Lenses
  , cacCertificateArn
  , cacCertificateId
  , cacCreationDate
  , cacStatus
  ) where

import qualified Network.AWS.IoT.Types.CACertificateStatus as Types
import qualified Network.AWS.IoT.Types.CertificateArn as Types
import qualified Network.AWS.IoT.Types.CertificateId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A CA certificate.
--
-- /See:/ 'mkCACertificate' smart constructor.
data CACertificate = CACertificate'
  { certificateArn :: Core.Maybe Types.CertificateArn
    -- ^ The ARN of the CA certificate.
  , certificateId :: Core.Maybe Types.CertificateId
    -- ^ The ID of the CA certificate.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the CA certificate was created.
  , status :: Core.Maybe Types.CACertificateStatus
    -- ^ The status of the CA certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CACertificate' value with any optional fields omitted.
mkCACertificate
    :: CACertificate
mkCACertificate
  = CACertificate'{certificateArn = Core.Nothing,
                   certificateId = Core.Nothing, creationDate = Core.Nothing,
                   status = Core.Nothing}

-- | The ARN of the CA certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacCertificateArn :: Lens.Lens' CACertificate (Core.Maybe Types.CertificateArn)
cacCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE cacCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The ID of the CA certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacCertificateId :: Lens.Lens' CACertificate (Core.Maybe Types.CertificateId)
cacCertificateId = Lens.field @"certificateId"
{-# INLINEABLE cacCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The date the CA certificate was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacCreationDate :: Lens.Lens' CACertificate (Core.Maybe Core.NominalDiffTime)
cacCreationDate = Lens.field @"creationDate"
{-# INLINEABLE cacCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The status of the CA certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacStatus :: Lens.Lens' CACertificate (Core.Maybe Types.CACertificateStatus)
cacStatus = Lens.field @"status"
{-# INLINEABLE cacStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON CACertificate where
        parseJSON
          = Core.withObject "CACertificate" Core.$
              \ x ->
                CACertificate' Core.<$>
                  (x Core..:? "certificateArn") Core.<*> x Core..:? "certificateId"
                    Core.<*> x Core..:? "creationDate"
                    Core.<*> x Core..:? "status"
