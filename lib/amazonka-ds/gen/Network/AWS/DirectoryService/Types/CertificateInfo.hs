{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.CertificateInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.CertificateInfo
  ( CertificateInfo (..)
  -- * Smart constructor
  , mkCertificateInfo
  -- * Lenses
  , ciCertificateId
  , ciCommonName
  , ciExpiryDateTime
  , ciState
  ) where

import qualified Network.AWS.DirectoryService.Types.CertificateCN as Types
import qualified Network.AWS.DirectoryService.Types.CertificateId as Types
import qualified Network.AWS.DirectoryService.Types.CertificateState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains general information about a certificate.
--
-- /See:/ 'mkCertificateInfo' smart constructor.
data CertificateInfo = CertificateInfo'
  { certificateId :: Core.Maybe Types.CertificateId
    -- ^ The identifier of the certificate.
  , commonName :: Core.Maybe Types.CertificateCN
    -- ^ The common name for the certificate.
  , expiryDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the certificate will expire.
  , state :: Core.Maybe Types.CertificateState
    -- ^ The state of the certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CertificateInfo' value with any optional fields omitted.
mkCertificateInfo
    :: CertificateInfo
mkCertificateInfo
  = CertificateInfo'{certificateId = Core.Nothing,
                     commonName = Core.Nothing, expiryDateTime = Core.Nothing,
                     state = Core.Nothing}

-- | The identifier of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCertificateId :: Lens.Lens' CertificateInfo (Core.Maybe Types.CertificateId)
ciCertificateId = Lens.field @"certificateId"
{-# INLINEABLE ciCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The common name for the certificate.
--
-- /Note:/ Consider using 'commonName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCommonName :: Lens.Lens' CertificateInfo (Core.Maybe Types.CertificateCN)
ciCommonName = Lens.field @"commonName"
{-# INLINEABLE ciCommonName #-}
{-# DEPRECATED commonName "Use generic-lens or generic-optics with 'commonName' instead"  #-}

-- | The date and time when the certificate will expire.
--
-- /Note:/ Consider using 'expiryDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciExpiryDateTime :: Lens.Lens' CertificateInfo (Core.Maybe Core.NominalDiffTime)
ciExpiryDateTime = Lens.field @"expiryDateTime"
{-# INLINEABLE ciExpiryDateTime #-}
{-# DEPRECATED expiryDateTime "Use generic-lens or generic-optics with 'expiryDateTime' instead"  #-}

-- | The state of the certificate.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciState :: Lens.Lens' CertificateInfo (Core.Maybe Types.CertificateState)
ciState = Lens.field @"state"
{-# INLINEABLE ciState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON CertificateInfo where
        parseJSON
          = Core.withObject "CertificateInfo" Core.$
              \ x ->
                CertificateInfo' Core.<$>
                  (x Core..:? "CertificateId") Core.<*> x Core..:? "CommonName"
                    Core.<*> x Core..:? "ExpiryDateTime"
                    Core.<*> x Core..:? "State"
