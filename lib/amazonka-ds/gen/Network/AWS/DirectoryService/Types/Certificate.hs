{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.Certificate
  ( Certificate (..)
  -- * Smart constructor
  , mkCertificate
  -- * Lenses
  , cCertificateId
  , cCommonName
  , cExpiryDateTime
  , cRegisteredDateTime
  , cState
  , cStateReason
  ) where

import qualified Network.AWS.DirectoryService.Types.CertificateCN as Types
import qualified Network.AWS.DirectoryService.Types.CertificateId as Types
import qualified Network.AWS.DirectoryService.Types.CertificateState as Types
import qualified Network.AWS.DirectoryService.Types.CertificateStateReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the certificate.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { certificateId :: Core.Maybe Types.CertificateId
    -- ^ The identifier of the certificate.
  , commonName :: Core.Maybe Types.CertificateCN
    -- ^ The common name for the certificate.
  , expiryDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the certificate will expire.
  , registeredDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the certificate was registered.
  , state :: Core.Maybe Types.CertificateState
    -- ^ The state of the certificate.
  , stateReason :: Core.Maybe Types.CertificateStateReason
    -- ^ Describes a state change for the certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Certificate' value with any optional fields omitted.
mkCertificate
    :: Certificate
mkCertificate
  = Certificate'{certificateId = Core.Nothing,
                 commonName = Core.Nothing, expiryDateTime = Core.Nothing,
                 registeredDateTime = Core.Nothing, state = Core.Nothing,
                 stateReason = Core.Nothing}

-- | The identifier of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateId :: Lens.Lens' Certificate (Core.Maybe Types.CertificateId)
cCertificateId = Lens.field @"certificateId"
{-# INLINEABLE cCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The common name for the certificate.
--
-- /Note:/ Consider using 'commonName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommonName :: Lens.Lens' Certificate (Core.Maybe Types.CertificateCN)
cCommonName = Lens.field @"commonName"
{-# INLINEABLE cCommonName #-}
{-# DEPRECATED commonName "Use generic-lens or generic-optics with 'commonName' instead"  #-}

-- | The date and time when the certificate will expire.
--
-- /Note:/ Consider using 'expiryDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpiryDateTime :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cExpiryDateTime = Lens.field @"expiryDateTime"
{-# INLINEABLE cExpiryDateTime #-}
{-# DEPRECATED expiryDateTime "Use generic-lens or generic-optics with 'expiryDateTime' instead"  #-}

-- | The date and time that the certificate was registered.
--
-- /Note:/ Consider using 'registeredDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRegisteredDateTime :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cRegisteredDateTime = Lens.field @"registeredDateTime"
{-# INLINEABLE cRegisteredDateTime #-}
{-# DEPRECATED registeredDateTime "Use generic-lens or generic-optics with 'registeredDateTime' instead"  #-}

-- | The state of the certificate.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cState :: Lens.Lens' Certificate (Core.Maybe Types.CertificateState)
cState = Lens.field @"state"
{-# INLINEABLE cState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Describes a state change for the certificate.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStateReason :: Lens.Lens' Certificate (Core.Maybe Types.CertificateStateReason)
cStateReason = Lens.field @"stateReason"
{-# INLINEABLE cStateReason #-}
{-# DEPRECATED stateReason "Use generic-lens or generic-optics with 'stateReason' instead"  #-}

instance Core.FromJSON Certificate where
        parseJSON
          = Core.withObject "Certificate" Core.$
              \ x ->
                Certificate' Core.<$>
                  (x Core..:? "CertificateId") Core.<*> x Core..:? "CommonName"
                    Core.<*> x Core..:? "ExpiryDateTime"
                    Core.<*> x Core..:? "RegisteredDateTime"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "StateReason"
