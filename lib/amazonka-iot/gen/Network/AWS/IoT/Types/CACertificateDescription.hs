{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CACertificateDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CACertificateDescription
  ( CACertificateDescription (..)
  -- * Smart constructor
  , mkCACertificateDescription
  -- * Lenses
  , cacdAutoRegistrationStatus
  , cacdCertificateArn
  , cacdCertificateId
  , cacdCertificatePem
  , cacdCreationDate
  , cacdCustomerVersion
  , cacdGenerationId
  , cacdLastModifiedDate
  , cacdOwnedBy
  , cacdStatus
  , cacdValidity
  ) where

import qualified Network.AWS.IoT.Types.AutoRegistrationStatus as Types
import qualified Network.AWS.IoT.Types.CACertificateStatus as Types
import qualified Network.AWS.IoT.Types.CertificateArn as Types
import qualified Network.AWS.IoT.Types.CertificateId as Types
import qualified Network.AWS.IoT.Types.CertificatePem as Types
import qualified Network.AWS.IoT.Types.CertificateValidity as Types
import qualified Network.AWS.IoT.Types.GenerationId as Types
import qualified Network.AWS.IoT.Types.OwnedBy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a CA certificate.
--
-- /See:/ 'mkCACertificateDescription' smart constructor.
data CACertificateDescription = CACertificateDescription'
  { autoRegistrationStatus :: Core.Maybe Types.AutoRegistrationStatus
    -- ^ Whether the CA certificate configured for auto registration of device certificates. Valid values are "ENABLE" and "DISABLE"
  , certificateArn :: Core.Maybe Types.CertificateArn
    -- ^ The CA certificate ARN.
  , certificateId :: Core.Maybe Types.CertificateId
    -- ^ The CA certificate ID.
  , certificatePem :: Core.Maybe Types.CertificatePem
    -- ^ The CA certificate data, in PEM format.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the CA certificate was created.
  , customerVersion :: Core.Maybe Core.Natural
    -- ^ The customer version of the CA certificate.
  , generationId :: Core.Maybe Types.GenerationId
    -- ^ The generation ID of the CA certificate.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the CA certificate was last modified.
  , ownedBy :: Core.Maybe Types.OwnedBy
    -- ^ The owner of the CA certificate.
  , status :: Core.Maybe Types.CACertificateStatus
    -- ^ The status of a CA certificate.
  , validity :: Core.Maybe Types.CertificateValidity
    -- ^ When the CA certificate is valid.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CACertificateDescription' value with any optional fields omitted.
mkCACertificateDescription
    :: CACertificateDescription
mkCACertificateDescription
  = CACertificateDescription'{autoRegistrationStatus = Core.Nothing,
                              certificateArn = Core.Nothing, certificateId = Core.Nothing,
                              certificatePem = Core.Nothing, creationDate = Core.Nothing,
                              customerVersion = Core.Nothing, generationId = Core.Nothing,
                              lastModifiedDate = Core.Nothing, ownedBy = Core.Nothing,
                              status = Core.Nothing, validity = Core.Nothing}

-- | Whether the CA certificate configured for auto registration of device certificates. Valid values are "ENABLE" and "DISABLE"
--
-- /Note:/ Consider using 'autoRegistrationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdAutoRegistrationStatus :: Lens.Lens' CACertificateDescription (Core.Maybe Types.AutoRegistrationStatus)
cacdAutoRegistrationStatus = Lens.field @"autoRegistrationStatus"
{-# INLINEABLE cacdAutoRegistrationStatus #-}
{-# DEPRECATED autoRegistrationStatus "Use generic-lens or generic-optics with 'autoRegistrationStatus' instead"  #-}

-- | The CA certificate ARN.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdCertificateArn :: Lens.Lens' CACertificateDescription (Core.Maybe Types.CertificateArn)
cacdCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE cacdCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The CA certificate ID.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdCertificateId :: Lens.Lens' CACertificateDescription (Core.Maybe Types.CertificateId)
cacdCertificateId = Lens.field @"certificateId"
{-# INLINEABLE cacdCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The CA certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdCertificatePem :: Lens.Lens' CACertificateDescription (Core.Maybe Types.CertificatePem)
cacdCertificatePem = Lens.field @"certificatePem"
{-# INLINEABLE cacdCertificatePem #-}
{-# DEPRECATED certificatePem "Use generic-lens or generic-optics with 'certificatePem' instead"  #-}

-- | The date the CA certificate was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdCreationDate :: Lens.Lens' CACertificateDescription (Core.Maybe Core.NominalDiffTime)
cacdCreationDate = Lens.field @"creationDate"
{-# INLINEABLE cacdCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The customer version of the CA certificate.
--
-- /Note:/ Consider using 'customerVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdCustomerVersion :: Lens.Lens' CACertificateDescription (Core.Maybe Core.Natural)
cacdCustomerVersion = Lens.field @"customerVersion"
{-# INLINEABLE cacdCustomerVersion #-}
{-# DEPRECATED customerVersion "Use generic-lens or generic-optics with 'customerVersion' instead"  #-}

-- | The generation ID of the CA certificate.
--
-- /Note:/ Consider using 'generationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdGenerationId :: Lens.Lens' CACertificateDescription (Core.Maybe Types.GenerationId)
cacdGenerationId = Lens.field @"generationId"
{-# INLINEABLE cacdGenerationId #-}
{-# DEPRECATED generationId "Use generic-lens or generic-optics with 'generationId' instead"  #-}

-- | The date the CA certificate was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdLastModifiedDate :: Lens.Lens' CACertificateDescription (Core.Maybe Core.NominalDiffTime)
cacdLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE cacdLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The owner of the CA certificate.
--
-- /Note:/ Consider using 'ownedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdOwnedBy :: Lens.Lens' CACertificateDescription (Core.Maybe Types.OwnedBy)
cacdOwnedBy = Lens.field @"ownedBy"
{-# INLINEABLE cacdOwnedBy #-}
{-# DEPRECATED ownedBy "Use generic-lens or generic-optics with 'ownedBy' instead"  #-}

-- | The status of a CA certificate.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdStatus :: Lens.Lens' CACertificateDescription (Core.Maybe Types.CACertificateStatus)
cacdStatus = Lens.field @"status"
{-# INLINEABLE cacdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | When the CA certificate is valid.
--
-- /Note:/ Consider using 'validity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdValidity :: Lens.Lens' CACertificateDescription (Core.Maybe Types.CertificateValidity)
cacdValidity = Lens.field @"validity"
{-# INLINEABLE cacdValidity #-}
{-# DEPRECATED validity "Use generic-lens or generic-optics with 'validity' instead"  #-}

instance Core.FromJSON CACertificateDescription where
        parseJSON
          = Core.withObject "CACertificateDescription" Core.$
              \ x ->
                CACertificateDescription' Core.<$>
                  (x Core..:? "autoRegistrationStatus") Core.<*>
                    x Core..:? "certificateArn"
                    Core.<*> x Core..:? "certificateId"
                    Core.<*> x Core..:? "certificatePem"
                    Core.<*> x Core..:? "creationDate"
                    Core.<*> x Core..:? "customerVersion"
                    Core.<*> x Core..:? "generationId"
                    Core.<*> x Core..:? "lastModifiedDate"
                    Core.<*> x Core..:? "ownedBy"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "validity"
