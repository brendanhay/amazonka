{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.Certificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSMv2.Types.Certificates
  ( Certificates (..)
  -- * Smart constructor
  , mkCertificates
  -- * Lenses
  , cAwsHardwareCertificate
  , cClusterCertificate
  , cClusterCsr
  , cHsmCertificate
  , cManufacturerHardwareCertificate
  ) where

import qualified Network.AWS.CloudHSMv2.Types.AwsHardwareCertificate as Types
import qualified Network.AWS.CloudHSMv2.Types.ClusterCertificate as Types
import qualified Network.AWS.CloudHSMv2.Types.ClusterCsr as Types
import qualified Network.AWS.CloudHSMv2.Types.HsmCertificate as Types
import qualified Network.AWS.CloudHSMv2.Types.ManufacturerHardwareCertificate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains one or more certificates or a certificate signing request (CSR).
--
-- /See:/ 'mkCertificates' smart constructor.
data Certificates = Certificates'
  { awsHardwareCertificate :: Core.Maybe Types.AwsHardwareCertificate
    -- ^ The HSM hardware certificate issued (signed) by AWS CloudHSM.
  , clusterCertificate :: Core.Maybe Types.ClusterCertificate
    -- ^ The cluster certificate issued (signed) by the issuing certificate authority (CA) of the cluster's owner.
  , clusterCsr :: Core.Maybe Types.ClusterCsr
    -- ^ The cluster's certificate signing request (CSR). The CSR exists only when the cluster's state is @UNINITIALIZED@ .
  , hsmCertificate :: Core.Maybe Types.HsmCertificate
    -- ^ The HSM certificate issued (signed) by the HSM hardware.
  , manufacturerHardwareCertificate :: Core.Maybe Types.ManufacturerHardwareCertificate
    -- ^ The HSM hardware certificate issued (signed) by the hardware manufacturer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Certificates' value with any optional fields omitted.
mkCertificates
    :: Certificates
mkCertificates
  = Certificates'{awsHardwareCertificate = Core.Nothing,
                  clusterCertificate = Core.Nothing, clusterCsr = Core.Nothing,
                  hsmCertificate = Core.Nothing,
                  manufacturerHardwareCertificate = Core.Nothing}

-- | The HSM hardware certificate issued (signed) by AWS CloudHSM.
--
-- /Note:/ Consider using 'awsHardwareCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAwsHardwareCertificate :: Lens.Lens' Certificates (Core.Maybe Types.AwsHardwareCertificate)
cAwsHardwareCertificate = Lens.field @"awsHardwareCertificate"
{-# INLINEABLE cAwsHardwareCertificate #-}
{-# DEPRECATED awsHardwareCertificate "Use generic-lens or generic-optics with 'awsHardwareCertificate' instead"  #-}

-- | The cluster certificate issued (signed) by the issuing certificate authority (CA) of the cluster's owner.
--
-- /Note:/ Consider using 'clusterCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterCertificate :: Lens.Lens' Certificates (Core.Maybe Types.ClusterCertificate)
cClusterCertificate = Lens.field @"clusterCertificate"
{-# INLINEABLE cClusterCertificate #-}
{-# DEPRECATED clusterCertificate "Use generic-lens or generic-optics with 'clusterCertificate' instead"  #-}

-- | The cluster's certificate signing request (CSR). The CSR exists only when the cluster's state is @UNINITIALIZED@ .
--
-- /Note:/ Consider using 'clusterCsr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterCsr :: Lens.Lens' Certificates (Core.Maybe Types.ClusterCsr)
cClusterCsr = Lens.field @"clusterCsr"
{-# INLINEABLE cClusterCsr #-}
{-# DEPRECATED clusterCsr "Use generic-lens or generic-optics with 'clusterCsr' instead"  #-}

-- | The HSM certificate issued (signed) by the HSM hardware.
--
-- /Note:/ Consider using 'hsmCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHsmCertificate :: Lens.Lens' Certificates (Core.Maybe Types.HsmCertificate)
cHsmCertificate = Lens.field @"hsmCertificate"
{-# INLINEABLE cHsmCertificate #-}
{-# DEPRECATED hsmCertificate "Use generic-lens or generic-optics with 'hsmCertificate' instead"  #-}

-- | The HSM hardware certificate issued (signed) by the hardware manufacturer.
--
-- /Note:/ Consider using 'manufacturerHardwareCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cManufacturerHardwareCertificate :: Lens.Lens' Certificates (Core.Maybe Types.ManufacturerHardwareCertificate)
cManufacturerHardwareCertificate = Lens.field @"manufacturerHardwareCertificate"
{-# INLINEABLE cManufacturerHardwareCertificate #-}
{-# DEPRECATED manufacturerHardwareCertificate "Use generic-lens or generic-optics with 'manufacturerHardwareCertificate' instead"  #-}

instance Core.FromJSON Certificates where
        parseJSON
          = Core.withObject "Certificates" Core.$
              \ x ->
                Certificates' Core.<$>
                  (x Core..:? "AwsHardwareCertificate") Core.<*>
                    x Core..:? "ClusterCertificate"
                    Core.<*> x Core..:? "ClusterCsr"
                    Core.<*> x Core..:? "HsmCertificate"
                    Core.<*> x Core..:? "ManufacturerHardwareCertificate"
