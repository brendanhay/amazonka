{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.InstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.InstanceDetails
  ( InstanceDetails (..)
  -- * Smart constructor
  , mkInstanceDetails
  -- * Lenses
  , idEC2InstanceDetails
  , idESInstanceDetails
  , idElastiCacheInstanceDetails
  , idRDSInstanceDetails
  , idRedshiftInstanceDetails
  ) where

import qualified Network.AWS.CostExplorer.Types.EC2InstanceDetails as Types
import qualified Network.AWS.CostExplorer.Types.ESInstanceDetails as Types
import qualified Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails as Types
import qualified Network.AWS.CostExplorer.Types.RDSInstanceDetails as Types
import qualified Network.AWS.CostExplorer.Types.RedshiftInstanceDetails as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the instances that AWS recommends that you purchase.
--
-- /See:/ 'mkInstanceDetails' smart constructor.
data InstanceDetails = InstanceDetails'
  { eC2InstanceDetails :: Core.Maybe Types.EC2InstanceDetails
    -- ^ The Amazon EC2 instances that AWS recommends that you purchase.
  , eSInstanceDetails :: Core.Maybe Types.ESInstanceDetails
    -- ^ The Amazon ES instances that AWS recommends that you purchase.
  , elastiCacheInstanceDetails :: Core.Maybe Types.ElastiCacheInstanceDetails
    -- ^ The ElastiCache instances that AWS recommends that you purchase.
  , rDSInstanceDetails :: Core.Maybe Types.RDSInstanceDetails
    -- ^ The Amazon RDS instances that AWS recommends that you purchase.
  , redshiftInstanceDetails :: Core.Maybe Types.RedshiftInstanceDetails
    -- ^ The Amazon Redshift instances that AWS recommends that you purchase.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceDetails' value with any optional fields omitted.
mkInstanceDetails
    :: InstanceDetails
mkInstanceDetails
  = InstanceDetails'{eC2InstanceDetails = Core.Nothing,
                     eSInstanceDetails = Core.Nothing,
                     elastiCacheInstanceDetails = Core.Nothing,
                     rDSInstanceDetails = Core.Nothing,
                     redshiftInstanceDetails = Core.Nothing}

-- | The Amazon EC2 instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'eC2InstanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idEC2InstanceDetails :: Lens.Lens' InstanceDetails (Core.Maybe Types.EC2InstanceDetails)
idEC2InstanceDetails = Lens.field @"eC2InstanceDetails"
{-# INLINEABLE idEC2InstanceDetails #-}
{-# DEPRECATED eC2InstanceDetails "Use generic-lens or generic-optics with 'eC2InstanceDetails' instead"  #-}

-- | The Amazon ES instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'eSInstanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idESInstanceDetails :: Lens.Lens' InstanceDetails (Core.Maybe Types.ESInstanceDetails)
idESInstanceDetails = Lens.field @"eSInstanceDetails"
{-# INLINEABLE idESInstanceDetails #-}
{-# DEPRECATED eSInstanceDetails "Use generic-lens or generic-optics with 'eSInstanceDetails' instead"  #-}

-- | The ElastiCache instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'elastiCacheInstanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idElastiCacheInstanceDetails :: Lens.Lens' InstanceDetails (Core.Maybe Types.ElastiCacheInstanceDetails)
idElastiCacheInstanceDetails = Lens.field @"elastiCacheInstanceDetails"
{-# INLINEABLE idElastiCacheInstanceDetails #-}
{-# DEPRECATED elastiCacheInstanceDetails "Use generic-lens or generic-optics with 'elastiCacheInstanceDetails' instead"  #-}

-- | The Amazon RDS instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'rDSInstanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idRDSInstanceDetails :: Lens.Lens' InstanceDetails (Core.Maybe Types.RDSInstanceDetails)
idRDSInstanceDetails = Lens.field @"rDSInstanceDetails"
{-# INLINEABLE idRDSInstanceDetails #-}
{-# DEPRECATED rDSInstanceDetails "Use generic-lens or generic-optics with 'rDSInstanceDetails' instead"  #-}

-- | The Amazon Redshift instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'redshiftInstanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idRedshiftInstanceDetails :: Lens.Lens' InstanceDetails (Core.Maybe Types.RedshiftInstanceDetails)
idRedshiftInstanceDetails = Lens.field @"redshiftInstanceDetails"
{-# INLINEABLE idRedshiftInstanceDetails #-}
{-# DEPRECATED redshiftInstanceDetails "Use generic-lens or generic-optics with 'redshiftInstanceDetails' instead"  #-}

instance Core.FromJSON InstanceDetails where
        parseJSON
          = Core.withObject "InstanceDetails" Core.$
              \ x ->
                InstanceDetails' Core.<$>
                  (x Core..:? "EC2InstanceDetails") Core.<*>
                    x Core..:? "ESInstanceDetails"
                    Core.<*> x Core..:? "ElastiCacheInstanceDetails"
                    Core.<*> x Core..:? "RDSInstanceDetails"
                    Core.<*> x Core..:? "RedshiftInstanceDetails"
