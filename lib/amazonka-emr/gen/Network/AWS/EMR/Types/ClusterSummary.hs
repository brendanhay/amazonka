{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ClusterSummary
  ( ClusterSummary (..)
  -- * Smart constructor
  , mkClusterSummary
  -- * Lenses
  , csClusterArn
  , csId
  , csName
  , csNormalizedInstanceHours
  , csOutpostArn
  , csStatus
  ) where

import qualified Network.AWS.EMR.Types.ArnType as Types
import qualified Network.AWS.EMR.Types.ClusterId as Types
import qualified Network.AWS.EMR.Types.ClusterStatus as Types
import qualified Network.AWS.EMR.Types.OptionalArnType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The summary description of the cluster.
--
-- /See:/ 'mkClusterSummary' smart constructor.
data ClusterSummary = ClusterSummary'
  { clusterArn :: Core.Maybe Types.ArnType
    -- ^ The Amazon Resource Name of the cluster.
  , id :: Core.Maybe Types.ClusterId
    -- ^ The unique identifier for the cluster.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the cluster.
  , normalizedInstanceHours :: Core.Maybe Core.Int
    -- ^ An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
  , outpostArn :: Core.Maybe Types.OptionalArnType
    -- ^ The Amazon Resource Name (ARN) of the Outpost where the cluster is launched. 
  , status :: Core.Maybe Types.ClusterStatus
    -- ^ The details about the current status of the cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ClusterSummary' value with any optional fields omitted.
mkClusterSummary
    :: ClusterSummary
mkClusterSummary
  = ClusterSummary'{clusterArn = Core.Nothing, id = Core.Nothing,
                    name = Core.Nothing, normalizedInstanceHours = Core.Nothing,
                    outpostArn = Core.Nothing, status = Core.Nothing}

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csClusterArn :: Lens.Lens' ClusterSummary (Core.Maybe Types.ArnType)
csClusterArn = Lens.field @"clusterArn"
{-# INLINEABLE csClusterArn #-}
{-# DEPRECATED clusterArn "Use generic-lens or generic-optics with 'clusterArn' instead"  #-}

-- | The unique identifier for the cluster.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csId :: Lens.Lens' ClusterSummary (Core.Maybe Types.ClusterId)
csId = Lens.field @"id"
{-# INLINEABLE csId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the cluster.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' ClusterSummary (Core.Maybe Core.Text)
csName = Lens.field @"name"
{-# INLINEABLE csName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
--
-- /Note:/ Consider using 'normalizedInstanceHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNormalizedInstanceHours :: Lens.Lens' ClusterSummary (Core.Maybe Core.Int)
csNormalizedInstanceHours = Lens.field @"normalizedInstanceHours"
{-# INLINEABLE csNormalizedInstanceHours #-}
{-# DEPRECATED normalizedInstanceHours "Use generic-lens or generic-optics with 'normalizedInstanceHours' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Outpost where the cluster is launched. 
--
-- /Note:/ Consider using 'outpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csOutpostArn :: Lens.Lens' ClusterSummary (Core.Maybe Types.OptionalArnType)
csOutpostArn = Lens.field @"outpostArn"
{-# INLINEABLE csOutpostArn #-}
{-# DEPRECATED outpostArn "Use generic-lens or generic-optics with 'outpostArn' instead"  #-}

-- | The details about the current status of the cluster.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStatus :: Lens.Lens' ClusterSummary (Core.Maybe Types.ClusterStatus)
csStatus = Lens.field @"status"
{-# INLINEABLE csStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON ClusterSummary where
        parseJSON
          = Core.withObject "ClusterSummary" Core.$
              \ x ->
                ClusterSummary' Core.<$>
                  (x Core..:? "ClusterArn") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "Name"
                    Core.<*> x Core..:? "NormalizedInstanceHours"
                    Core.<*> x Core..:? "OutpostArn"
                    Core.<*> x Core..:? "Status"
