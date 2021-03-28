{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.ScalingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.ScalingParameters
  ( ScalingParameters (..)
  -- * Smart constructor
  , mkScalingParameters
  -- * Lenses
  , spDesiredInstanceType
  , spDesiredPartitionCount
  , spDesiredReplicationCount
  ) where

import qualified Network.AWS.CloudSearch.Types.PartitionInstanceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The desired instance type and desired number of replicas of each index partition.
--
-- /See:/ 'mkScalingParameters' smart constructor.
data ScalingParameters = ScalingParameters'
  { desiredInstanceType :: Core.Maybe Types.PartitionInstanceType
    -- ^ The instance type that you want to preconfigure for your domain. For example, @search.m1.small@ .
  , desiredPartitionCount :: Core.Maybe Core.Natural
    -- ^ The number of partitions you want to preconfigure for your domain. Only valid when you select @m2.2xlarge@ as the desired instance type.
  , desiredReplicationCount :: Core.Maybe Core.Natural
    -- ^ The number of replicas you want to preconfigure for each index partition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScalingParameters' value with any optional fields omitted.
mkScalingParameters
    :: ScalingParameters
mkScalingParameters
  = ScalingParameters'{desiredInstanceType = Core.Nothing,
                       desiredPartitionCount = Core.Nothing,
                       desiredReplicationCount = Core.Nothing}

-- | The instance type that you want to preconfigure for your domain. For example, @search.m1.small@ .
--
-- /Note:/ Consider using 'desiredInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spDesiredInstanceType :: Lens.Lens' ScalingParameters (Core.Maybe Types.PartitionInstanceType)
spDesiredInstanceType = Lens.field @"desiredInstanceType"
{-# INLINEABLE spDesiredInstanceType #-}
{-# DEPRECATED desiredInstanceType "Use generic-lens or generic-optics with 'desiredInstanceType' instead"  #-}

-- | The number of partitions you want to preconfigure for your domain. Only valid when you select @m2.2xlarge@ as the desired instance type.
--
-- /Note:/ Consider using 'desiredPartitionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spDesiredPartitionCount :: Lens.Lens' ScalingParameters (Core.Maybe Core.Natural)
spDesiredPartitionCount = Lens.field @"desiredPartitionCount"
{-# INLINEABLE spDesiredPartitionCount #-}
{-# DEPRECATED desiredPartitionCount "Use generic-lens or generic-optics with 'desiredPartitionCount' instead"  #-}

-- | The number of replicas you want to preconfigure for each index partition.
--
-- /Note:/ Consider using 'desiredReplicationCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spDesiredReplicationCount :: Lens.Lens' ScalingParameters (Core.Maybe Core.Natural)
spDesiredReplicationCount = Lens.field @"desiredReplicationCount"
{-# INLINEABLE spDesiredReplicationCount #-}
{-# DEPRECATED desiredReplicationCount "Use generic-lens or generic-optics with 'desiredReplicationCount' instead"  #-}

instance Core.ToQuery ScalingParameters where
        toQuery ScalingParameters{..}
          = Core.maybe Core.mempty (Core.toQueryPair "DesiredInstanceType")
              desiredInstanceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DesiredPartitionCount")
                desiredPartitionCount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DesiredReplicationCount")
                desiredReplicationCount

instance Core.FromXML ScalingParameters where
        parseXML x
          = ScalingParameters' Core.<$>
              (x Core..@? "DesiredInstanceType") Core.<*>
                x Core..@? "DesiredPartitionCount"
                Core.<*> x Core..@? "DesiredReplicationCount"
