-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.ScalingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ScalingParameters
  ( ScalingParameters (..),

    -- * Smart constructor
    mkScalingParameters,

    -- * Lenses
    spDesiredInstanceType,
    spDesiredReplicationCount,
    spDesiredPartitionCount,
  )
where

import Network.AWS.CloudSearch.Types.PartitionInstanceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The desired instance type and desired number of replicas of each index partition.
--
-- /See:/ 'mkScalingParameters' smart constructor.
data ScalingParameters = ScalingParameters'
  { desiredInstanceType ::
      Lude.Maybe PartitionInstanceType,
    desiredReplicationCount :: Lude.Maybe Lude.Natural,
    desiredPartitionCount :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingParameters' with the minimum fields required to make a request.
--
-- * 'desiredInstanceType' - The instance type that you want to preconfigure for your domain. For example, @search.m1.small@ .
-- * 'desiredPartitionCount' - The number of partitions you want to preconfigure for your domain. Only valid when you select @m2.2xlarge@ as the desired instance type.
-- * 'desiredReplicationCount' - The number of replicas you want to preconfigure for each index partition.
mkScalingParameters ::
  ScalingParameters
mkScalingParameters =
  ScalingParameters'
    { desiredInstanceType = Lude.Nothing,
      desiredReplicationCount = Lude.Nothing,
      desiredPartitionCount = Lude.Nothing
    }

-- | The instance type that you want to preconfigure for your domain. For example, @search.m1.small@ .
--
-- /Note:/ Consider using 'desiredInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spDesiredInstanceType :: Lens.Lens' ScalingParameters (Lude.Maybe PartitionInstanceType)
spDesiredInstanceType = Lens.lens (desiredInstanceType :: ScalingParameters -> Lude.Maybe PartitionInstanceType) (\s a -> s {desiredInstanceType = a} :: ScalingParameters)
{-# DEPRECATED spDesiredInstanceType "Use generic-lens or generic-optics with 'desiredInstanceType' instead." #-}

-- | The number of replicas you want to preconfigure for each index partition.
--
-- /Note:/ Consider using 'desiredReplicationCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spDesiredReplicationCount :: Lens.Lens' ScalingParameters (Lude.Maybe Lude.Natural)
spDesiredReplicationCount = Lens.lens (desiredReplicationCount :: ScalingParameters -> Lude.Maybe Lude.Natural) (\s a -> s {desiredReplicationCount = a} :: ScalingParameters)
{-# DEPRECATED spDesiredReplicationCount "Use generic-lens or generic-optics with 'desiredReplicationCount' instead." #-}

-- | The number of partitions you want to preconfigure for your domain. Only valid when you select @m2.2xlarge@ as the desired instance type.
--
-- /Note:/ Consider using 'desiredPartitionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spDesiredPartitionCount :: Lens.Lens' ScalingParameters (Lude.Maybe Lude.Natural)
spDesiredPartitionCount = Lens.lens (desiredPartitionCount :: ScalingParameters -> Lude.Maybe Lude.Natural) (\s a -> s {desiredPartitionCount = a} :: ScalingParameters)
{-# DEPRECATED spDesiredPartitionCount "Use generic-lens or generic-optics with 'desiredPartitionCount' instead." #-}

instance Lude.FromXML ScalingParameters where
  parseXML x =
    ScalingParameters'
      Lude.<$> (x Lude..@? "DesiredInstanceType")
      Lude.<*> (x Lude..@? "DesiredReplicationCount")
      Lude.<*> (x Lude..@? "DesiredPartitionCount")

instance Lude.ToQuery ScalingParameters where
  toQuery ScalingParameters' {..} =
    Lude.mconcat
      [ "DesiredInstanceType" Lude.=: desiredInstanceType,
        "DesiredReplicationCount" Lude.=: desiredReplicationCount,
        "DesiredPartitionCount" Lude.=: desiredPartitionCount
      ]
