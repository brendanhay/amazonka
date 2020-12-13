{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterSummary
  ( ClusterSummary (..),

    -- * Smart constructor
    mkClusterSummary,

    -- * Lenses
    csStatus,
    csClusterARN,
    csOutpostARN,
    csNormalizedInstanceHours,
    csName,
    csId,
  )
where

import Network.AWS.EMR.Types.ClusterStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The summary description of the cluster.
--
-- /See:/ 'mkClusterSummary' smart constructor.
data ClusterSummary = ClusterSummary'
  { -- | The details about the current status of the cluster.
    status :: Lude.Maybe ClusterStatus,
    -- | The Amazon Resource Name of the cluster.
    clusterARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost where the cluster is launched.
    outpostARN :: Lude.Maybe Lude.Text,
    -- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
    normalizedInstanceHours :: Lude.Maybe Lude.Int,
    -- | The name of the cluster.
    name :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the cluster.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterSummary' with the minimum fields required to make a request.
--
-- * 'status' - The details about the current status of the cluster.
-- * 'clusterARN' - The Amazon Resource Name of the cluster.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost where the cluster is launched.
-- * 'normalizedInstanceHours' - An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
-- * 'name' - The name of the cluster.
-- * 'id' - The unique identifier for the cluster.
mkClusterSummary ::
  ClusterSummary
mkClusterSummary =
  ClusterSummary'
    { status = Lude.Nothing,
      clusterARN = Lude.Nothing,
      outpostARN = Lude.Nothing,
      normalizedInstanceHours = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The details about the current status of the cluster.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStatus :: Lens.Lens' ClusterSummary (Lude.Maybe ClusterStatus)
csStatus = Lens.lens (status :: ClusterSummary -> Lude.Maybe ClusterStatus) (\s a -> s {status = a} :: ClusterSummary)
{-# DEPRECATED csStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csClusterARN :: Lens.Lens' ClusterSummary (Lude.Maybe Lude.Text)
csClusterARN = Lens.lens (clusterARN :: ClusterSummary -> Lude.Maybe Lude.Text) (\s a -> s {clusterARN = a} :: ClusterSummary)
{-# DEPRECATED csClusterARN "Use generic-lens or generic-optics with 'clusterARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost where the cluster is launched.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csOutpostARN :: Lens.Lens' ClusterSummary (Lude.Maybe Lude.Text)
csOutpostARN = Lens.lens (outpostARN :: ClusterSummary -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: ClusterSummary)
{-# DEPRECATED csOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
--
-- /Note:/ Consider using 'normalizedInstanceHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNormalizedInstanceHours :: Lens.Lens' ClusterSummary (Lude.Maybe Lude.Int)
csNormalizedInstanceHours = Lens.lens (normalizedInstanceHours :: ClusterSummary -> Lude.Maybe Lude.Int) (\s a -> s {normalizedInstanceHours = a} :: ClusterSummary)
{-# DEPRECATED csNormalizedInstanceHours "Use generic-lens or generic-optics with 'normalizedInstanceHours' instead." #-}

-- | The name of the cluster.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' ClusterSummary (Lude.Maybe Lude.Text)
csName = Lens.lens (name :: ClusterSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ClusterSummary)
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier for the cluster.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csId :: Lens.Lens' ClusterSummary (Lude.Maybe Lude.Text)
csId = Lens.lens (id :: ClusterSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ClusterSummary)
{-# DEPRECATED csId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON ClusterSummary where
  parseJSON =
    Lude.withObject
      "ClusterSummary"
      ( \x ->
          ClusterSummary'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ClusterArn")
            Lude.<*> (x Lude..:? "OutpostArn")
            Lude.<*> (x Lude..:? "NormalizedInstanceHours")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
