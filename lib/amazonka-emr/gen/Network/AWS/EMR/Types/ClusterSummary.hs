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
    cStatus,
    cClusterARN,
    cOutpostARN,
    cNormalizedInstanceHours,
    cName,
    cId,
  )
where

import Network.AWS.EMR.Types.ClusterStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The summary description of the cluster.
--
-- /See:/ 'mkClusterSummary' smart constructor.
data ClusterSummary = ClusterSummary'
  { status ::
      Lude.Maybe ClusterStatus,
    clusterARN :: Lude.Maybe Lude.Text,
    outpostARN :: Lude.Maybe Lude.Text,
    normalizedInstanceHours :: Lude.Maybe Lude.Int,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterSummary' with the minimum fields required to make a request.
--
-- * 'clusterARN' - The Amazon Resource Name of the cluster.
-- * 'id' - The unique identifier for the cluster.
-- * 'name' - The name of the cluster.
-- * 'normalizedInstanceHours' - An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost where the cluster is launched.
-- * 'status' - The details about the current status of the cluster.
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
cStatus :: Lens.Lens' ClusterSummary (Lude.Maybe ClusterStatus)
cStatus = Lens.lens (status :: ClusterSummary -> Lude.Maybe ClusterStatus) (\s a -> s {status = a} :: ClusterSummary)
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterARN :: Lens.Lens' ClusterSummary (Lude.Maybe Lude.Text)
cClusterARN = Lens.lens (clusterARN :: ClusterSummary -> Lude.Maybe Lude.Text) (\s a -> s {clusterARN = a} :: ClusterSummary)
{-# DEPRECATED cClusterARN "Use generic-lens or generic-optics with 'clusterARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost where the cluster is launched.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOutpostARN :: Lens.Lens' ClusterSummary (Lude.Maybe Lude.Text)
cOutpostARN = Lens.lens (outpostARN :: ClusterSummary -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: ClusterSummary)
{-# DEPRECATED cOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
--
-- /Note:/ Consider using 'normalizedInstanceHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNormalizedInstanceHours :: Lens.Lens' ClusterSummary (Lude.Maybe Lude.Int)
cNormalizedInstanceHours = Lens.lens (normalizedInstanceHours :: ClusterSummary -> Lude.Maybe Lude.Int) (\s a -> s {normalizedInstanceHours = a} :: ClusterSummary)
{-# DEPRECATED cNormalizedInstanceHours "Use generic-lens or generic-optics with 'normalizedInstanceHours' instead." #-}

-- | The name of the cluster.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' ClusterSummary (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: ClusterSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ClusterSummary)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier for the cluster.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' ClusterSummary (Lude.Maybe Lude.Text)
cId = Lens.lens (id :: ClusterSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ClusterSummary)
{-# DEPRECATED cId "Use generic-lens or generic-optics with 'id' instead." #-}

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
