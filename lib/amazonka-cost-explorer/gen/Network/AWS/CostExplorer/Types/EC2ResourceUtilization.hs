-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EC2ResourceUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2ResourceUtilization
  ( EC2ResourceUtilization (..),

    -- * Smart constructor
    mkEC2ResourceUtilization,

    -- * Lenses
    eruMaxCPUUtilizationPercentage,
    eruEBSResourceUtilization,
    eruMaxStorageUtilizationPercentage,
    eruMaxMemoryUtilizationPercentage,
  )
where

import Network.AWS.CostExplorer.Types.EBSResourceUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Utilization metrics of the instance.
--
-- /See:/ 'mkEC2ResourceUtilization' smart constructor.
data EC2ResourceUtilization = EC2ResourceUtilization'
  { maxCPUUtilizationPercentage ::
      Lude.Maybe Lude.Text,
    ebsResourceUtilization ::
      Lude.Maybe EBSResourceUtilization,
    maxStorageUtilizationPercentage ::
      Lude.Maybe Lude.Text,
    maxMemoryUtilizationPercentage ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EC2ResourceUtilization' with the minimum fields required to make a request.
--
-- * 'ebsResourceUtilization' - The EBS field that contains a list of EBS metrics associated with the current instance.
-- * 'maxCPUUtilizationPercentage' - Maximum observed or expected CPU utilization of the instance.
-- * 'maxMemoryUtilizationPercentage' - Maximum observed or expected memory utilization of the instance.
-- * 'maxStorageUtilizationPercentage' - Maximum observed or expected storage utilization of the instance (does not measure EBS storage).
mkEC2ResourceUtilization ::
  EC2ResourceUtilization
mkEC2ResourceUtilization =
  EC2ResourceUtilization'
    { maxCPUUtilizationPercentage =
        Lude.Nothing,
      ebsResourceUtilization = Lude.Nothing,
      maxStorageUtilizationPercentage = Lude.Nothing,
      maxMemoryUtilizationPercentage = Lude.Nothing
    }

-- | Maximum observed or expected CPU utilization of the instance.
--
-- /Note:/ Consider using 'maxCPUUtilizationPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eruMaxCPUUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Lude.Maybe Lude.Text)
eruMaxCPUUtilizationPercentage = Lens.lens (maxCPUUtilizationPercentage :: EC2ResourceUtilization -> Lude.Maybe Lude.Text) (\s a -> s {maxCPUUtilizationPercentage = a} :: EC2ResourceUtilization)
{-# DEPRECATED eruMaxCPUUtilizationPercentage "Use generic-lens or generic-optics with 'maxCPUUtilizationPercentage' instead." #-}

-- | The EBS field that contains a list of EBS metrics associated with the current instance.
--
-- /Note:/ Consider using 'ebsResourceUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eruEBSResourceUtilization :: Lens.Lens' EC2ResourceUtilization (Lude.Maybe EBSResourceUtilization)
eruEBSResourceUtilization = Lens.lens (ebsResourceUtilization :: EC2ResourceUtilization -> Lude.Maybe EBSResourceUtilization) (\s a -> s {ebsResourceUtilization = a} :: EC2ResourceUtilization)
{-# DEPRECATED eruEBSResourceUtilization "Use generic-lens or generic-optics with 'ebsResourceUtilization' instead." #-}

-- | Maximum observed or expected storage utilization of the instance (does not measure EBS storage).
--
-- /Note:/ Consider using 'maxStorageUtilizationPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eruMaxStorageUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Lude.Maybe Lude.Text)
eruMaxStorageUtilizationPercentage = Lens.lens (maxStorageUtilizationPercentage :: EC2ResourceUtilization -> Lude.Maybe Lude.Text) (\s a -> s {maxStorageUtilizationPercentage = a} :: EC2ResourceUtilization)
{-# DEPRECATED eruMaxStorageUtilizationPercentage "Use generic-lens or generic-optics with 'maxStorageUtilizationPercentage' instead." #-}

-- | Maximum observed or expected memory utilization of the instance.
--
-- /Note:/ Consider using 'maxMemoryUtilizationPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eruMaxMemoryUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Lude.Maybe Lude.Text)
eruMaxMemoryUtilizationPercentage = Lens.lens (maxMemoryUtilizationPercentage :: EC2ResourceUtilization -> Lude.Maybe Lude.Text) (\s a -> s {maxMemoryUtilizationPercentage = a} :: EC2ResourceUtilization)
{-# DEPRECATED eruMaxMemoryUtilizationPercentage "Use generic-lens or generic-optics with 'maxMemoryUtilizationPercentage' instead." #-}

instance Lude.FromJSON EC2ResourceUtilization where
  parseJSON =
    Lude.withObject
      "EC2ResourceUtilization"
      ( \x ->
          EC2ResourceUtilization'
            Lude.<$> (x Lude..:? "MaxCpuUtilizationPercentage")
            Lude.<*> (x Lude..:? "EBSResourceUtilization")
            Lude.<*> (x Lude..:? "MaxStorageUtilizationPercentage")
            Lude.<*> (x Lude..:? "MaxMemoryUtilizationPercentage")
      )
