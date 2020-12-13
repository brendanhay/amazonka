{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.InstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.InstanceDetails
  ( InstanceDetails (..),

    -- * Smart constructor
    mkInstanceDetails,

    -- * Lenses
    idESInstanceDetails,
    idRDSInstanceDetails,
    idElastiCacheInstanceDetails,
    idEC2InstanceDetails,
    idRedshiftInstanceDetails,
  )
where

import Network.AWS.CostExplorer.Types.EC2InstanceDetails
import Network.AWS.CostExplorer.Types.ESInstanceDetails
import Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
import Network.AWS.CostExplorer.Types.RDSInstanceDetails
import Network.AWS.CostExplorer.Types.RedshiftInstanceDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the instances that AWS recommends that you purchase.
--
-- /See:/ 'mkInstanceDetails' smart constructor.
data InstanceDetails = InstanceDetails'
  { -- | The Amazon ES instances that AWS recommends that you purchase.
    eSInstanceDetails :: Lude.Maybe ESInstanceDetails,
    -- | The Amazon RDS instances that AWS recommends that you purchase.
    rdsInstanceDetails :: Lude.Maybe RDSInstanceDetails,
    -- | The ElastiCache instances that AWS recommends that you purchase.
    elastiCacheInstanceDetails :: Lude.Maybe ElastiCacheInstanceDetails,
    -- | The Amazon EC2 instances that AWS recommends that you purchase.
    ec2InstanceDetails :: Lude.Maybe EC2InstanceDetails,
    -- | The Amazon Redshift instances that AWS recommends that you purchase.
    redshiftInstanceDetails :: Lude.Maybe RedshiftInstanceDetails
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceDetails' with the minimum fields required to make a request.
--
-- * 'eSInstanceDetails' - The Amazon ES instances that AWS recommends that you purchase.
-- * 'rdsInstanceDetails' - The Amazon RDS instances that AWS recommends that you purchase.
-- * 'elastiCacheInstanceDetails' - The ElastiCache instances that AWS recommends that you purchase.
-- * 'ec2InstanceDetails' - The Amazon EC2 instances that AWS recommends that you purchase.
-- * 'redshiftInstanceDetails' - The Amazon Redshift instances that AWS recommends that you purchase.
mkInstanceDetails ::
  InstanceDetails
mkInstanceDetails =
  InstanceDetails'
    { eSInstanceDetails = Lude.Nothing,
      rdsInstanceDetails = Lude.Nothing,
      elastiCacheInstanceDetails = Lude.Nothing,
      ec2InstanceDetails = Lude.Nothing,
      redshiftInstanceDetails = Lude.Nothing
    }

-- | The Amazon ES instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'eSInstanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idESInstanceDetails :: Lens.Lens' InstanceDetails (Lude.Maybe ESInstanceDetails)
idESInstanceDetails = Lens.lens (eSInstanceDetails :: InstanceDetails -> Lude.Maybe ESInstanceDetails) (\s a -> s {eSInstanceDetails = a} :: InstanceDetails)
{-# DEPRECATED idESInstanceDetails "Use generic-lens or generic-optics with 'eSInstanceDetails' instead." #-}

-- | The Amazon RDS instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'rdsInstanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idRDSInstanceDetails :: Lens.Lens' InstanceDetails (Lude.Maybe RDSInstanceDetails)
idRDSInstanceDetails = Lens.lens (rdsInstanceDetails :: InstanceDetails -> Lude.Maybe RDSInstanceDetails) (\s a -> s {rdsInstanceDetails = a} :: InstanceDetails)
{-# DEPRECATED idRDSInstanceDetails "Use generic-lens or generic-optics with 'rdsInstanceDetails' instead." #-}

-- | The ElastiCache instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'elastiCacheInstanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idElastiCacheInstanceDetails :: Lens.Lens' InstanceDetails (Lude.Maybe ElastiCacheInstanceDetails)
idElastiCacheInstanceDetails = Lens.lens (elastiCacheInstanceDetails :: InstanceDetails -> Lude.Maybe ElastiCacheInstanceDetails) (\s a -> s {elastiCacheInstanceDetails = a} :: InstanceDetails)
{-# DEPRECATED idElastiCacheInstanceDetails "Use generic-lens or generic-optics with 'elastiCacheInstanceDetails' instead." #-}

-- | The Amazon EC2 instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'ec2InstanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idEC2InstanceDetails :: Lens.Lens' InstanceDetails (Lude.Maybe EC2InstanceDetails)
idEC2InstanceDetails = Lens.lens (ec2InstanceDetails :: InstanceDetails -> Lude.Maybe EC2InstanceDetails) (\s a -> s {ec2InstanceDetails = a} :: InstanceDetails)
{-# DEPRECATED idEC2InstanceDetails "Use generic-lens or generic-optics with 'ec2InstanceDetails' instead." #-}

-- | The Amazon Redshift instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'redshiftInstanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idRedshiftInstanceDetails :: Lens.Lens' InstanceDetails (Lude.Maybe RedshiftInstanceDetails)
idRedshiftInstanceDetails = Lens.lens (redshiftInstanceDetails :: InstanceDetails -> Lude.Maybe RedshiftInstanceDetails) (\s a -> s {redshiftInstanceDetails = a} :: InstanceDetails)
{-# DEPRECATED idRedshiftInstanceDetails "Use generic-lens or generic-optics with 'redshiftInstanceDetails' instead." #-}

instance Lude.FromJSON InstanceDetails where
  parseJSON =
    Lude.withObject
      "InstanceDetails"
      ( \x ->
          InstanceDetails'
            Lude.<$> (x Lude..:? "ESInstanceDetails")
            Lude.<*> (x Lude..:? "RDSInstanceDetails")
            Lude.<*> (x Lude..:? "ElastiCacheInstanceDetails")
            Lude.<*> (x Lude..:? "EC2InstanceDetails")
            Lude.<*> (x Lude..:? "RedshiftInstanceDetails")
      )
