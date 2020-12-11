-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DistributionBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DistributionBundle
  ( DistributionBundle (..),

    -- * Smart constructor
    mkDistributionBundle,

    -- * Lenses
    dbTransferPerMonthInGb,
    dbBundleId,
    dbName,
    dbPrice,
    dbIsActive,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the specifications of a distribution bundle.
--
-- /See:/ 'mkDistributionBundle' smart constructor.
data DistributionBundle = DistributionBundle'
  { transferPerMonthInGb ::
      Lude.Maybe Lude.Int,
    bundleId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    price :: Lude.Maybe Lude.Double,
    isActive :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DistributionBundle' with the minimum fields required to make a request.
--
-- * 'bundleId' - The ID of the bundle.
-- * 'isActive' - Indicates whether the bundle is active, and can be specified for a new distribution.
-- * 'name' - The name of the distribution bundle.
-- * 'price' - The monthly price, in US dollars, of the bundle.
-- * 'transferPerMonthInGb' - The monthly network transfer quota of the bundle.
mkDistributionBundle ::
  DistributionBundle
mkDistributionBundle =
  DistributionBundle'
    { transferPerMonthInGb = Lude.Nothing,
      bundleId = Lude.Nothing,
      name = Lude.Nothing,
      price = Lude.Nothing,
      isActive = Lude.Nothing
    }

-- | The monthly network transfer quota of the bundle.
--
-- /Note:/ Consider using 'transferPerMonthInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbTransferPerMonthInGb :: Lens.Lens' DistributionBundle (Lude.Maybe Lude.Int)
dbTransferPerMonthInGb = Lens.lens (transferPerMonthInGb :: DistributionBundle -> Lude.Maybe Lude.Int) (\s a -> s {transferPerMonthInGb = a} :: DistributionBundle)
{-# DEPRECATED dbTransferPerMonthInGb "Use generic-lens or generic-optics with 'transferPerMonthInGb' instead." #-}

-- | The ID of the bundle.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBundleId :: Lens.Lens' DistributionBundle (Lude.Maybe Lude.Text)
dbBundleId = Lens.lens (bundleId :: DistributionBundle -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: DistributionBundle)
{-# DEPRECATED dbBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The name of the distribution bundle.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbName :: Lens.Lens' DistributionBundle (Lude.Maybe Lude.Text)
dbName = Lens.lens (name :: DistributionBundle -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DistributionBundle)
{-# DEPRECATED dbName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The monthly price, in US dollars, of the bundle.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbPrice :: Lens.Lens' DistributionBundle (Lude.Maybe Lude.Double)
dbPrice = Lens.lens (price :: DistributionBundle -> Lude.Maybe Lude.Double) (\s a -> s {price = a} :: DistributionBundle)
{-# DEPRECATED dbPrice "Use generic-lens or generic-optics with 'price' instead." #-}

-- | Indicates whether the bundle is active, and can be specified for a new distribution.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbIsActive :: Lens.Lens' DistributionBundle (Lude.Maybe Lude.Bool)
dbIsActive = Lens.lens (isActive :: DistributionBundle -> Lude.Maybe Lude.Bool) (\s a -> s {isActive = a} :: DistributionBundle)
{-# DEPRECATED dbIsActive "Use generic-lens or generic-optics with 'isActive' instead." #-}

instance Lude.FromJSON DistributionBundle where
  parseJSON =
    Lude.withObject
      "DistributionBundle"
      ( \x ->
          DistributionBundle'
            Lude.<$> (x Lude..:? "transferPerMonthInGb")
            Lude.<*> (x Lude..:? "bundleId")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "price")
            Lude.<*> (x Lude..:? "isActive")
      )
