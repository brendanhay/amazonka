-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
  ( CostCategoryProcessingStatus (..),

    -- * Smart constructor
    mkCostCategoryProcessingStatus,

    -- * Lenses
    ccpsStatus,
    ccpsComponent,
  )
where

import Network.AWS.CostExplorer.Types.CostCategoryStatus
import Network.AWS.CostExplorer.Types.CostCategoryStatusComponent
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The list of processing statuses for Cost Management products for a specific cost category.
--
-- /See:/ 'mkCostCategoryProcessingStatus' smart constructor.
data CostCategoryProcessingStatus = CostCategoryProcessingStatus'
  { status ::
      Lude.Maybe CostCategoryStatus,
    component ::
      Lude.Maybe
        CostCategoryStatusComponent
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CostCategoryProcessingStatus' with the minimum fields required to make a request.
--
-- * 'component' - The Cost Management product name of the applied status.
-- * 'status' - The process status for a specific cost category.
mkCostCategoryProcessingStatus ::
  CostCategoryProcessingStatus
mkCostCategoryProcessingStatus =
  CostCategoryProcessingStatus'
    { status = Lude.Nothing,
      component = Lude.Nothing
    }

-- | The process status for a specific cost category.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpsStatus :: Lens.Lens' CostCategoryProcessingStatus (Lude.Maybe CostCategoryStatus)
ccpsStatus = Lens.lens (status :: CostCategoryProcessingStatus -> Lude.Maybe CostCategoryStatus) (\s a -> s {status = a} :: CostCategoryProcessingStatus)
{-# DEPRECATED ccpsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Cost Management product name of the applied status.
--
-- /Note:/ Consider using 'component' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpsComponent :: Lens.Lens' CostCategoryProcessingStatus (Lude.Maybe CostCategoryStatusComponent)
ccpsComponent = Lens.lens (component :: CostCategoryProcessingStatus -> Lude.Maybe CostCategoryStatusComponent) (\s a -> s {component = a} :: CostCategoryProcessingStatus)
{-# DEPRECATED ccpsComponent "Use generic-lens or generic-optics with 'component' instead." #-}

instance Lude.FromJSON CostCategoryProcessingStatus where
  parseJSON =
    Lude.withObject
      "CostCategoryProcessingStatus"
      ( \x ->
          CostCategoryProcessingStatus'
            Lude.<$> (x Lude..:? "Status") Lude.<*> (x Lude..:? "Component")
      )
