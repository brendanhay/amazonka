{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.BudgetDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.BudgetDetail
  ( BudgetDetail (..)
  -- * Smart constructor
  , mkBudgetDetail
  -- * Lenses
  , bdBudgetName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.BudgetName as Types

-- | Information about a budget.
--
-- /See:/ 'mkBudgetDetail' smart constructor.
newtype BudgetDetail = BudgetDetail'
  { budgetName :: Core.Maybe Types.BudgetName
    -- ^ Name of the associated budget.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BudgetDetail' value with any optional fields omitted.
mkBudgetDetail
    :: BudgetDetail
mkBudgetDetail = BudgetDetail'{budgetName = Core.Nothing}

-- | Name of the associated budget.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBudgetName :: Lens.Lens' BudgetDetail (Core.Maybe Types.BudgetName)
bdBudgetName = Lens.field @"budgetName"
{-# INLINEABLE bdBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

instance Core.FromJSON BudgetDetail where
        parseJSON
          = Core.withObject "BudgetDetail" Core.$
              \ x -> BudgetDetail' Core.<$> (x Core..:? "BudgetName")
