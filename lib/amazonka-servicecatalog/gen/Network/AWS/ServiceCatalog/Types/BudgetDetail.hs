-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.BudgetDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.BudgetDetail
  ( BudgetDetail (..),

    -- * Smart constructor
    mkBudgetDetail,

    -- * Lenses
    bdBudgetName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a budget.
--
-- /See:/ 'mkBudgetDetail' smart constructor.
newtype BudgetDetail = BudgetDetail'
  { budgetName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BudgetDetail' with the minimum fields required to make a request.
--
-- * 'budgetName' - Name of the associated budget.
mkBudgetDetail ::
  BudgetDetail
mkBudgetDetail = BudgetDetail' {budgetName = Lude.Nothing}

-- | Name of the associated budget.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBudgetName :: Lens.Lens' BudgetDetail (Lude.Maybe Lude.Text)
bdBudgetName = Lens.lens (budgetName :: BudgetDetail -> Lude.Maybe Lude.Text) (\s a -> s {budgetName = a} :: BudgetDetail)
{-# DEPRECATED bdBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Lude.FromJSON BudgetDetail where
  parseJSON =
    Lude.withObject
      "BudgetDetail"
      (\x -> BudgetDetail' Lude.<$> (x Lude..:? "BudgetName"))
