{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ConstraintDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ConstraintDetail
  ( ConstraintDetail (..),

    -- * Smart constructor
    mkConstraintDetail,

    -- * Lenses
    cdPortfolioId,
    cdConstraintId,
    cdOwner,
    cdType,
    cdDescription,
    cdProductId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a constraint.
--
-- /See:/ 'mkConstraintDetail' smart constructor.
data ConstraintDetail = ConstraintDetail'
  { portfolioId ::
      Lude.Maybe Lude.Text,
    constraintId :: Lude.Maybe Lude.Text,
    owner :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    productId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConstraintDetail' with the minimum fields required to make a request.
--
-- * 'constraintId' - The identifier of the constraint.
-- * 'description' - The description of the constraint.
-- * 'owner' - The owner of the constraint.
-- * 'portfolioId' - The identifier of the portfolio the product resides in. The constraint applies only to the instance of the product that lives within this portfolio.
-- * 'productId' - The identifier of the product the constraint applies to. Note that a constraint applies to a specific instance of a product within a certain portfolio.
-- * 'type'' - The type of constraint.
--
--
--     * @LAUNCH@
--
--
--     * @NOTIFICATION@
--
--
--     * STACKSET
--
--
--     * @TEMPLATE@
mkConstraintDetail ::
  ConstraintDetail
mkConstraintDetail =
  ConstraintDetail'
    { portfolioId = Lude.Nothing,
      constraintId = Lude.Nothing,
      owner = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing,
      productId = Lude.Nothing
    }

-- | The identifier of the portfolio the product resides in. The constraint applies only to the instance of the product that lives within this portfolio.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPortfolioId :: Lens.Lens' ConstraintDetail (Lude.Maybe Lude.Text)
cdPortfolioId = Lens.lens (portfolioId :: ConstraintDetail -> Lude.Maybe Lude.Text) (\s a -> s {portfolioId = a} :: ConstraintDetail)
{-# DEPRECATED cdPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | The identifier of the constraint.
--
-- /Note:/ Consider using 'constraintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConstraintId :: Lens.Lens' ConstraintDetail (Lude.Maybe Lude.Text)
cdConstraintId = Lens.lens (constraintId :: ConstraintDetail -> Lude.Maybe Lude.Text) (\s a -> s {constraintId = a} :: ConstraintDetail)
{-# DEPRECATED cdConstraintId "Use generic-lens or generic-optics with 'constraintId' instead." #-}

-- | The owner of the constraint.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdOwner :: Lens.Lens' ConstraintDetail (Lude.Maybe Lude.Text)
cdOwner = Lens.lens (owner :: ConstraintDetail -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: ConstraintDetail)
{-# DEPRECATED cdOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The type of constraint.
--
--
--     * @LAUNCH@
--
--
--     * @NOTIFICATION@
--
--
--     * STACKSET
--
--
--     * @TEMPLATE@
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdType :: Lens.Lens' ConstraintDetail (Lude.Maybe Lude.Text)
cdType = Lens.lens (type' :: ConstraintDetail -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ConstraintDetail)
{-# DEPRECATED cdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The description of the constraint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDescription :: Lens.Lens' ConstraintDetail (Lude.Maybe Lude.Text)
cdDescription = Lens.lens (description :: ConstraintDetail -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ConstraintDetail)
{-# DEPRECATED cdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The identifier of the product the constraint applies to. Note that a constraint applies to a specific instance of a product within a certain portfolio.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdProductId :: Lens.Lens' ConstraintDetail (Lude.Maybe Lude.Text)
cdProductId = Lens.lens (productId :: ConstraintDetail -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: ConstraintDetail)
{-# DEPRECATED cdProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Lude.FromJSON ConstraintDetail where
  parseJSON =
    Lude.withObject
      "ConstraintDetail"
      ( \x ->
          ConstraintDetail'
            Lude.<$> (x Lude..:? "PortfolioId")
            Lude.<*> (x Lude..:? "ConstraintId")
            Lude.<*> (x Lude..:? "Owner")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "ProductId")
      )
