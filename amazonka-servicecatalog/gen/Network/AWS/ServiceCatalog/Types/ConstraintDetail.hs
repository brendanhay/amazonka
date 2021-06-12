{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ConstraintDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ConstraintDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a constraint.
--
-- /See:/ 'newConstraintDetail' smart constructor.
data ConstraintDetail = ConstraintDetail'
  { -- | The identifier of the constraint.
    constraintId :: Core.Maybe Core.Text,
    -- | The identifier of the portfolio the product resides in. The constraint
    -- applies only to the instance of the product that lives within this
    -- portfolio.
    portfolioId :: Core.Maybe Core.Text,
    -- | The owner of the constraint.
    owner :: Core.Maybe Core.Text,
    -- | The identifier of the product the constraint applies to. Note that a
    -- constraint applies to a specific instance of a product within a certain
    -- portfolio.
    productId :: Core.Maybe Core.Text,
    -- | The description of the constraint.
    description :: Core.Maybe Core.Text,
    -- | The type of constraint.
    --
    -- -   @LAUNCH@
    --
    -- -   @NOTIFICATION@
    --
    -- -   STACKSET
    --
    -- -   @TEMPLATE@
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConstraintDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintId', 'constraintDetail_constraintId' - The identifier of the constraint.
--
-- 'portfolioId', 'constraintDetail_portfolioId' - The identifier of the portfolio the product resides in. The constraint
-- applies only to the instance of the product that lives within this
-- portfolio.
--
-- 'owner', 'constraintDetail_owner' - The owner of the constraint.
--
-- 'productId', 'constraintDetail_productId' - The identifier of the product the constraint applies to. Note that a
-- constraint applies to a specific instance of a product within a certain
-- portfolio.
--
-- 'description', 'constraintDetail_description' - The description of the constraint.
--
-- 'type'', 'constraintDetail_type' - The type of constraint.
--
-- -   @LAUNCH@
--
-- -   @NOTIFICATION@
--
-- -   STACKSET
--
-- -   @TEMPLATE@
newConstraintDetail ::
  ConstraintDetail
newConstraintDetail =
  ConstraintDetail'
    { constraintId = Core.Nothing,
      portfolioId = Core.Nothing,
      owner = Core.Nothing,
      productId = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing
    }

-- | The identifier of the constraint.
constraintDetail_constraintId :: Lens.Lens' ConstraintDetail (Core.Maybe Core.Text)
constraintDetail_constraintId = Lens.lens (\ConstraintDetail' {constraintId} -> constraintId) (\s@ConstraintDetail' {} a -> s {constraintId = a} :: ConstraintDetail)

-- | The identifier of the portfolio the product resides in. The constraint
-- applies only to the instance of the product that lives within this
-- portfolio.
constraintDetail_portfolioId :: Lens.Lens' ConstraintDetail (Core.Maybe Core.Text)
constraintDetail_portfolioId = Lens.lens (\ConstraintDetail' {portfolioId} -> portfolioId) (\s@ConstraintDetail' {} a -> s {portfolioId = a} :: ConstraintDetail)

-- | The owner of the constraint.
constraintDetail_owner :: Lens.Lens' ConstraintDetail (Core.Maybe Core.Text)
constraintDetail_owner = Lens.lens (\ConstraintDetail' {owner} -> owner) (\s@ConstraintDetail' {} a -> s {owner = a} :: ConstraintDetail)

-- | The identifier of the product the constraint applies to. Note that a
-- constraint applies to a specific instance of a product within a certain
-- portfolio.
constraintDetail_productId :: Lens.Lens' ConstraintDetail (Core.Maybe Core.Text)
constraintDetail_productId = Lens.lens (\ConstraintDetail' {productId} -> productId) (\s@ConstraintDetail' {} a -> s {productId = a} :: ConstraintDetail)

-- | The description of the constraint.
constraintDetail_description :: Lens.Lens' ConstraintDetail (Core.Maybe Core.Text)
constraintDetail_description = Lens.lens (\ConstraintDetail' {description} -> description) (\s@ConstraintDetail' {} a -> s {description = a} :: ConstraintDetail)

-- | The type of constraint.
--
-- -   @LAUNCH@
--
-- -   @NOTIFICATION@
--
-- -   STACKSET
--
-- -   @TEMPLATE@
constraintDetail_type :: Lens.Lens' ConstraintDetail (Core.Maybe Core.Text)
constraintDetail_type = Lens.lens (\ConstraintDetail' {type'} -> type') (\s@ConstraintDetail' {} a -> s {type' = a} :: ConstraintDetail)

instance Core.FromJSON ConstraintDetail where
  parseJSON =
    Core.withObject
      "ConstraintDetail"
      ( \x ->
          ConstraintDetail'
            Core.<$> (x Core..:? "ConstraintId")
            Core.<*> (x Core..:? "PortfolioId")
            Core.<*> (x Core..:? "Owner")
            Core.<*> (x Core..:? "ProductId")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable ConstraintDetail

instance Core.NFData ConstraintDetail
