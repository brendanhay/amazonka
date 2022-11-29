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
-- Module      : Amazonka.ServiceCatalog.Types.ConstraintDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ConstraintDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a constraint.
--
-- /See:/ 'newConstraintDetail' smart constructor.
data ConstraintDetail = ConstraintDetail'
  { -- | The identifier of the portfolio the product resides in. The constraint
    -- applies only to the instance of the product that lives within this
    -- portfolio.
    portfolioId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the constraint.
    constraintId :: Prelude.Maybe Prelude.Text,
    -- | The type of constraint.
    --
    -- -   @LAUNCH@
    --
    -- -   @NOTIFICATION@
    --
    -- -   STACKSET
    --
    -- -   @TEMPLATE@
    type' :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the product the constraint applies to. Note that a
    -- constraint applies to a specific instance of a product within a certain
    -- portfolio.
    productId :: Prelude.Maybe Prelude.Text,
    -- | The owner of the constraint.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The description of the constraint.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConstraintDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portfolioId', 'constraintDetail_portfolioId' - The identifier of the portfolio the product resides in. The constraint
-- applies only to the instance of the product that lives within this
-- portfolio.
--
-- 'constraintId', 'constraintDetail_constraintId' - The identifier of the constraint.
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
--
-- 'productId', 'constraintDetail_productId' - The identifier of the product the constraint applies to. Note that a
-- constraint applies to a specific instance of a product within a certain
-- portfolio.
--
-- 'owner', 'constraintDetail_owner' - The owner of the constraint.
--
-- 'description', 'constraintDetail_description' - The description of the constraint.
newConstraintDetail ::
  ConstraintDetail
newConstraintDetail =
  ConstraintDetail'
    { portfolioId = Prelude.Nothing,
      constraintId = Prelude.Nothing,
      type' = Prelude.Nothing,
      productId = Prelude.Nothing,
      owner = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The identifier of the portfolio the product resides in. The constraint
-- applies only to the instance of the product that lives within this
-- portfolio.
constraintDetail_portfolioId :: Lens.Lens' ConstraintDetail (Prelude.Maybe Prelude.Text)
constraintDetail_portfolioId = Lens.lens (\ConstraintDetail' {portfolioId} -> portfolioId) (\s@ConstraintDetail' {} a -> s {portfolioId = a} :: ConstraintDetail)

-- | The identifier of the constraint.
constraintDetail_constraintId :: Lens.Lens' ConstraintDetail (Prelude.Maybe Prelude.Text)
constraintDetail_constraintId = Lens.lens (\ConstraintDetail' {constraintId} -> constraintId) (\s@ConstraintDetail' {} a -> s {constraintId = a} :: ConstraintDetail)

-- | The type of constraint.
--
-- -   @LAUNCH@
--
-- -   @NOTIFICATION@
--
-- -   STACKSET
--
-- -   @TEMPLATE@
constraintDetail_type :: Lens.Lens' ConstraintDetail (Prelude.Maybe Prelude.Text)
constraintDetail_type = Lens.lens (\ConstraintDetail' {type'} -> type') (\s@ConstraintDetail' {} a -> s {type' = a} :: ConstraintDetail)

-- | The identifier of the product the constraint applies to. Note that a
-- constraint applies to a specific instance of a product within a certain
-- portfolio.
constraintDetail_productId :: Lens.Lens' ConstraintDetail (Prelude.Maybe Prelude.Text)
constraintDetail_productId = Lens.lens (\ConstraintDetail' {productId} -> productId) (\s@ConstraintDetail' {} a -> s {productId = a} :: ConstraintDetail)

-- | The owner of the constraint.
constraintDetail_owner :: Lens.Lens' ConstraintDetail (Prelude.Maybe Prelude.Text)
constraintDetail_owner = Lens.lens (\ConstraintDetail' {owner} -> owner) (\s@ConstraintDetail' {} a -> s {owner = a} :: ConstraintDetail)

-- | The description of the constraint.
constraintDetail_description :: Lens.Lens' ConstraintDetail (Prelude.Maybe Prelude.Text)
constraintDetail_description = Lens.lens (\ConstraintDetail' {description} -> description) (\s@ConstraintDetail' {} a -> s {description = a} :: ConstraintDetail)

instance Core.FromJSON ConstraintDetail where
  parseJSON =
    Core.withObject
      "ConstraintDetail"
      ( \x ->
          ConstraintDetail'
            Prelude.<$> (x Core..:? "PortfolioId")
            Prelude.<*> (x Core..:? "ConstraintId")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "ProductId")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable ConstraintDetail where
  hashWithSalt _salt ConstraintDetail' {..} =
    _salt `Prelude.hashWithSalt` portfolioId
      `Prelude.hashWithSalt` constraintId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` description

instance Prelude.NFData ConstraintDetail where
  rnf ConstraintDetail' {..} =
    Prelude.rnf portfolioId
      `Prelude.seq` Prelude.rnf constraintId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf description
