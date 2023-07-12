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
-- Module      : Amazonka.BillingConductor.Types.PricingPlanListElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.PricingPlanListElement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of a pricing plan.
--
-- /See:/ 'newPricingPlanListElement' smart constructor.
data PricingPlanListElement = PricingPlanListElement'
  { -- | The pricing plan Amazon Resource Names (ARN). This can be used to
    -- uniquely identify a pricing plan.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time when the pricing plan was created.
    creationTime :: Prelude.Maybe Prelude.Integer,
    -- | The pricing plan description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The most recent time when the pricing plan was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The name of a pricing plan.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The pricing rules count that\'s currently associated with this pricing
    -- plan list element.
    size :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PricingPlanListElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'pricingPlanListElement_arn' - The pricing plan Amazon Resource Names (ARN). This can be used to
-- uniquely identify a pricing plan.
--
-- 'creationTime', 'pricingPlanListElement_creationTime' - The time when the pricing plan was created.
--
-- 'description', 'pricingPlanListElement_description' - The pricing plan description.
--
-- 'lastModifiedTime', 'pricingPlanListElement_lastModifiedTime' - The most recent time when the pricing plan was modified.
--
-- 'name', 'pricingPlanListElement_name' - The name of a pricing plan.
--
-- 'size', 'pricingPlanListElement_size' - The pricing rules count that\'s currently associated with this pricing
-- plan list element.
newPricingPlanListElement ::
  PricingPlanListElement
newPricingPlanListElement =
  PricingPlanListElement'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      size = Prelude.Nothing
    }

-- | The pricing plan Amazon Resource Names (ARN). This can be used to
-- uniquely identify a pricing plan.
pricingPlanListElement_arn :: Lens.Lens' PricingPlanListElement (Prelude.Maybe Prelude.Text)
pricingPlanListElement_arn = Lens.lens (\PricingPlanListElement' {arn} -> arn) (\s@PricingPlanListElement' {} a -> s {arn = a} :: PricingPlanListElement)

-- | The time when the pricing plan was created.
pricingPlanListElement_creationTime :: Lens.Lens' PricingPlanListElement (Prelude.Maybe Prelude.Integer)
pricingPlanListElement_creationTime = Lens.lens (\PricingPlanListElement' {creationTime} -> creationTime) (\s@PricingPlanListElement' {} a -> s {creationTime = a} :: PricingPlanListElement)

-- | The pricing plan description.
pricingPlanListElement_description :: Lens.Lens' PricingPlanListElement (Prelude.Maybe Prelude.Text)
pricingPlanListElement_description = Lens.lens (\PricingPlanListElement' {description} -> description) (\s@PricingPlanListElement' {} a -> s {description = a} :: PricingPlanListElement) Prelude.. Lens.mapping Data._Sensitive

-- | The most recent time when the pricing plan was modified.
pricingPlanListElement_lastModifiedTime :: Lens.Lens' PricingPlanListElement (Prelude.Maybe Prelude.Integer)
pricingPlanListElement_lastModifiedTime = Lens.lens (\PricingPlanListElement' {lastModifiedTime} -> lastModifiedTime) (\s@PricingPlanListElement' {} a -> s {lastModifiedTime = a} :: PricingPlanListElement)

-- | The name of a pricing plan.
pricingPlanListElement_name :: Lens.Lens' PricingPlanListElement (Prelude.Maybe Prelude.Text)
pricingPlanListElement_name = Lens.lens (\PricingPlanListElement' {name} -> name) (\s@PricingPlanListElement' {} a -> s {name = a} :: PricingPlanListElement) Prelude.. Lens.mapping Data._Sensitive

-- | The pricing rules count that\'s currently associated with this pricing
-- plan list element.
pricingPlanListElement_size :: Lens.Lens' PricingPlanListElement (Prelude.Maybe Prelude.Natural)
pricingPlanListElement_size = Lens.lens (\PricingPlanListElement' {size} -> size) (\s@PricingPlanListElement' {} a -> s {size = a} :: PricingPlanListElement)

instance Data.FromJSON PricingPlanListElement where
  parseJSON =
    Data.withObject
      "PricingPlanListElement"
      ( \x ->
          PricingPlanListElement'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Size")
      )

instance Prelude.Hashable PricingPlanListElement where
  hashWithSalt _salt PricingPlanListElement' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` size

instance Prelude.NFData PricingPlanListElement where
  rnf PricingPlanListElement' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf size
