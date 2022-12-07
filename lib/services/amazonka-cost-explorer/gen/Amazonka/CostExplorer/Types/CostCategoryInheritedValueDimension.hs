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
-- Module      : Amazonka.CostExplorer.Types.CostCategoryInheritedValueDimension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostCategoryInheritedValueDimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.CostCategoryInheritedValueDimensionName
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When you create or update a cost category, you can define the
-- @CostCategoryRule@ rule type as @INHERITED_VALUE@. This rule type adds
-- the flexibility to define a rule that dynamically inherits the cost
-- category value from the dimension value that\'s defined by
-- @CostCategoryInheritedValueDimension@. For example, suppose that you
-- want to dynamically group costs that are based on the value of a
-- specific tag key. First, choose an inherited value rule type, and then
-- choose the tag dimension and specify the tag key to use.
--
-- /See:/ 'newCostCategoryInheritedValueDimension' smart constructor.
data CostCategoryInheritedValueDimension = CostCategoryInheritedValueDimension'
  { -- | The key to extract cost category values.
    dimensionKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the dimension that\'s used to group costs.
    --
    -- If you specify @LINKED_ACCOUNT_NAME@, the cost category value is based
    -- on account name. If you specify @TAG@, the cost category value is based
    -- on the value of the specified tag key.
    dimensionName :: Prelude.Maybe CostCategoryInheritedValueDimensionName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostCategoryInheritedValueDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionKey', 'costCategoryInheritedValueDimension_dimensionKey' - The key to extract cost category values.
--
-- 'dimensionName', 'costCategoryInheritedValueDimension_dimensionName' - The name of the dimension that\'s used to group costs.
--
-- If you specify @LINKED_ACCOUNT_NAME@, the cost category value is based
-- on account name. If you specify @TAG@, the cost category value is based
-- on the value of the specified tag key.
newCostCategoryInheritedValueDimension ::
  CostCategoryInheritedValueDimension
newCostCategoryInheritedValueDimension =
  CostCategoryInheritedValueDimension'
    { dimensionKey =
        Prelude.Nothing,
      dimensionName = Prelude.Nothing
    }

-- | The key to extract cost category values.
costCategoryInheritedValueDimension_dimensionKey :: Lens.Lens' CostCategoryInheritedValueDimension (Prelude.Maybe Prelude.Text)
costCategoryInheritedValueDimension_dimensionKey = Lens.lens (\CostCategoryInheritedValueDimension' {dimensionKey} -> dimensionKey) (\s@CostCategoryInheritedValueDimension' {} a -> s {dimensionKey = a} :: CostCategoryInheritedValueDimension)

-- | The name of the dimension that\'s used to group costs.
--
-- If you specify @LINKED_ACCOUNT_NAME@, the cost category value is based
-- on account name. If you specify @TAG@, the cost category value is based
-- on the value of the specified tag key.
costCategoryInheritedValueDimension_dimensionName :: Lens.Lens' CostCategoryInheritedValueDimension (Prelude.Maybe CostCategoryInheritedValueDimensionName)
costCategoryInheritedValueDimension_dimensionName = Lens.lens (\CostCategoryInheritedValueDimension' {dimensionName} -> dimensionName) (\s@CostCategoryInheritedValueDimension' {} a -> s {dimensionName = a} :: CostCategoryInheritedValueDimension)

instance
  Data.FromJSON
    CostCategoryInheritedValueDimension
  where
  parseJSON =
    Data.withObject
      "CostCategoryInheritedValueDimension"
      ( \x ->
          CostCategoryInheritedValueDimension'
            Prelude.<$> (x Data..:? "DimensionKey")
            Prelude.<*> (x Data..:? "DimensionName")
      )

instance
  Prelude.Hashable
    CostCategoryInheritedValueDimension
  where
  hashWithSalt
    _salt
    CostCategoryInheritedValueDimension' {..} =
      _salt `Prelude.hashWithSalt` dimensionKey
        `Prelude.hashWithSalt` dimensionName

instance
  Prelude.NFData
    CostCategoryInheritedValueDimension
  where
  rnf CostCategoryInheritedValueDimension' {..} =
    Prelude.rnf dimensionKey
      `Prelude.seq` Prelude.rnf dimensionName

instance
  Data.ToJSON
    CostCategoryInheritedValueDimension
  where
  toJSON CostCategoryInheritedValueDimension' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DimensionKey" Data..=) Prelude.<$> dimensionKey,
            ("DimensionName" Data..=) Prelude.<$> dimensionName
          ]
      )
