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
-- Module      : Amazonka.QuickSight.Types.ComparativeOrder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ComparativeOrder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnOrderingType
import Amazonka.QuickSight.Types.UndefinedSpecifiedValueType

-- | A structure that represents a comparative order.
--
-- /See:/ 'newComparativeOrder' smart constructor.
data ComparativeOrder = ComparativeOrder'
  { -- | The list of columns to be used in the ordering.
    specifedOrder :: Prelude.Maybe [Prelude.Text],
    -- | The treat of undefined specified values. Valid values for this structure
    -- are @LEAST@ and @MOST@.
    treatUndefinedSpecifiedValues :: Prelude.Maybe UndefinedSpecifiedValueType,
    -- | The ordering type for a column. Valid values for this structure are
    -- @GREATER_IS_BETTER@, @LESSER_IS_BETTER@ and @SPECIFIED@.
    useOrdering :: Prelude.Maybe ColumnOrderingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComparativeOrder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'specifedOrder', 'comparativeOrder_specifedOrder' - The list of columns to be used in the ordering.
--
-- 'treatUndefinedSpecifiedValues', 'comparativeOrder_treatUndefinedSpecifiedValues' - The treat of undefined specified values. Valid values for this structure
-- are @LEAST@ and @MOST@.
--
-- 'useOrdering', 'comparativeOrder_useOrdering' - The ordering type for a column. Valid values for this structure are
-- @GREATER_IS_BETTER@, @LESSER_IS_BETTER@ and @SPECIFIED@.
newComparativeOrder ::
  ComparativeOrder
newComparativeOrder =
  ComparativeOrder'
    { specifedOrder = Prelude.Nothing,
      treatUndefinedSpecifiedValues = Prelude.Nothing,
      useOrdering = Prelude.Nothing
    }

-- | The list of columns to be used in the ordering.
comparativeOrder_specifedOrder :: Lens.Lens' ComparativeOrder (Prelude.Maybe [Prelude.Text])
comparativeOrder_specifedOrder = Lens.lens (\ComparativeOrder' {specifedOrder} -> specifedOrder) (\s@ComparativeOrder' {} a -> s {specifedOrder = a} :: ComparativeOrder) Prelude.. Lens.mapping Lens.coerced

-- | The treat of undefined specified values. Valid values for this structure
-- are @LEAST@ and @MOST@.
comparativeOrder_treatUndefinedSpecifiedValues :: Lens.Lens' ComparativeOrder (Prelude.Maybe UndefinedSpecifiedValueType)
comparativeOrder_treatUndefinedSpecifiedValues = Lens.lens (\ComparativeOrder' {treatUndefinedSpecifiedValues} -> treatUndefinedSpecifiedValues) (\s@ComparativeOrder' {} a -> s {treatUndefinedSpecifiedValues = a} :: ComparativeOrder)

-- | The ordering type for a column. Valid values for this structure are
-- @GREATER_IS_BETTER@, @LESSER_IS_BETTER@ and @SPECIFIED@.
comparativeOrder_useOrdering :: Lens.Lens' ComparativeOrder (Prelude.Maybe ColumnOrderingType)
comparativeOrder_useOrdering = Lens.lens (\ComparativeOrder' {useOrdering} -> useOrdering) (\s@ComparativeOrder' {} a -> s {useOrdering = a} :: ComparativeOrder)

instance Data.FromJSON ComparativeOrder where
  parseJSON =
    Data.withObject
      "ComparativeOrder"
      ( \x ->
          ComparativeOrder'
            Prelude.<$> (x Data..:? "SpecifedOrder" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TreatUndefinedSpecifiedValues")
            Prelude.<*> (x Data..:? "UseOrdering")
      )

instance Prelude.Hashable ComparativeOrder where
  hashWithSalt _salt ComparativeOrder' {..} =
    _salt
      `Prelude.hashWithSalt` specifedOrder
      `Prelude.hashWithSalt` treatUndefinedSpecifiedValues
      `Prelude.hashWithSalt` useOrdering

instance Prelude.NFData ComparativeOrder where
  rnf ComparativeOrder' {..} =
    Prelude.rnf specifedOrder
      `Prelude.seq` Prelude.rnf treatUndefinedSpecifiedValues
      `Prelude.seq` Prelude.rnf useOrdering

instance Data.ToJSON ComparativeOrder where
  toJSON ComparativeOrder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SpecifedOrder" Data..=) Prelude.<$> specifedOrder,
            ("TreatUndefinedSpecifiedValues" Data..=)
              Prelude.<$> treatUndefinedSpecifiedValues,
            ("UseOrdering" Data..=) Prelude.<$> useOrdering
          ]
      )
