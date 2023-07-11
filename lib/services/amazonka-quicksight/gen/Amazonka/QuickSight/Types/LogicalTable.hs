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
-- Module      : Amazonka.QuickSight.Types.LogicalTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LogicalTable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LogicalTableSource
import Amazonka.QuickSight.Types.TransformOperation

-- | A /logical table/ is a unit that joins and that data transformations
-- operate on. A logical table has a source, which can be either a physical
-- table or result of a join. When a logical table points to a physical
-- table, the logical table acts as a mutable copy of that physical table
-- through transform operations.
--
-- /See:/ 'newLogicalTable' smart constructor.
data LogicalTable = LogicalTable'
  { -- | Transform operations that act on this logical table.
    dataTransforms :: Prelude.Maybe (Prelude.NonEmpty TransformOperation),
    -- | A display name for the logical table.
    alias :: Prelude.Text,
    -- | Source of this logical table.
    source :: LogicalTableSource
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogicalTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTransforms', 'logicalTable_dataTransforms' - Transform operations that act on this logical table.
--
-- 'alias', 'logicalTable_alias' - A display name for the logical table.
--
-- 'source', 'logicalTable_source' - Source of this logical table.
newLogicalTable ::
  -- | 'alias'
  Prelude.Text ->
  -- | 'source'
  LogicalTableSource ->
  LogicalTable
newLogicalTable pAlias_ pSource_ =
  LogicalTable'
    { dataTransforms = Prelude.Nothing,
      alias = pAlias_,
      source = pSource_
    }

-- | Transform operations that act on this logical table.
logicalTable_dataTransforms :: Lens.Lens' LogicalTable (Prelude.Maybe (Prelude.NonEmpty TransformOperation))
logicalTable_dataTransforms = Lens.lens (\LogicalTable' {dataTransforms} -> dataTransforms) (\s@LogicalTable' {} a -> s {dataTransforms = a} :: LogicalTable) Prelude.. Lens.mapping Lens.coerced

-- | A display name for the logical table.
logicalTable_alias :: Lens.Lens' LogicalTable Prelude.Text
logicalTable_alias = Lens.lens (\LogicalTable' {alias} -> alias) (\s@LogicalTable' {} a -> s {alias = a} :: LogicalTable)

-- | Source of this logical table.
logicalTable_source :: Lens.Lens' LogicalTable LogicalTableSource
logicalTable_source = Lens.lens (\LogicalTable' {source} -> source) (\s@LogicalTable' {} a -> s {source = a} :: LogicalTable)

instance Data.FromJSON LogicalTable where
  parseJSON =
    Data.withObject
      "LogicalTable"
      ( \x ->
          LogicalTable'
            Prelude.<$> (x Data..:? "DataTransforms")
            Prelude.<*> (x Data..: "Alias")
            Prelude.<*> (x Data..: "Source")
      )

instance Prelude.Hashable LogicalTable where
  hashWithSalt _salt LogicalTable' {..} =
    _salt
      `Prelude.hashWithSalt` dataTransforms
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` source

instance Prelude.NFData LogicalTable where
  rnf LogicalTable' {..} =
    Prelude.rnf dataTransforms
      `Prelude.seq` Prelude.rnf alias
      `Prelude.seq` Prelude.rnf source

instance Data.ToJSON LogicalTable where
  toJSON LogicalTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataTransforms" Data..=)
              Prelude.<$> dataTransforms,
            Prelude.Just ("Alias" Data..= alias),
            Prelude.Just ("Source" Data..= source)
          ]
      )
