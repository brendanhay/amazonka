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
-- Module      : Amazonka.QuickSight.Types.PredefinedHierarchy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PredefinedHierarchy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.DrillDownFilter

-- | The option that determines the hierarchy of the fields that are defined
-- during data preparation. These fields are available to use in any
-- analysis that uses the data source.
--
-- /See:/ 'newPredefinedHierarchy' smart constructor.
data PredefinedHierarchy = PredefinedHierarchy'
  { -- | The option that determines the drill down filters for the predefined
    -- hierarchy.
    drillDownFilters :: Prelude.Maybe [DrillDownFilter],
    -- | The hierarchy ID of the predefined hierarchy.
    hierarchyId :: Prelude.Text,
    -- | The list of columns that define the predefined hierarchy.
    columns :: Prelude.NonEmpty ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredefinedHierarchy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'drillDownFilters', 'predefinedHierarchy_drillDownFilters' - The option that determines the drill down filters for the predefined
-- hierarchy.
--
-- 'hierarchyId', 'predefinedHierarchy_hierarchyId' - The hierarchy ID of the predefined hierarchy.
--
-- 'columns', 'predefinedHierarchy_columns' - The list of columns that define the predefined hierarchy.
newPredefinedHierarchy ::
  -- | 'hierarchyId'
  Prelude.Text ->
  -- | 'columns'
  Prelude.NonEmpty ColumnIdentifier ->
  PredefinedHierarchy
newPredefinedHierarchy pHierarchyId_ pColumns_ =
  PredefinedHierarchy'
    { drillDownFilters =
        Prelude.Nothing,
      hierarchyId = pHierarchyId_,
      columns = Lens.coerced Lens.# pColumns_
    }

-- | The option that determines the drill down filters for the predefined
-- hierarchy.
predefinedHierarchy_drillDownFilters :: Lens.Lens' PredefinedHierarchy (Prelude.Maybe [DrillDownFilter])
predefinedHierarchy_drillDownFilters = Lens.lens (\PredefinedHierarchy' {drillDownFilters} -> drillDownFilters) (\s@PredefinedHierarchy' {} a -> s {drillDownFilters = a} :: PredefinedHierarchy) Prelude.. Lens.mapping Lens.coerced

-- | The hierarchy ID of the predefined hierarchy.
predefinedHierarchy_hierarchyId :: Lens.Lens' PredefinedHierarchy Prelude.Text
predefinedHierarchy_hierarchyId = Lens.lens (\PredefinedHierarchy' {hierarchyId} -> hierarchyId) (\s@PredefinedHierarchy' {} a -> s {hierarchyId = a} :: PredefinedHierarchy)

-- | The list of columns that define the predefined hierarchy.
predefinedHierarchy_columns :: Lens.Lens' PredefinedHierarchy (Prelude.NonEmpty ColumnIdentifier)
predefinedHierarchy_columns = Lens.lens (\PredefinedHierarchy' {columns} -> columns) (\s@PredefinedHierarchy' {} a -> s {columns = a} :: PredefinedHierarchy) Prelude.. Lens.coerced

instance Data.FromJSON PredefinedHierarchy where
  parseJSON =
    Data.withObject
      "PredefinedHierarchy"
      ( \x ->
          PredefinedHierarchy'
            Prelude.<$> ( x
                            Data..:? "DrillDownFilters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "HierarchyId")
            Prelude.<*> (x Data..: "Columns")
      )

instance Prelude.Hashable PredefinedHierarchy where
  hashWithSalt _salt PredefinedHierarchy' {..} =
    _salt
      `Prelude.hashWithSalt` drillDownFilters
      `Prelude.hashWithSalt` hierarchyId
      `Prelude.hashWithSalt` columns

instance Prelude.NFData PredefinedHierarchy where
  rnf PredefinedHierarchy' {..} =
    Prelude.rnf drillDownFilters `Prelude.seq`
      Prelude.rnf hierarchyId `Prelude.seq`
        Prelude.rnf columns

instance Data.ToJSON PredefinedHierarchy where
  toJSON PredefinedHierarchy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DrillDownFilters" Data..=)
              Prelude.<$> drillDownFilters,
            Prelude.Just ("HierarchyId" Data..= hierarchyId),
            Prelude.Just ("Columns" Data..= columns)
          ]
      )
