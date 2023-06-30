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
-- Module      : Amazonka.QuickSight.Types.ExplicitHierarchy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ExplicitHierarchy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.DrillDownFilter

-- | The option that determines the hierarchy of the fields that are built
-- within a visual\'s field wells. These fields can\'t be duplicated to
-- other visuals.
--
-- /See:/ 'newExplicitHierarchy' smart constructor.
data ExplicitHierarchy = ExplicitHierarchy'
  { -- | The option that determines the drill down filters for the explicit
    -- hierarchy.
    drillDownFilters :: Prelude.Maybe [DrillDownFilter],
    -- | The hierarchy ID of the explicit hierarchy.
    hierarchyId :: Prelude.Text,
    -- | The list of columns that define the explicit hierarchy.
    columns :: Prelude.NonEmpty ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExplicitHierarchy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'drillDownFilters', 'explicitHierarchy_drillDownFilters' - The option that determines the drill down filters for the explicit
-- hierarchy.
--
-- 'hierarchyId', 'explicitHierarchy_hierarchyId' - The hierarchy ID of the explicit hierarchy.
--
-- 'columns', 'explicitHierarchy_columns' - The list of columns that define the explicit hierarchy.
newExplicitHierarchy ::
  -- | 'hierarchyId'
  Prelude.Text ->
  -- | 'columns'
  Prelude.NonEmpty ColumnIdentifier ->
  ExplicitHierarchy
newExplicitHierarchy pHierarchyId_ pColumns_ =
  ExplicitHierarchy'
    { drillDownFilters =
        Prelude.Nothing,
      hierarchyId = pHierarchyId_,
      columns = Lens.coerced Lens.# pColumns_
    }

-- | The option that determines the drill down filters for the explicit
-- hierarchy.
explicitHierarchy_drillDownFilters :: Lens.Lens' ExplicitHierarchy (Prelude.Maybe [DrillDownFilter])
explicitHierarchy_drillDownFilters = Lens.lens (\ExplicitHierarchy' {drillDownFilters} -> drillDownFilters) (\s@ExplicitHierarchy' {} a -> s {drillDownFilters = a} :: ExplicitHierarchy) Prelude.. Lens.mapping Lens.coerced

-- | The hierarchy ID of the explicit hierarchy.
explicitHierarchy_hierarchyId :: Lens.Lens' ExplicitHierarchy Prelude.Text
explicitHierarchy_hierarchyId = Lens.lens (\ExplicitHierarchy' {hierarchyId} -> hierarchyId) (\s@ExplicitHierarchy' {} a -> s {hierarchyId = a} :: ExplicitHierarchy)

-- | The list of columns that define the explicit hierarchy.
explicitHierarchy_columns :: Lens.Lens' ExplicitHierarchy (Prelude.NonEmpty ColumnIdentifier)
explicitHierarchy_columns = Lens.lens (\ExplicitHierarchy' {columns} -> columns) (\s@ExplicitHierarchy' {} a -> s {columns = a} :: ExplicitHierarchy) Prelude.. Lens.coerced

instance Data.FromJSON ExplicitHierarchy where
  parseJSON =
    Data.withObject
      "ExplicitHierarchy"
      ( \x ->
          ExplicitHierarchy'
            Prelude.<$> ( x
                            Data..:? "DrillDownFilters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "HierarchyId")
            Prelude.<*> (x Data..: "Columns")
      )

instance Prelude.Hashable ExplicitHierarchy where
  hashWithSalt _salt ExplicitHierarchy' {..} =
    _salt
      `Prelude.hashWithSalt` drillDownFilters
      `Prelude.hashWithSalt` hierarchyId
      `Prelude.hashWithSalt` columns

instance Prelude.NFData ExplicitHierarchy where
  rnf ExplicitHierarchy' {..} =
    Prelude.rnf drillDownFilters
      `Prelude.seq` Prelude.rnf hierarchyId
      `Prelude.seq` Prelude.rnf columns

instance Data.ToJSON ExplicitHierarchy where
  toJSON ExplicitHierarchy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DrillDownFilters" Data..=)
              Prelude.<$> drillDownFilters,
            Prelude.Just ("HierarchyId" Data..= hierarchyId),
            Prelude.Just ("Columns" Data..= columns)
          ]
      )
