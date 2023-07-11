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
-- Module      : Amazonka.QuickSight.Types.DateTimeHierarchy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateTimeHierarchy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DrillDownFilter

-- | The option that determines the hierarchy of any @DateTime@ fields.
--
-- /See:/ 'newDateTimeHierarchy' smart constructor.
data DateTimeHierarchy = DateTimeHierarchy'
  { -- | The option that determines the drill down filters for the @DateTime@
    -- hierarchy.
    drillDownFilters :: Prelude.Maybe [DrillDownFilter],
    -- | The hierarchy ID of the @DateTime@ hierarchy.
    hierarchyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateTimeHierarchy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'drillDownFilters', 'dateTimeHierarchy_drillDownFilters' - The option that determines the drill down filters for the @DateTime@
-- hierarchy.
--
-- 'hierarchyId', 'dateTimeHierarchy_hierarchyId' - The hierarchy ID of the @DateTime@ hierarchy.
newDateTimeHierarchy ::
  -- | 'hierarchyId'
  Prelude.Text ->
  DateTimeHierarchy
newDateTimeHierarchy pHierarchyId_ =
  DateTimeHierarchy'
    { drillDownFilters =
        Prelude.Nothing,
      hierarchyId = pHierarchyId_
    }

-- | The option that determines the drill down filters for the @DateTime@
-- hierarchy.
dateTimeHierarchy_drillDownFilters :: Lens.Lens' DateTimeHierarchy (Prelude.Maybe [DrillDownFilter])
dateTimeHierarchy_drillDownFilters = Lens.lens (\DateTimeHierarchy' {drillDownFilters} -> drillDownFilters) (\s@DateTimeHierarchy' {} a -> s {drillDownFilters = a} :: DateTimeHierarchy) Prelude.. Lens.mapping Lens.coerced

-- | The hierarchy ID of the @DateTime@ hierarchy.
dateTimeHierarchy_hierarchyId :: Lens.Lens' DateTimeHierarchy Prelude.Text
dateTimeHierarchy_hierarchyId = Lens.lens (\DateTimeHierarchy' {hierarchyId} -> hierarchyId) (\s@DateTimeHierarchy' {} a -> s {hierarchyId = a} :: DateTimeHierarchy)

instance Data.FromJSON DateTimeHierarchy where
  parseJSON =
    Data.withObject
      "DateTimeHierarchy"
      ( \x ->
          DateTimeHierarchy'
            Prelude.<$> ( x
                            Data..:? "DrillDownFilters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "HierarchyId")
      )

instance Prelude.Hashable DateTimeHierarchy where
  hashWithSalt _salt DateTimeHierarchy' {..} =
    _salt
      `Prelude.hashWithSalt` drillDownFilters
      `Prelude.hashWithSalt` hierarchyId

instance Prelude.NFData DateTimeHierarchy where
  rnf DateTimeHierarchy' {..} =
    Prelude.rnf drillDownFilters
      `Prelude.seq` Prelude.rnf hierarchyId

instance Data.ToJSON DateTimeHierarchy where
  toJSON DateTimeHierarchy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DrillDownFilters" Data..=)
              Prelude.<$> drillDownFilters,
            Prelude.Just ("HierarchyId" Data..= hierarchyId)
          ]
      )
