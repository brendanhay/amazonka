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
-- Module      : Amazonka.QuickSight.Types.ColumnHierarchy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnHierarchy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DateTimeHierarchy
import Amazonka.QuickSight.Types.ExplicitHierarchy
import Amazonka.QuickSight.Types.PredefinedHierarchy

-- | The option that determines the hierarchy of the fields for a visual
-- element.
--
-- /See:/ 'newColumnHierarchy' smart constructor.
data ColumnHierarchy = ColumnHierarchy'
  { -- | The option that determines the hierarchy of any @DateTime@ fields.
    dateTimeHierarchy :: Prelude.Maybe DateTimeHierarchy,
    -- | The option that determines the hierarchy of the fields that are built
    -- within a visual\'s field wells. These fields can\'t be duplicated to
    -- other visuals.
    explicitHierarchy :: Prelude.Maybe ExplicitHierarchy,
    -- | The option that determines the hierarchy of the fields that are defined
    -- during data preparation. These fields are available to use in any
    -- analysis that uses the data source.
    predefinedHierarchy :: Prelude.Maybe PredefinedHierarchy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnHierarchy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateTimeHierarchy', 'columnHierarchy_dateTimeHierarchy' - The option that determines the hierarchy of any @DateTime@ fields.
--
-- 'explicitHierarchy', 'columnHierarchy_explicitHierarchy' - The option that determines the hierarchy of the fields that are built
-- within a visual\'s field wells. These fields can\'t be duplicated to
-- other visuals.
--
-- 'predefinedHierarchy', 'columnHierarchy_predefinedHierarchy' - The option that determines the hierarchy of the fields that are defined
-- during data preparation. These fields are available to use in any
-- analysis that uses the data source.
newColumnHierarchy ::
  ColumnHierarchy
newColumnHierarchy =
  ColumnHierarchy'
    { dateTimeHierarchy =
        Prelude.Nothing,
      explicitHierarchy = Prelude.Nothing,
      predefinedHierarchy = Prelude.Nothing
    }

-- | The option that determines the hierarchy of any @DateTime@ fields.
columnHierarchy_dateTimeHierarchy :: Lens.Lens' ColumnHierarchy (Prelude.Maybe DateTimeHierarchy)
columnHierarchy_dateTimeHierarchy = Lens.lens (\ColumnHierarchy' {dateTimeHierarchy} -> dateTimeHierarchy) (\s@ColumnHierarchy' {} a -> s {dateTimeHierarchy = a} :: ColumnHierarchy)

-- | The option that determines the hierarchy of the fields that are built
-- within a visual\'s field wells. These fields can\'t be duplicated to
-- other visuals.
columnHierarchy_explicitHierarchy :: Lens.Lens' ColumnHierarchy (Prelude.Maybe ExplicitHierarchy)
columnHierarchy_explicitHierarchy = Lens.lens (\ColumnHierarchy' {explicitHierarchy} -> explicitHierarchy) (\s@ColumnHierarchy' {} a -> s {explicitHierarchy = a} :: ColumnHierarchy)

-- | The option that determines the hierarchy of the fields that are defined
-- during data preparation. These fields are available to use in any
-- analysis that uses the data source.
columnHierarchy_predefinedHierarchy :: Lens.Lens' ColumnHierarchy (Prelude.Maybe PredefinedHierarchy)
columnHierarchy_predefinedHierarchy = Lens.lens (\ColumnHierarchy' {predefinedHierarchy} -> predefinedHierarchy) (\s@ColumnHierarchy' {} a -> s {predefinedHierarchy = a} :: ColumnHierarchy)

instance Data.FromJSON ColumnHierarchy where
  parseJSON =
    Data.withObject
      "ColumnHierarchy"
      ( \x ->
          ColumnHierarchy'
            Prelude.<$> (x Data..:? "DateTimeHierarchy")
            Prelude.<*> (x Data..:? "ExplicitHierarchy")
            Prelude.<*> (x Data..:? "PredefinedHierarchy")
      )

instance Prelude.Hashable ColumnHierarchy where
  hashWithSalt _salt ColumnHierarchy' {..} =
    _salt `Prelude.hashWithSalt` dateTimeHierarchy
      `Prelude.hashWithSalt` explicitHierarchy
      `Prelude.hashWithSalt` predefinedHierarchy

instance Prelude.NFData ColumnHierarchy where
  rnf ColumnHierarchy' {..} =
    Prelude.rnf dateTimeHierarchy
      `Prelude.seq` Prelude.rnf explicitHierarchy
      `Prelude.seq` Prelude.rnf predefinedHierarchy

instance Data.ToJSON ColumnHierarchy where
  toJSON ColumnHierarchy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateTimeHierarchy" Data..=)
              Prelude.<$> dateTimeHierarchy,
            ("ExplicitHierarchy" Data..=)
              Prelude.<$> explicitHierarchy,
            ("PredefinedHierarchy" Data..=)
              Prelude.<$> predefinedHierarchy
          ]
      )
