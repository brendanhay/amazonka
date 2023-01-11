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
-- Module      : Amazonka.QuickSight.Types.ProjectOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ProjectOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A transform operation that projects columns. Operations that come after
-- a projection can only refer to projected columns.
--
-- /See:/ 'newProjectOperation' smart constructor.
data ProjectOperation = ProjectOperation'
  { -- | Projected columns.
    projectedColumns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectedColumns', 'projectOperation_projectedColumns' - Projected columns.
newProjectOperation ::
  -- | 'projectedColumns'
  Prelude.NonEmpty Prelude.Text ->
  ProjectOperation
newProjectOperation pProjectedColumns_ =
  ProjectOperation'
    { projectedColumns =
        Lens.coerced Lens.# pProjectedColumns_
    }

-- | Projected columns.
projectOperation_projectedColumns :: Lens.Lens' ProjectOperation (Prelude.NonEmpty Prelude.Text)
projectOperation_projectedColumns = Lens.lens (\ProjectOperation' {projectedColumns} -> projectedColumns) (\s@ProjectOperation' {} a -> s {projectedColumns = a} :: ProjectOperation) Prelude.. Lens.coerced

instance Data.FromJSON ProjectOperation where
  parseJSON =
    Data.withObject
      "ProjectOperation"
      ( \x ->
          ProjectOperation'
            Prelude.<$> (x Data..: "ProjectedColumns")
      )

instance Prelude.Hashable ProjectOperation where
  hashWithSalt _salt ProjectOperation' {..} =
    _salt `Prelude.hashWithSalt` projectedColumns

instance Prelude.NFData ProjectOperation where
  rnf ProjectOperation' {..} =
    Prelude.rnf projectedColumns

instance Data.ToJSON ProjectOperation where
  toJSON ProjectOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProjectedColumns" Data..= projectedColumns)
          ]
      )
