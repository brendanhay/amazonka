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
-- Module      : Amazonka.QuickSight.Types.FilledMapFieldWells
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilledMapFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FilledMapAggregatedFieldWells

-- | The field wells of a @FilledMapVisual@.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newFilledMapFieldWells' smart constructor.
data FilledMapFieldWells = FilledMapFieldWells'
  { -- | The aggregated field well of the filled map.
    filledMapAggregatedFieldWells :: Prelude.Maybe FilledMapAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilledMapFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filledMapAggregatedFieldWells', 'filledMapFieldWells_filledMapAggregatedFieldWells' - The aggregated field well of the filled map.
newFilledMapFieldWells ::
  FilledMapFieldWells
newFilledMapFieldWells =
  FilledMapFieldWells'
    { filledMapAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The aggregated field well of the filled map.
filledMapFieldWells_filledMapAggregatedFieldWells :: Lens.Lens' FilledMapFieldWells (Prelude.Maybe FilledMapAggregatedFieldWells)
filledMapFieldWells_filledMapAggregatedFieldWells = Lens.lens (\FilledMapFieldWells' {filledMapAggregatedFieldWells} -> filledMapAggregatedFieldWells) (\s@FilledMapFieldWells' {} a -> s {filledMapAggregatedFieldWells = a} :: FilledMapFieldWells)

instance Data.FromJSON FilledMapFieldWells where
  parseJSON =
    Data.withObject
      "FilledMapFieldWells"
      ( \x ->
          FilledMapFieldWells'
            Prelude.<$> (x Data..:? "FilledMapAggregatedFieldWells")
      )

instance Prelude.Hashable FilledMapFieldWells where
  hashWithSalt _salt FilledMapFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` filledMapAggregatedFieldWells

instance Prelude.NFData FilledMapFieldWells where
  rnf FilledMapFieldWells' {..} =
    Prelude.rnf filledMapAggregatedFieldWells

instance Data.ToJSON FilledMapFieldWells where
  toJSON FilledMapFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilledMapAggregatedFieldWells" Data..=)
              Prelude.<$> filledMapAggregatedFieldWells
          ]
      )
