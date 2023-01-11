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
-- Module      : Amazonka.QuickSight.Types.FilterOperationTargetVisualsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterOperationTargetVisualsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SameSheetTargetVisualConfiguration

-- | The configuration of target visuals that you want to be filtered.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newFilterOperationTargetVisualsConfiguration' smart constructor.
data FilterOperationTargetVisualsConfiguration = FilterOperationTargetVisualsConfiguration'
  { -- | The configuration of the same-sheet target visuals that you want to be
    -- filtered.
    sameSheetTargetVisualConfiguration :: Prelude.Maybe SameSheetTargetVisualConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterOperationTargetVisualsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sameSheetTargetVisualConfiguration', 'filterOperationTargetVisualsConfiguration_sameSheetTargetVisualConfiguration' - The configuration of the same-sheet target visuals that you want to be
-- filtered.
newFilterOperationTargetVisualsConfiguration ::
  FilterOperationTargetVisualsConfiguration
newFilterOperationTargetVisualsConfiguration =
  FilterOperationTargetVisualsConfiguration'
    { sameSheetTargetVisualConfiguration =
        Prelude.Nothing
    }

-- | The configuration of the same-sheet target visuals that you want to be
-- filtered.
filterOperationTargetVisualsConfiguration_sameSheetTargetVisualConfiguration :: Lens.Lens' FilterOperationTargetVisualsConfiguration (Prelude.Maybe SameSheetTargetVisualConfiguration)
filterOperationTargetVisualsConfiguration_sameSheetTargetVisualConfiguration = Lens.lens (\FilterOperationTargetVisualsConfiguration' {sameSheetTargetVisualConfiguration} -> sameSheetTargetVisualConfiguration) (\s@FilterOperationTargetVisualsConfiguration' {} a -> s {sameSheetTargetVisualConfiguration = a} :: FilterOperationTargetVisualsConfiguration)

instance
  Data.FromJSON
    FilterOperationTargetVisualsConfiguration
  where
  parseJSON =
    Data.withObject
      "FilterOperationTargetVisualsConfiguration"
      ( \x ->
          FilterOperationTargetVisualsConfiguration'
            Prelude.<$> (x Data..:? "SameSheetTargetVisualConfiguration")
      )

instance
  Prelude.Hashable
    FilterOperationTargetVisualsConfiguration
  where
  hashWithSalt
    _salt
    FilterOperationTargetVisualsConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` sameSheetTargetVisualConfiguration

instance
  Prelude.NFData
    FilterOperationTargetVisualsConfiguration
  where
  rnf FilterOperationTargetVisualsConfiguration' {..} =
    Prelude.rnf sameSheetTargetVisualConfiguration

instance
  Data.ToJSON
    FilterOperationTargetVisualsConfiguration
  where
  toJSON FilterOperationTargetVisualsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SameSheetTargetVisualConfiguration" Data..=)
              Prelude.<$> sameSheetTargetVisualConfiguration
          ]
      )
