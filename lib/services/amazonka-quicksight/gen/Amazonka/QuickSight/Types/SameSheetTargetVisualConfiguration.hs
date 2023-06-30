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
-- Module      : Amazonka.QuickSight.Types.SameSheetTargetVisualConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SameSheetTargetVisualConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TargetVisualOptions

-- | The configuration of the same-sheet target visuals that you want to be
-- filtered.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newSameSheetTargetVisualConfiguration' smart constructor.
data SameSheetTargetVisualConfiguration = SameSheetTargetVisualConfiguration'
  { -- | The options that choose the target visual in the same sheet.
    --
    -- Valid values are defined as follows:
    --
    -- -   @ALL_VISUALS@: Applies the filter operation to all visuals in the
    --     same sheet.
    targetVisualOptions :: Prelude.Maybe TargetVisualOptions,
    -- | A list of the target visual IDs that are located in the same sheet of
    -- the analysis.
    targetVisuals :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SameSheetTargetVisualConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetVisualOptions', 'sameSheetTargetVisualConfiguration_targetVisualOptions' - The options that choose the target visual in the same sheet.
--
-- Valid values are defined as follows:
--
-- -   @ALL_VISUALS@: Applies the filter operation to all visuals in the
--     same sheet.
--
-- 'targetVisuals', 'sameSheetTargetVisualConfiguration_targetVisuals' - A list of the target visual IDs that are located in the same sheet of
-- the analysis.
newSameSheetTargetVisualConfiguration ::
  SameSheetTargetVisualConfiguration
newSameSheetTargetVisualConfiguration =
  SameSheetTargetVisualConfiguration'
    { targetVisualOptions =
        Prelude.Nothing,
      targetVisuals = Prelude.Nothing
    }

-- | The options that choose the target visual in the same sheet.
--
-- Valid values are defined as follows:
--
-- -   @ALL_VISUALS@: Applies the filter operation to all visuals in the
--     same sheet.
sameSheetTargetVisualConfiguration_targetVisualOptions :: Lens.Lens' SameSheetTargetVisualConfiguration (Prelude.Maybe TargetVisualOptions)
sameSheetTargetVisualConfiguration_targetVisualOptions = Lens.lens (\SameSheetTargetVisualConfiguration' {targetVisualOptions} -> targetVisualOptions) (\s@SameSheetTargetVisualConfiguration' {} a -> s {targetVisualOptions = a} :: SameSheetTargetVisualConfiguration)

-- | A list of the target visual IDs that are located in the same sheet of
-- the analysis.
sameSheetTargetVisualConfiguration_targetVisuals :: Lens.Lens' SameSheetTargetVisualConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
sameSheetTargetVisualConfiguration_targetVisuals = Lens.lens (\SameSheetTargetVisualConfiguration' {targetVisuals} -> targetVisuals) (\s@SameSheetTargetVisualConfiguration' {} a -> s {targetVisuals = a} :: SameSheetTargetVisualConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    SameSheetTargetVisualConfiguration
  where
  parseJSON =
    Data.withObject
      "SameSheetTargetVisualConfiguration"
      ( \x ->
          SameSheetTargetVisualConfiguration'
            Prelude.<$> (x Data..:? "TargetVisualOptions")
            Prelude.<*> (x Data..:? "TargetVisuals")
      )

instance
  Prelude.Hashable
    SameSheetTargetVisualConfiguration
  where
  hashWithSalt
    _salt
    SameSheetTargetVisualConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` targetVisualOptions
        `Prelude.hashWithSalt` targetVisuals

instance
  Prelude.NFData
    SameSheetTargetVisualConfiguration
  where
  rnf SameSheetTargetVisualConfiguration' {..} =
    Prelude.rnf targetVisualOptions
      `Prelude.seq` Prelude.rnf targetVisuals

instance
  Data.ToJSON
    SameSheetTargetVisualConfiguration
  where
  toJSON SameSheetTargetVisualConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetVisualOptions" Data..=)
              Prelude.<$> targetVisualOptions,
            ("TargetVisuals" Data..=) Prelude.<$> targetVisuals
          ]
      )
