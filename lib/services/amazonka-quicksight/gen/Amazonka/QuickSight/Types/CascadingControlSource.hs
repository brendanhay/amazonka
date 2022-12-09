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
-- Module      : Amazonka.QuickSight.Types.CascadingControlSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CascadingControlSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier

-- | The source controls that are used in a @CascadingControlConfiguration@.
--
-- /See:/ 'newCascadingControlSource' smart constructor.
data CascadingControlSource = CascadingControlSource'
  { -- | The column identifier that determines which column to look up for the
    -- source sheet control.
    columnToMatch :: Prelude.Maybe ColumnIdentifier,
    -- | The source sheet control ID of a @CascadingControlSource@.
    sourceSheetControlId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CascadingControlSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnToMatch', 'cascadingControlSource_columnToMatch' - The column identifier that determines which column to look up for the
-- source sheet control.
--
-- 'sourceSheetControlId', 'cascadingControlSource_sourceSheetControlId' - The source sheet control ID of a @CascadingControlSource@.
newCascadingControlSource ::
  CascadingControlSource
newCascadingControlSource =
  CascadingControlSource'
    { columnToMatch =
        Prelude.Nothing,
      sourceSheetControlId = Prelude.Nothing
    }

-- | The column identifier that determines which column to look up for the
-- source sheet control.
cascadingControlSource_columnToMatch :: Lens.Lens' CascadingControlSource (Prelude.Maybe ColumnIdentifier)
cascadingControlSource_columnToMatch = Lens.lens (\CascadingControlSource' {columnToMatch} -> columnToMatch) (\s@CascadingControlSource' {} a -> s {columnToMatch = a} :: CascadingControlSource)

-- | The source sheet control ID of a @CascadingControlSource@.
cascadingControlSource_sourceSheetControlId :: Lens.Lens' CascadingControlSource (Prelude.Maybe Prelude.Text)
cascadingControlSource_sourceSheetControlId = Lens.lens (\CascadingControlSource' {sourceSheetControlId} -> sourceSheetControlId) (\s@CascadingControlSource' {} a -> s {sourceSheetControlId = a} :: CascadingControlSource)

instance Data.FromJSON CascadingControlSource where
  parseJSON =
    Data.withObject
      "CascadingControlSource"
      ( \x ->
          CascadingControlSource'
            Prelude.<$> (x Data..:? "ColumnToMatch")
            Prelude.<*> (x Data..:? "SourceSheetControlId")
      )

instance Prelude.Hashable CascadingControlSource where
  hashWithSalt _salt CascadingControlSource' {..} =
    _salt `Prelude.hashWithSalt` columnToMatch
      `Prelude.hashWithSalt` sourceSheetControlId

instance Prelude.NFData CascadingControlSource where
  rnf CascadingControlSource' {..} =
    Prelude.rnf columnToMatch
      `Prelude.seq` Prelude.rnf sourceSheetControlId

instance Data.ToJSON CascadingControlSource where
  toJSON CascadingControlSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColumnToMatch" Data..=) Prelude.<$> columnToMatch,
            ("SourceSheetControlId" Data..=)
              Prelude.<$> sourceSheetControlId
          ]
      )
