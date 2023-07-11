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
-- Module      : Amazonka.QuickSight.Types.GridLayoutElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GridLayoutElement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LayoutElementType

-- | An element within a grid layout.
--
-- /See:/ 'newGridLayoutElement' smart constructor.
data GridLayoutElement = GridLayoutElement'
  { -- | The column index for the upper left corner of an element.
    columnIndex :: Prelude.Maybe Prelude.Natural,
    -- | The row index for the upper left corner of an element.
    rowIndex :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for an element within a grid layout.
    elementId :: Prelude.Text,
    -- | The type of element.
    elementType :: LayoutElementType,
    -- | The width of a grid element expressed as a number of grid columns.
    columnSpan :: Prelude.Natural,
    -- | The height of a grid element expressed as a number of grid rows.
    rowSpan :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GridLayoutElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnIndex', 'gridLayoutElement_columnIndex' - The column index for the upper left corner of an element.
--
-- 'rowIndex', 'gridLayoutElement_rowIndex' - The row index for the upper left corner of an element.
--
-- 'elementId', 'gridLayoutElement_elementId' - A unique identifier for an element within a grid layout.
--
-- 'elementType', 'gridLayoutElement_elementType' - The type of element.
--
-- 'columnSpan', 'gridLayoutElement_columnSpan' - The width of a grid element expressed as a number of grid columns.
--
-- 'rowSpan', 'gridLayoutElement_rowSpan' - The height of a grid element expressed as a number of grid rows.
newGridLayoutElement ::
  -- | 'elementId'
  Prelude.Text ->
  -- | 'elementType'
  LayoutElementType ->
  -- | 'columnSpan'
  Prelude.Natural ->
  -- | 'rowSpan'
  Prelude.Natural ->
  GridLayoutElement
newGridLayoutElement
  pElementId_
  pElementType_
  pColumnSpan_
  pRowSpan_ =
    GridLayoutElement'
      { columnIndex = Prelude.Nothing,
        rowIndex = Prelude.Nothing,
        elementId = pElementId_,
        elementType = pElementType_,
        columnSpan = pColumnSpan_,
        rowSpan = pRowSpan_
      }

-- | The column index for the upper left corner of an element.
gridLayoutElement_columnIndex :: Lens.Lens' GridLayoutElement (Prelude.Maybe Prelude.Natural)
gridLayoutElement_columnIndex = Lens.lens (\GridLayoutElement' {columnIndex} -> columnIndex) (\s@GridLayoutElement' {} a -> s {columnIndex = a} :: GridLayoutElement)

-- | The row index for the upper left corner of an element.
gridLayoutElement_rowIndex :: Lens.Lens' GridLayoutElement (Prelude.Maybe Prelude.Natural)
gridLayoutElement_rowIndex = Lens.lens (\GridLayoutElement' {rowIndex} -> rowIndex) (\s@GridLayoutElement' {} a -> s {rowIndex = a} :: GridLayoutElement)

-- | A unique identifier for an element within a grid layout.
gridLayoutElement_elementId :: Lens.Lens' GridLayoutElement Prelude.Text
gridLayoutElement_elementId = Lens.lens (\GridLayoutElement' {elementId} -> elementId) (\s@GridLayoutElement' {} a -> s {elementId = a} :: GridLayoutElement)

-- | The type of element.
gridLayoutElement_elementType :: Lens.Lens' GridLayoutElement LayoutElementType
gridLayoutElement_elementType = Lens.lens (\GridLayoutElement' {elementType} -> elementType) (\s@GridLayoutElement' {} a -> s {elementType = a} :: GridLayoutElement)

-- | The width of a grid element expressed as a number of grid columns.
gridLayoutElement_columnSpan :: Lens.Lens' GridLayoutElement Prelude.Natural
gridLayoutElement_columnSpan = Lens.lens (\GridLayoutElement' {columnSpan} -> columnSpan) (\s@GridLayoutElement' {} a -> s {columnSpan = a} :: GridLayoutElement)

-- | The height of a grid element expressed as a number of grid rows.
gridLayoutElement_rowSpan :: Lens.Lens' GridLayoutElement Prelude.Natural
gridLayoutElement_rowSpan = Lens.lens (\GridLayoutElement' {rowSpan} -> rowSpan) (\s@GridLayoutElement' {} a -> s {rowSpan = a} :: GridLayoutElement)

instance Data.FromJSON GridLayoutElement where
  parseJSON =
    Data.withObject
      "GridLayoutElement"
      ( \x ->
          GridLayoutElement'
            Prelude.<$> (x Data..:? "ColumnIndex")
            Prelude.<*> (x Data..:? "RowIndex")
            Prelude.<*> (x Data..: "ElementId")
            Prelude.<*> (x Data..: "ElementType")
            Prelude.<*> (x Data..: "ColumnSpan")
            Prelude.<*> (x Data..: "RowSpan")
      )

instance Prelude.Hashable GridLayoutElement where
  hashWithSalt _salt GridLayoutElement' {..} =
    _salt
      `Prelude.hashWithSalt` columnIndex
      `Prelude.hashWithSalt` rowIndex
      `Prelude.hashWithSalt` elementId
      `Prelude.hashWithSalt` elementType
      `Prelude.hashWithSalt` columnSpan
      `Prelude.hashWithSalt` rowSpan

instance Prelude.NFData GridLayoutElement where
  rnf GridLayoutElement' {..} =
    Prelude.rnf columnIndex
      `Prelude.seq` Prelude.rnf rowIndex
      `Prelude.seq` Prelude.rnf elementId
      `Prelude.seq` Prelude.rnf elementType
      `Prelude.seq` Prelude.rnf columnSpan
      `Prelude.seq` Prelude.rnf rowSpan

instance Data.ToJSON GridLayoutElement where
  toJSON GridLayoutElement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColumnIndex" Data..=) Prelude.<$> columnIndex,
            ("RowIndex" Data..=) Prelude.<$> rowIndex,
            Prelude.Just ("ElementId" Data..= elementId),
            Prelude.Just ("ElementType" Data..= elementType),
            Prelude.Just ("ColumnSpan" Data..= columnSpan),
            Prelude.Just ("RowSpan" Data..= rowSpan)
          ]
      )
