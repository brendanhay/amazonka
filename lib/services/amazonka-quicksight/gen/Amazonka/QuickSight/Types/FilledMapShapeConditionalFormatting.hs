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
-- Module      : Amazonka.QuickSight.Types.FilledMapShapeConditionalFormatting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilledMapShapeConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ShapeConditionalFormat

-- | The conditional formatting that determines the shape of the filled map.
--
-- /See:/ 'newFilledMapShapeConditionalFormatting' smart constructor.
data FilledMapShapeConditionalFormatting = FilledMapShapeConditionalFormatting'
  { -- | The conditional formatting that determines the background color of a
    -- filled map\'s shape.
    format :: Prelude.Maybe ShapeConditionalFormat,
    -- | The field ID of the filled map shape.
    fieldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilledMapShapeConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'filledMapShapeConditionalFormatting_format' - The conditional formatting that determines the background color of a
-- filled map\'s shape.
--
-- 'fieldId', 'filledMapShapeConditionalFormatting_fieldId' - The field ID of the filled map shape.
newFilledMapShapeConditionalFormatting ::
  -- | 'fieldId'
  Prelude.Text ->
  FilledMapShapeConditionalFormatting
newFilledMapShapeConditionalFormatting pFieldId_ =
  FilledMapShapeConditionalFormatting'
    { format =
        Prelude.Nothing,
      fieldId = pFieldId_
    }

-- | The conditional formatting that determines the background color of a
-- filled map\'s shape.
filledMapShapeConditionalFormatting_format :: Lens.Lens' FilledMapShapeConditionalFormatting (Prelude.Maybe ShapeConditionalFormat)
filledMapShapeConditionalFormatting_format = Lens.lens (\FilledMapShapeConditionalFormatting' {format} -> format) (\s@FilledMapShapeConditionalFormatting' {} a -> s {format = a} :: FilledMapShapeConditionalFormatting)

-- | The field ID of the filled map shape.
filledMapShapeConditionalFormatting_fieldId :: Lens.Lens' FilledMapShapeConditionalFormatting Prelude.Text
filledMapShapeConditionalFormatting_fieldId = Lens.lens (\FilledMapShapeConditionalFormatting' {fieldId} -> fieldId) (\s@FilledMapShapeConditionalFormatting' {} a -> s {fieldId = a} :: FilledMapShapeConditionalFormatting)

instance
  Data.FromJSON
    FilledMapShapeConditionalFormatting
  where
  parseJSON =
    Data.withObject
      "FilledMapShapeConditionalFormatting"
      ( \x ->
          FilledMapShapeConditionalFormatting'
            Prelude.<$> (x Data..:? "Format")
            Prelude.<*> (x Data..: "FieldId")
      )

instance
  Prelude.Hashable
    FilledMapShapeConditionalFormatting
  where
  hashWithSalt
    _salt
    FilledMapShapeConditionalFormatting' {..} =
      _salt `Prelude.hashWithSalt` format
        `Prelude.hashWithSalt` fieldId

instance
  Prelude.NFData
    FilledMapShapeConditionalFormatting
  where
  rnf FilledMapShapeConditionalFormatting' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf fieldId

instance
  Data.ToJSON
    FilledMapShapeConditionalFormatting
  where
  toJSON FilledMapShapeConditionalFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Format" Data..=) Prelude.<$> format,
            Prelude.Just ("FieldId" Data..= fieldId)
          ]
      )
