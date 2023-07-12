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
-- Module      : Amazonka.QuickSight.Types.TableFieldOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableFieldOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableFieldURLConfiguration
import Amazonka.QuickSight.Types.Visibility

-- | The options for a table field.
--
-- /See:/ 'newTableFieldOption' smart constructor.
data TableFieldOption = TableFieldOption'
  { -- | The custom label for a table field.
    customLabel :: Prelude.Maybe Prelude.Text,
    -- | The URL configuration for a table field.
    uRLStyling :: Prelude.Maybe TableFieldURLConfiguration,
    -- | The visibility of a table field.
    visibility :: Prelude.Maybe Visibility,
    -- | The width for a table field.
    width :: Prelude.Maybe Prelude.Text,
    -- | The field ID for a table field.
    fieldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableFieldOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customLabel', 'tableFieldOption_customLabel' - The custom label for a table field.
--
-- 'uRLStyling', 'tableFieldOption_uRLStyling' - The URL configuration for a table field.
--
-- 'visibility', 'tableFieldOption_visibility' - The visibility of a table field.
--
-- 'width', 'tableFieldOption_width' - The width for a table field.
--
-- 'fieldId', 'tableFieldOption_fieldId' - The field ID for a table field.
newTableFieldOption ::
  -- | 'fieldId'
  Prelude.Text ->
  TableFieldOption
newTableFieldOption pFieldId_ =
  TableFieldOption'
    { customLabel = Prelude.Nothing,
      uRLStyling = Prelude.Nothing,
      visibility = Prelude.Nothing,
      width = Prelude.Nothing,
      fieldId = pFieldId_
    }

-- | The custom label for a table field.
tableFieldOption_customLabel :: Lens.Lens' TableFieldOption (Prelude.Maybe Prelude.Text)
tableFieldOption_customLabel = Lens.lens (\TableFieldOption' {customLabel} -> customLabel) (\s@TableFieldOption' {} a -> s {customLabel = a} :: TableFieldOption)

-- | The URL configuration for a table field.
tableFieldOption_uRLStyling :: Lens.Lens' TableFieldOption (Prelude.Maybe TableFieldURLConfiguration)
tableFieldOption_uRLStyling = Lens.lens (\TableFieldOption' {uRLStyling} -> uRLStyling) (\s@TableFieldOption' {} a -> s {uRLStyling = a} :: TableFieldOption)

-- | The visibility of a table field.
tableFieldOption_visibility :: Lens.Lens' TableFieldOption (Prelude.Maybe Visibility)
tableFieldOption_visibility = Lens.lens (\TableFieldOption' {visibility} -> visibility) (\s@TableFieldOption' {} a -> s {visibility = a} :: TableFieldOption)

-- | The width for a table field.
tableFieldOption_width :: Lens.Lens' TableFieldOption (Prelude.Maybe Prelude.Text)
tableFieldOption_width = Lens.lens (\TableFieldOption' {width} -> width) (\s@TableFieldOption' {} a -> s {width = a} :: TableFieldOption)

-- | The field ID for a table field.
tableFieldOption_fieldId :: Lens.Lens' TableFieldOption Prelude.Text
tableFieldOption_fieldId = Lens.lens (\TableFieldOption' {fieldId} -> fieldId) (\s@TableFieldOption' {} a -> s {fieldId = a} :: TableFieldOption)

instance Data.FromJSON TableFieldOption where
  parseJSON =
    Data.withObject
      "TableFieldOption"
      ( \x ->
          TableFieldOption'
            Prelude.<$> (x Data..:? "CustomLabel")
            Prelude.<*> (x Data..:? "URLStyling")
            Prelude.<*> (x Data..:? "Visibility")
            Prelude.<*> (x Data..:? "Width")
            Prelude.<*> (x Data..: "FieldId")
      )

instance Prelude.Hashable TableFieldOption where
  hashWithSalt _salt TableFieldOption' {..} =
    _salt
      `Prelude.hashWithSalt` customLabel
      `Prelude.hashWithSalt` uRLStyling
      `Prelude.hashWithSalt` visibility
      `Prelude.hashWithSalt` width
      `Prelude.hashWithSalt` fieldId

instance Prelude.NFData TableFieldOption where
  rnf TableFieldOption' {..} =
    Prelude.rnf customLabel
      `Prelude.seq` Prelude.rnf uRLStyling
      `Prelude.seq` Prelude.rnf visibility
      `Prelude.seq` Prelude.rnf width
      `Prelude.seq` Prelude.rnf fieldId

instance Data.ToJSON TableFieldOption where
  toJSON TableFieldOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomLabel" Data..=) Prelude.<$> customLabel,
            ("URLStyling" Data..=) Prelude.<$> uRLStyling,
            ("Visibility" Data..=) Prelude.<$> visibility,
            ("Width" Data..=) Prelude.<$> width,
            Prelude.Just ("FieldId" Data..= fieldId)
          ]
      )
