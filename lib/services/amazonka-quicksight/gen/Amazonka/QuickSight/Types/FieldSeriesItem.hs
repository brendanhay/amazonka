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
-- Module      : Amazonka.QuickSight.Types.FieldSeriesItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FieldSeriesItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisBinding
import Amazonka.QuickSight.Types.LineChartSeriesSettings

-- | The field series item configuration of a line chart.
--
-- /See:/ 'newFieldSeriesItem' smart constructor.
data FieldSeriesItem = FieldSeriesItem'
  { -- | The options that determine the presentation of line series associated to
    -- the field.
    settings :: Prelude.Maybe LineChartSeriesSettings,
    -- | The field ID of the field for which you are setting the axis binding.
    fieldId :: Prelude.Text,
    -- | The axis that you are binding the field to.
    axisBinding :: AxisBinding
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldSeriesItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'fieldSeriesItem_settings' - The options that determine the presentation of line series associated to
-- the field.
--
-- 'fieldId', 'fieldSeriesItem_fieldId' - The field ID of the field for which you are setting the axis binding.
--
-- 'axisBinding', 'fieldSeriesItem_axisBinding' - The axis that you are binding the field to.
newFieldSeriesItem ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'axisBinding'
  AxisBinding ->
  FieldSeriesItem
newFieldSeriesItem pFieldId_ pAxisBinding_ =
  FieldSeriesItem'
    { settings = Prelude.Nothing,
      fieldId = pFieldId_,
      axisBinding = pAxisBinding_
    }

-- | The options that determine the presentation of line series associated to
-- the field.
fieldSeriesItem_settings :: Lens.Lens' FieldSeriesItem (Prelude.Maybe LineChartSeriesSettings)
fieldSeriesItem_settings = Lens.lens (\FieldSeriesItem' {settings} -> settings) (\s@FieldSeriesItem' {} a -> s {settings = a} :: FieldSeriesItem)

-- | The field ID of the field for which you are setting the axis binding.
fieldSeriesItem_fieldId :: Lens.Lens' FieldSeriesItem Prelude.Text
fieldSeriesItem_fieldId = Lens.lens (\FieldSeriesItem' {fieldId} -> fieldId) (\s@FieldSeriesItem' {} a -> s {fieldId = a} :: FieldSeriesItem)

-- | The axis that you are binding the field to.
fieldSeriesItem_axisBinding :: Lens.Lens' FieldSeriesItem AxisBinding
fieldSeriesItem_axisBinding = Lens.lens (\FieldSeriesItem' {axisBinding} -> axisBinding) (\s@FieldSeriesItem' {} a -> s {axisBinding = a} :: FieldSeriesItem)

instance Data.FromJSON FieldSeriesItem where
  parseJSON =
    Data.withObject
      "FieldSeriesItem"
      ( \x ->
          FieldSeriesItem'
            Prelude.<$> (x Data..:? "Settings")
            Prelude.<*> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "AxisBinding")
      )

instance Prelude.Hashable FieldSeriesItem where
  hashWithSalt _salt FieldSeriesItem' {..} =
    _salt
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` axisBinding

instance Prelude.NFData FieldSeriesItem where
  rnf FieldSeriesItem' {..} =
    Prelude.rnf settings `Prelude.seq`
      Prelude.rnf fieldId `Prelude.seq`
        Prelude.rnf axisBinding

instance Data.ToJSON FieldSeriesItem where
  toJSON FieldSeriesItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Settings" Data..=) Prelude.<$> settings,
            Prelude.Just ("FieldId" Data..= fieldId),
            Prelude.Just ("AxisBinding" Data..= axisBinding)
          ]
      )
