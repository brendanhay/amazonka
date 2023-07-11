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
-- Module      : Amazonka.QuickSight.Types.DataFieldSeriesItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataFieldSeriesItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisBinding
import Amazonka.QuickSight.Types.LineChartSeriesSettings

-- | The data field series item configuration of a line chart.
--
-- /See:/ 'newDataFieldSeriesItem' smart constructor.
data DataFieldSeriesItem = DataFieldSeriesItem'
  { -- | The field value of the field that you are setting the axis binding to.
    fieldValue :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The options that determine the presentation of line series associated to
    -- the field.
    settings :: Prelude.Maybe LineChartSeriesSettings,
    -- | The field ID of the field that you are setting the axis binding to.
    fieldId :: Prelude.Text,
    -- | The axis that you are binding the field to.
    axisBinding :: AxisBinding
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataFieldSeriesItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldValue', 'dataFieldSeriesItem_fieldValue' - The field value of the field that you are setting the axis binding to.
--
-- 'settings', 'dataFieldSeriesItem_settings' - The options that determine the presentation of line series associated to
-- the field.
--
-- 'fieldId', 'dataFieldSeriesItem_fieldId' - The field ID of the field that you are setting the axis binding to.
--
-- 'axisBinding', 'dataFieldSeriesItem_axisBinding' - The axis that you are binding the field to.
newDataFieldSeriesItem ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'axisBinding'
  AxisBinding ->
  DataFieldSeriesItem
newDataFieldSeriesItem pFieldId_ pAxisBinding_ =
  DataFieldSeriesItem'
    { fieldValue = Prelude.Nothing,
      settings = Prelude.Nothing,
      fieldId = pFieldId_,
      axisBinding = pAxisBinding_
    }

-- | The field value of the field that you are setting the axis binding to.
dataFieldSeriesItem_fieldValue :: Lens.Lens' DataFieldSeriesItem (Prelude.Maybe Prelude.Text)
dataFieldSeriesItem_fieldValue = Lens.lens (\DataFieldSeriesItem' {fieldValue} -> fieldValue) (\s@DataFieldSeriesItem' {} a -> s {fieldValue = a} :: DataFieldSeriesItem) Prelude.. Lens.mapping Data._Sensitive

-- | The options that determine the presentation of line series associated to
-- the field.
dataFieldSeriesItem_settings :: Lens.Lens' DataFieldSeriesItem (Prelude.Maybe LineChartSeriesSettings)
dataFieldSeriesItem_settings = Lens.lens (\DataFieldSeriesItem' {settings} -> settings) (\s@DataFieldSeriesItem' {} a -> s {settings = a} :: DataFieldSeriesItem)

-- | The field ID of the field that you are setting the axis binding to.
dataFieldSeriesItem_fieldId :: Lens.Lens' DataFieldSeriesItem Prelude.Text
dataFieldSeriesItem_fieldId = Lens.lens (\DataFieldSeriesItem' {fieldId} -> fieldId) (\s@DataFieldSeriesItem' {} a -> s {fieldId = a} :: DataFieldSeriesItem)

-- | The axis that you are binding the field to.
dataFieldSeriesItem_axisBinding :: Lens.Lens' DataFieldSeriesItem AxisBinding
dataFieldSeriesItem_axisBinding = Lens.lens (\DataFieldSeriesItem' {axisBinding} -> axisBinding) (\s@DataFieldSeriesItem' {} a -> s {axisBinding = a} :: DataFieldSeriesItem)

instance Data.FromJSON DataFieldSeriesItem where
  parseJSON =
    Data.withObject
      "DataFieldSeriesItem"
      ( \x ->
          DataFieldSeriesItem'
            Prelude.<$> (x Data..:? "FieldValue")
            Prelude.<*> (x Data..:? "Settings")
            Prelude.<*> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "AxisBinding")
      )

instance Prelude.Hashable DataFieldSeriesItem where
  hashWithSalt _salt DataFieldSeriesItem' {..} =
    _salt
      `Prelude.hashWithSalt` fieldValue
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` axisBinding

instance Prelude.NFData DataFieldSeriesItem where
  rnf DataFieldSeriesItem' {..} =
    Prelude.rnf fieldValue
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf axisBinding

instance Data.ToJSON DataFieldSeriesItem where
  toJSON DataFieldSeriesItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldValue" Data..=) Prelude.<$> fieldValue,
            ("Settings" Data..=) Prelude.<$> settings,
            Prelude.Just ("FieldId" Data..= fieldId),
            Prelude.Just ("AxisBinding" Data..= axisBinding)
          ]
      )
