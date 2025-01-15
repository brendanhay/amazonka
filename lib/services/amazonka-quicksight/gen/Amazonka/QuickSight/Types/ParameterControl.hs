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
-- Module      : Amazonka.QuickSight.Types.ParameterControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ParameterControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ParameterDateTimePickerControl
import Amazonka.QuickSight.Types.ParameterDropDownControl
import Amazonka.QuickSight.Types.ParameterListControl
import Amazonka.QuickSight.Types.ParameterSliderControl
import Amazonka.QuickSight.Types.ParameterTextAreaControl
import Amazonka.QuickSight.Types.ParameterTextFieldControl

-- | The control of a parameter that users can interact with in a dashboard
-- or an analysis.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newParameterControl' smart constructor.
data ParameterControl = ParameterControl'
  { -- | A control from a date parameter that specifies date and time.
    dateTimePicker :: Prelude.Maybe ParameterDateTimePickerControl,
    -- | A control to display a dropdown list with buttons that are used to
    -- select a single value.
    dropdown :: Prelude.Maybe ParameterDropDownControl,
    -- | A control to display a list with buttons or boxes that are used to
    -- select either a single value or multiple values.
    list :: Prelude.Maybe ParameterListControl,
    -- | A control to display a horizontal toggle bar. This is used to change a
    -- value by sliding the toggle.
    slider :: Prelude.Maybe ParameterSliderControl,
    -- | A control to display a text box that is used to enter multiple entries.
    textArea :: Prelude.Maybe ParameterTextAreaControl,
    -- | A control to display a text box that is used to enter a single entry.
    textField :: Prelude.Maybe ParameterTextFieldControl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateTimePicker', 'parameterControl_dateTimePicker' - A control from a date parameter that specifies date and time.
--
-- 'dropdown', 'parameterControl_dropdown' - A control to display a dropdown list with buttons that are used to
-- select a single value.
--
-- 'list', 'parameterControl_list' - A control to display a list with buttons or boxes that are used to
-- select either a single value or multiple values.
--
-- 'slider', 'parameterControl_slider' - A control to display a horizontal toggle bar. This is used to change a
-- value by sliding the toggle.
--
-- 'textArea', 'parameterControl_textArea' - A control to display a text box that is used to enter multiple entries.
--
-- 'textField', 'parameterControl_textField' - A control to display a text box that is used to enter a single entry.
newParameterControl ::
  ParameterControl
newParameterControl =
  ParameterControl'
    { dateTimePicker = Prelude.Nothing,
      dropdown = Prelude.Nothing,
      list = Prelude.Nothing,
      slider = Prelude.Nothing,
      textArea = Prelude.Nothing,
      textField = Prelude.Nothing
    }

-- | A control from a date parameter that specifies date and time.
parameterControl_dateTimePicker :: Lens.Lens' ParameterControl (Prelude.Maybe ParameterDateTimePickerControl)
parameterControl_dateTimePicker = Lens.lens (\ParameterControl' {dateTimePicker} -> dateTimePicker) (\s@ParameterControl' {} a -> s {dateTimePicker = a} :: ParameterControl)

-- | A control to display a dropdown list with buttons that are used to
-- select a single value.
parameterControl_dropdown :: Lens.Lens' ParameterControl (Prelude.Maybe ParameterDropDownControl)
parameterControl_dropdown = Lens.lens (\ParameterControl' {dropdown} -> dropdown) (\s@ParameterControl' {} a -> s {dropdown = a} :: ParameterControl)

-- | A control to display a list with buttons or boxes that are used to
-- select either a single value or multiple values.
parameterControl_list :: Lens.Lens' ParameterControl (Prelude.Maybe ParameterListControl)
parameterControl_list = Lens.lens (\ParameterControl' {list} -> list) (\s@ParameterControl' {} a -> s {list = a} :: ParameterControl)

-- | A control to display a horizontal toggle bar. This is used to change a
-- value by sliding the toggle.
parameterControl_slider :: Lens.Lens' ParameterControl (Prelude.Maybe ParameterSliderControl)
parameterControl_slider = Lens.lens (\ParameterControl' {slider} -> slider) (\s@ParameterControl' {} a -> s {slider = a} :: ParameterControl)

-- | A control to display a text box that is used to enter multiple entries.
parameterControl_textArea :: Lens.Lens' ParameterControl (Prelude.Maybe ParameterTextAreaControl)
parameterControl_textArea = Lens.lens (\ParameterControl' {textArea} -> textArea) (\s@ParameterControl' {} a -> s {textArea = a} :: ParameterControl)

-- | A control to display a text box that is used to enter a single entry.
parameterControl_textField :: Lens.Lens' ParameterControl (Prelude.Maybe ParameterTextFieldControl)
parameterControl_textField = Lens.lens (\ParameterControl' {textField} -> textField) (\s@ParameterControl' {} a -> s {textField = a} :: ParameterControl)

instance Data.FromJSON ParameterControl where
  parseJSON =
    Data.withObject
      "ParameterControl"
      ( \x ->
          ParameterControl'
            Prelude.<$> (x Data..:? "DateTimePicker")
            Prelude.<*> (x Data..:? "Dropdown")
            Prelude.<*> (x Data..:? "List")
            Prelude.<*> (x Data..:? "Slider")
            Prelude.<*> (x Data..:? "TextArea")
            Prelude.<*> (x Data..:? "TextField")
      )

instance Prelude.Hashable ParameterControl where
  hashWithSalt _salt ParameterControl' {..} =
    _salt
      `Prelude.hashWithSalt` dateTimePicker
      `Prelude.hashWithSalt` dropdown
      `Prelude.hashWithSalt` list
      `Prelude.hashWithSalt` slider
      `Prelude.hashWithSalt` textArea
      `Prelude.hashWithSalt` textField

instance Prelude.NFData ParameterControl where
  rnf ParameterControl' {..} =
    Prelude.rnf dateTimePicker `Prelude.seq`
      Prelude.rnf dropdown `Prelude.seq`
        Prelude.rnf list `Prelude.seq`
          Prelude.rnf slider `Prelude.seq`
            Prelude.rnf textArea `Prelude.seq`
              Prelude.rnf textField

instance Data.ToJSON ParameterControl where
  toJSON ParameterControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateTimePicker" Data..=)
              Prelude.<$> dateTimePicker,
            ("Dropdown" Data..=) Prelude.<$> dropdown,
            ("List" Data..=) Prelude.<$> list,
            ("Slider" Data..=) Prelude.<$> slider,
            ("TextArea" Data..=) Prelude.<$> textArea,
            ("TextField" Data..=) Prelude.<$> textField
          ]
      )
