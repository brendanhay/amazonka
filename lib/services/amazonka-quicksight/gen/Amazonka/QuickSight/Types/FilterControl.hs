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
-- Module      : Amazonka.QuickSight.Types.FilterControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FilterDateTimePickerControl
import Amazonka.QuickSight.Types.FilterDropDownControl
import Amazonka.QuickSight.Types.FilterListControl
import Amazonka.QuickSight.Types.FilterRelativeDateTimeControl
import Amazonka.QuickSight.Types.FilterSliderControl
import Amazonka.QuickSight.Types.FilterTextAreaControl
import Amazonka.QuickSight.Types.FilterTextFieldControl

-- | The control of a filter that is used to interact with a dashboard or an
-- analysis.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newFilterControl' smart constructor.
data FilterControl = FilterControl'
  { -- | A control from a date filter that is used to specify date and time.
    dateTimePicker :: Prelude.Maybe FilterDateTimePickerControl,
    -- | A control to display a dropdown list with buttons that are used to
    -- select a single value.
    dropdown :: Prelude.Maybe FilterDropDownControl,
    -- | A control to display a list of buttons or boxes. This is used to select
    -- either a single value or multiple values.
    list :: Prelude.Maybe FilterListControl,
    -- | A control from a date filter that is used to specify the relative date.
    relativeDateTime :: Prelude.Maybe FilterRelativeDateTimeControl,
    -- | A control to display a horizontal toggle bar. This is used to change a
    -- value by sliding the toggle.
    slider :: Prelude.Maybe FilterSliderControl,
    -- | A control to display a text box that is used to enter multiple entries.
    textArea :: Prelude.Maybe FilterTextAreaControl,
    -- | A control to display a text box that is used to enter a single entry.
    textField :: Prelude.Maybe FilterTextFieldControl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateTimePicker', 'filterControl_dateTimePicker' - A control from a date filter that is used to specify date and time.
--
-- 'dropdown', 'filterControl_dropdown' - A control to display a dropdown list with buttons that are used to
-- select a single value.
--
-- 'list', 'filterControl_list' - A control to display a list of buttons or boxes. This is used to select
-- either a single value or multiple values.
--
-- 'relativeDateTime', 'filterControl_relativeDateTime' - A control from a date filter that is used to specify the relative date.
--
-- 'slider', 'filterControl_slider' - A control to display a horizontal toggle bar. This is used to change a
-- value by sliding the toggle.
--
-- 'textArea', 'filterControl_textArea' - A control to display a text box that is used to enter multiple entries.
--
-- 'textField', 'filterControl_textField' - A control to display a text box that is used to enter a single entry.
newFilterControl ::
  FilterControl
newFilterControl =
  FilterControl'
    { dateTimePicker = Prelude.Nothing,
      dropdown = Prelude.Nothing,
      list = Prelude.Nothing,
      relativeDateTime = Prelude.Nothing,
      slider = Prelude.Nothing,
      textArea = Prelude.Nothing,
      textField = Prelude.Nothing
    }

-- | A control from a date filter that is used to specify date and time.
filterControl_dateTimePicker :: Lens.Lens' FilterControl (Prelude.Maybe FilterDateTimePickerControl)
filterControl_dateTimePicker = Lens.lens (\FilterControl' {dateTimePicker} -> dateTimePicker) (\s@FilterControl' {} a -> s {dateTimePicker = a} :: FilterControl)

-- | A control to display a dropdown list with buttons that are used to
-- select a single value.
filterControl_dropdown :: Lens.Lens' FilterControl (Prelude.Maybe FilterDropDownControl)
filterControl_dropdown = Lens.lens (\FilterControl' {dropdown} -> dropdown) (\s@FilterControl' {} a -> s {dropdown = a} :: FilterControl)

-- | A control to display a list of buttons or boxes. This is used to select
-- either a single value or multiple values.
filterControl_list :: Lens.Lens' FilterControl (Prelude.Maybe FilterListControl)
filterControl_list = Lens.lens (\FilterControl' {list} -> list) (\s@FilterControl' {} a -> s {list = a} :: FilterControl)

-- | A control from a date filter that is used to specify the relative date.
filterControl_relativeDateTime :: Lens.Lens' FilterControl (Prelude.Maybe FilterRelativeDateTimeControl)
filterControl_relativeDateTime = Lens.lens (\FilterControl' {relativeDateTime} -> relativeDateTime) (\s@FilterControl' {} a -> s {relativeDateTime = a} :: FilterControl)

-- | A control to display a horizontal toggle bar. This is used to change a
-- value by sliding the toggle.
filterControl_slider :: Lens.Lens' FilterControl (Prelude.Maybe FilterSliderControl)
filterControl_slider = Lens.lens (\FilterControl' {slider} -> slider) (\s@FilterControl' {} a -> s {slider = a} :: FilterControl)

-- | A control to display a text box that is used to enter multiple entries.
filterControl_textArea :: Lens.Lens' FilterControl (Prelude.Maybe FilterTextAreaControl)
filterControl_textArea = Lens.lens (\FilterControl' {textArea} -> textArea) (\s@FilterControl' {} a -> s {textArea = a} :: FilterControl)

-- | A control to display a text box that is used to enter a single entry.
filterControl_textField :: Lens.Lens' FilterControl (Prelude.Maybe FilterTextFieldControl)
filterControl_textField = Lens.lens (\FilterControl' {textField} -> textField) (\s@FilterControl' {} a -> s {textField = a} :: FilterControl)

instance Data.FromJSON FilterControl where
  parseJSON =
    Data.withObject
      "FilterControl"
      ( \x ->
          FilterControl'
            Prelude.<$> (x Data..:? "DateTimePicker")
            Prelude.<*> (x Data..:? "Dropdown")
            Prelude.<*> (x Data..:? "List")
            Prelude.<*> (x Data..:? "RelativeDateTime")
            Prelude.<*> (x Data..:? "Slider")
            Prelude.<*> (x Data..:? "TextArea")
            Prelude.<*> (x Data..:? "TextField")
      )

instance Prelude.Hashable FilterControl where
  hashWithSalt _salt FilterControl' {..} =
    _salt `Prelude.hashWithSalt` dateTimePicker
      `Prelude.hashWithSalt` dropdown
      `Prelude.hashWithSalt` list
      `Prelude.hashWithSalt` relativeDateTime
      `Prelude.hashWithSalt` slider
      `Prelude.hashWithSalt` textArea
      `Prelude.hashWithSalt` textField

instance Prelude.NFData FilterControl where
  rnf FilterControl' {..} =
    Prelude.rnf dateTimePicker
      `Prelude.seq` Prelude.rnf dropdown
      `Prelude.seq` Prelude.rnf list
      `Prelude.seq` Prelude.rnf relativeDateTime
      `Prelude.seq` Prelude.rnf slider
      `Prelude.seq` Prelude.rnf textArea
      `Prelude.seq` Prelude.rnf textField

instance Data.ToJSON FilterControl where
  toJSON FilterControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateTimePicker" Data..=)
              Prelude.<$> dateTimePicker,
            ("Dropdown" Data..=) Prelude.<$> dropdown,
            ("List" Data..=) Prelude.<$> list,
            ("RelativeDateTime" Data..=)
              Prelude.<$> relativeDateTime,
            ("Slider" Data..=) Prelude.<$> slider,
            ("TextArea" Data..=) Prelude.<$> textArea,
            ("TextField" Data..=) Prelude.<$> textField
          ]
      )
