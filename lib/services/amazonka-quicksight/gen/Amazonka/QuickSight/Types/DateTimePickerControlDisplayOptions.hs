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
-- Module      : Amazonka.QuickSight.Types.DateTimePickerControlDisplayOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateTimePickerControlDisplayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LabelOptions

-- | The display options of a control.
--
-- /See:/ 'newDateTimePickerControlDisplayOptions' smart constructor.
data DateTimePickerControlDisplayOptions = DateTimePickerControlDisplayOptions'
  { -- | Customize how dates are formatted in controls.
    dateTimeFormat :: Prelude.Maybe Prelude.Text,
    -- | The options to configure the title visibility, name, and font size.
    titleOptions :: Prelude.Maybe LabelOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateTimePickerControlDisplayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateTimeFormat', 'dateTimePickerControlDisplayOptions_dateTimeFormat' - Customize how dates are formatted in controls.
--
-- 'titleOptions', 'dateTimePickerControlDisplayOptions_titleOptions' - The options to configure the title visibility, name, and font size.
newDateTimePickerControlDisplayOptions ::
  DateTimePickerControlDisplayOptions
newDateTimePickerControlDisplayOptions =
  DateTimePickerControlDisplayOptions'
    { dateTimeFormat =
        Prelude.Nothing,
      titleOptions = Prelude.Nothing
    }

-- | Customize how dates are formatted in controls.
dateTimePickerControlDisplayOptions_dateTimeFormat :: Lens.Lens' DateTimePickerControlDisplayOptions (Prelude.Maybe Prelude.Text)
dateTimePickerControlDisplayOptions_dateTimeFormat = Lens.lens (\DateTimePickerControlDisplayOptions' {dateTimeFormat} -> dateTimeFormat) (\s@DateTimePickerControlDisplayOptions' {} a -> s {dateTimeFormat = a} :: DateTimePickerControlDisplayOptions)

-- | The options to configure the title visibility, name, and font size.
dateTimePickerControlDisplayOptions_titleOptions :: Lens.Lens' DateTimePickerControlDisplayOptions (Prelude.Maybe LabelOptions)
dateTimePickerControlDisplayOptions_titleOptions = Lens.lens (\DateTimePickerControlDisplayOptions' {titleOptions} -> titleOptions) (\s@DateTimePickerControlDisplayOptions' {} a -> s {titleOptions = a} :: DateTimePickerControlDisplayOptions)

instance
  Data.FromJSON
    DateTimePickerControlDisplayOptions
  where
  parseJSON =
    Data.withObject
      "DateTimePickerControlDisplayOptions"
      ( \x ->
          DateTimePickerControlDisplayOptions'
            Prelude.<$> (x Data..:? "DateTimeFormat")
            Prelude.<*> (x Data..:? "TitleOptions")
      )

instance
  Prelude.Hashable
    DateTimePickerControlDisplayOptions
  where
  hashWithSalt
    _salt
    DateTimePickerControlDisplayOptions' {..} =
      _salt
        `Prelude.hashWithSalt` dateTimeFormat
        `Prelude.hashWithSalt` titleOptions

instance
  Prelude.NFData
    DateTimePickerControlDisplayOptions
  where
  rnf DateTimePickerControlDisplayOptions' {..} =
    Prelude.rnf dateTimeFormat `Prelude.seq`
      Prelude.rnf titleOptions

instance
  Data.ToJSON
    DateTimePickerControlDisplayOptions
  where
  toJSON DateTimePickerControlDisplayOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateTimeFormat" Data..=)
              Prelude.<$> dateTimeFormat,
            ("TitleOptions" Data..=) Prelude.<$> titleOptions
          ]
      )
