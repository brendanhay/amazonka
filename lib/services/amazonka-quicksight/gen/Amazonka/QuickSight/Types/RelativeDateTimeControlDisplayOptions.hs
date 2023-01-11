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
-- Module      : Amazonka.QuickSight.Types.RelativeDateTimeControlDisplayOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RelativeDateTimeControlDisplayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LabelOptions

-- | The display options of a control.
--
-- /See:/ 'newRelativeDateTimeControlDisplayOptions' smart constructor.
data RelativeDateTimeControlDisplayOptions = RelativeDateTimeControlDisplayOptions'
  { -- | Customize how dates are formatted in controls.
    dateTimeFormat :: Prelude.Maybe Prelude.Text,
    -- | The options to configure the title visibility, name, and font size.
    titleOptions :: Prelude.Maybe LabelOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelativeDateTimeControlDisplayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateTimeFormat', 'relativeDateTimeControlDisplayOptions_dateTimeFormat' - Customize how dates are formatted in controls.
--
-- 'titleOptions', 'relativeDateTimeControlDisplayOptions_titleOptions' - The options to configure the title visibility, name, and font size.
newRelativeDateTimeControlDisplayOptions ::
  RelativeDateTimeControlDisplayOptions
newRelativeDateTimeControlDisplayOptions =
  RelativeDateTimeControlDisplayOptions'
    { dateTimeFormat =
        Prelude.Nothing,
      titleOptions = Prelude.Nothing
    }

-- | Customize how dates are formatted in controls.
relativeDateTimeControlDisplayOptions_dateTimeFormat :: Lens.Lens' RelativeDateTimeControlDisplayOptions (Prelude.Maybe Prelude.Text)
relativeDateTimeControlDisplayOptions_dateTimeFormat = Lens.lens (\RelativeDateTimeControlDisplayOptions' {dateTimeFormat} -> dateTimeFormat) (\s@RelativeDateTimeControlDisplayOptions' {} a -> s {dateTimeFormat = a} :: RelativeDateTimeControlDisplayOptions)

-- | The options to configure the title visibility, name, and font size.
relativeDateTimeControlDisplayOptions_titleOptions :: Lens.Lens' RelativeDateTimeControlDisplayOptions (Prelude.Maybe LabelOptions)
relativeDateTimeControlDisplayOptions_titleOptions = Lens.lens (\RelativeDateTimeControlDisplayOptions' {titleOptions} -> titleOptions) (\s@RelativeDateTimeControlDisplayOptions' {} a -> s {titleOptions = a} :: RelativeDateTimeControlDisplayOptions)

instance
  Data.FromJSON
    RelativeDateTimeControlDisplayOptions
  where
  parseJSON =
    Data.withObject
      "RelativeDateTimeControlDisplayOptions"
      ( \x ->
          RelativeDateTimeControlDisplayOptions'
            Prelude.<$> (x Data..:? "DateTimeFormat")
            Prelude.<*> (x Data..:? "TitleOptions")
      )

instance
  Prelude.Hashable
    RelativeDateTimeControlDisplayOptions
  where
  hashWithSalt
    _salt
    RelativeDateTimeControlDisplayOptions' {..} =
      _salt `Prelude.hashWithSalt` dateTimeFormat
        `Prelude.hashWithSalt` titleOptions

instance
  Prelude.NFData
    RelativeDateTimeControlDisplayOptions
  where
  rnf RelativeDateTimeControlDisplayOptions' {..} =
    Prelude.rnf dateTimeFormat
      `Prelude.seq` Prelude.rnf titleOptions

instance
  Data.ToJSON
    RelativeDateTimeControlDisplayOptions
  where
  toJSON RelativeDateTimeControlDisplayOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateTimeFormat" Data..=)
              Prelude.<$> dateTimeFormat,
            ("TitleOptions" Data..=) Prelude.<$> titleOptions
          ]
      )
