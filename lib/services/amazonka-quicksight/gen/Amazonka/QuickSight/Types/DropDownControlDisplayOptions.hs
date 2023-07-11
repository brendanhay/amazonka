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
-- Module      : Amazonka.QuickSight.Types.DropDownControlDisplayOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DropDownControlDisplayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LabelOptions
import Amazonka.QuickSight.Types.ListControlSelectAllOptions

-- | The display options of a control.
--
-- /See:/ 'newDropDownControlDisplayOptions' smart constructor.
data DropDownControlDisplayOptions = DropDownControlDisplayOptions'
  { -- | The configuration of the @Select all@ options in a dropdown control.
    selectAllOptions :: Prelude.Maybe ListControlSelectAllOptions,
    -- | The options to configure the title visibility, name, and font size.
    titleOptions :: Prelude.Maybe LabelOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DropDownControlDisplayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectAllOptions', 'dropDownControlDisplayOptions_selectAllOptions' - The configuration of the @Select all@ options in a dropdown control.
--
-- 'titleOptions', 'dropDownControlDisplayOptions_titleOptions' - The options to configure the title visibility, name, and font size.
newDropDownControlDisplayOptions ::
  DropDownControlDisplayOptions
newDropDownControlDisplayOptions =
  DropDownControlDisplayOptions'
    { selectAllOptions =
        Prelude.Nothing,
      titleOptions = Prelude.Nothing
    }

-- | The configuration of the @Select all@ options in a dropdown control.
dropDownControlDisplayOptions_selectAllOptions :: Lens.Lens' DropDownControlDisplayOptions (Prelude.Maybe ListControlSelectAllOptions)
dropDownControlDisplayOptions_selectAllOptions = Lens.lens (\DropDownControlDisplayOptions' {selectAllOptions} -> selectAllOptions) (\s@DropDownControlDisplayOptions' {} a -> s {selectAllOptions = a} :: DropDownControlDisplayOptions)

-- | The options to configure the title visibility, name, and font size.
dropDownControlDisplayOptions_titleOptions :: Lens.Lens' DropDownControlDisplayOptions (Prelude.Maybe LabelOptions)
dropDownControlDisplayOptions_titleOptions = Lens.lens (\DropDownControlDisplayOptions' {titleOptions} -> titleOptions) (\s@DropDownControlDisplayOptions' {} a -> s {titleOptions = a} :: DropDownControlDisplayOptions)

instance Data.FromJSON DropDownControlDisplayOptions where
  parseJSON =
    Data.withObject
      "DropDownControlDisplayOptions"
      ( \x ->
          DropDownControlDisplayOptions'
            Prelude.<$> (x Data..:? "SelectAllOptions")
            Prelude.<*> (x Data..:? "TitleOptions")
      )

instance
  Prelude.Hashable
    DropDownControlDisplayOptions
  where
  hashWithSalt _salt DropDownControlDisplayOptions' {..} =
    _salt
      `Prelude.hashWithSalt` selectAllOptions
      `Prelude.hashWithSalt` titleOptions

instance Prelude.NFData DropDownControlDisplayOptions where
  rnf DropDownControlDisplayOptions' {..} =
    Prelude.rnf selectAllOptions
      `Prelude.seq` Prelude.rnf titleOptions

instance Data.ToJSON DropDownControlDisplayOptions where
  toJSON DropDownControlDisplayOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SelectAllOptions" Data..=)
              Prelude.<$> selectAllOptions,
            ("TitleOptions" Data..=) Prelude.<$> titleOptions
          ]
      )
