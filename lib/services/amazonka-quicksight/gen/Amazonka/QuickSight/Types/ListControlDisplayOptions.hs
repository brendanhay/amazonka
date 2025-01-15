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
-- Module      : Amazonka.QuickSight.Types.ListControlDisplayOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ListControlDisplayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LabelOptions
import Amazonka.QuickSight.Types.ListControlSearchOptions
import Amazonka.QuickSight.Types.ListControlSelectAllOptions

-- | The display options of a control.
--
-- /See:/ 'newListControlDisplayOptions' smart constructor.
data ListControlDisplayOptions = ListControlDisplayOptions'
  { -- | The configuration of the search options in a list control.
    searchOptions :: Prelude.Maybe ListControlSearchOptions,
    -- | The configuration of the @Select all@ options in a list control.
    selectAllOptions :: Prelude.Maybe ListControlSelectAllOptions,
    -- | The options to configure the title visibility, name, and font size.
    titleOptions :: Prelude.Maybe LabelOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListControlDisplayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'searchOptions', 'listControlDisplayOptions_searchOptions' - The configuration of the search options in a list control.
--
-- 'selectAllOptions', 'listControlDisplayOptions_selectAllOptions' - The configuration of the @Select all@ options in a list control.
--
-- 'titleOptions', 'listControlDisplayOptions_titleOptions' - The options to configure the title visibility, name, and font size.
newListControlDisplayOptions ::
  ListControlDisplayOptions
newListControlDisplayOptions =
  ListControlDisplayOptions'
    { searchOptions =
        Prelude.Nothing,
      selectAllOptions = Prelude.Nothing,
      titleOptions = Prelude.Nothing
    }

-- | The configuration of the search options in a list control.
listControlDisplayOptions_searchOptions :: Lens.Lens' ListControlDisplayOptions (Prelude.Maybe ListControlSearchOptions)
listControlDisplayOptions_searchOptions = Lens.lens (\ListControlDisplayOptions' {searchOptions} -> searchOptions) (\s@ListControlDisplayOptions' {} a -> s {searchOptions = a} :: ListControlDisplayOptions)

-- | The configuration of the @Select all@ options in a list control.
listControlDisplayOptions_selectAllOptions :: Lens.Lens' ListControlDisplayOptions (Prelude.Maybe ListControlSelectAllOptions)
listControlDisplayOptions_selectAllOptions = Lens.lens (\ListControlDisplayOptions' {selectAllOptions} -> selectAllOptions) (\s@ListControlDisplayOptions' {} a -> s {selectAllOptions = a} :: ListControlDisplayOptions)

-- | The options to configure the title visibility, name, and font size.
listControlDisplayOptions_titleOptions :: Lens.Lens' ListControlDisplayOptions (Prelude.Maybe LabelOptions)
listControlDisplayOptions_titleOptions = Lens.lens (\ListControlDisplayOptions' {titleOptions} -> titleOptions) (\s@ListControlDisplayOptions' {} a -> s {titleOptions = a} :: ListControlDisplayOptions)

instance Data.FromJSON ListControlDisplayOptions where
  parseJSON =
    Data.withObject
      "ListControlDisplayOptions"
      ( \x ->
          ListControlDisplayOptions'
            Prelude.<$> (x Data..:? "SearchOptions")
            Prelude.<*> (x Data..:? "SelectAllOptions")
            Prelude.<*> (x Data..:? "TitleOptions")
      )

instance Prelude.Hashable ListControlDisplayOptions where
  hashWithSalt _salt ListControlDisplayOptions' {..} =
    _salt
      `Prelude.hashWithSalt` searchOptions
      `Prelude.hashWithSalt` selectAllOptions
      `Prelude.hashWithSalt` titleOptions

instance Prelude.NFData ListControlDisplayOptions where
  rnf ListControlDisplayOptions' {..} =
    Prelude.rnf searchOptions `Prelude.seq`
      Prelude.rnf selectAllOptions `Prelude.seq`
        Prelude.rnf titleOptions

instance Data.ToJSON ListControlDisplayOptions where
  toJSON ListControlDisplayOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SearchOptions" Data..=) Prelude.<$> searchOptions,
            ("SelectAllOptions" Data..=)
              Prelude.<$> selectAllOptions,
            ("TitleOptions" Data..=) Prelude.<$> titleOptions
          ]
      )
