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
-- Module      : Amazonka.QuickSight.Types.TextFieldControlDisplayOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TextFieldControlDisplayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LabelOptions
import Amazonka.QuickSight.Types.TextControlPlaceholderOptions

-- | The display options of a control.
--
-- /See:/ 'newTextFieldControlDisplayOptions' smart constructor.
data TextFieldControlDisplayOptions = TextFieldControlDisplayOptions'
  { -- | The configuration of the placeholder options in a text field control.
    placeholderOptions :: Prelude.Maybe TextControlPlaceholderOptions,
    -- | The options to configure the title visibility, name, and font size.
    titleOptions :: Prelude.Maybe LabelOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextFieldControlDisplayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placeholderOptions', 'textFieldControlDisplayOptions_placeholderOptions' - The configuration of the placeholder options in a text field control.
--
-- 'titleOptions', 'textFieldControlDisplayOptions_titleOptions' - The options to configure the title visibility, name, and font size.
newTextFieldControlDisplayOptions ::
  TextFieldControlDisplayOptions
newTextFieldControlDisplayOptions =
  TextFieldControlDisplayOptions'
    { placeholderOptions =
        Prelude.Nothing,
      titleOptions = Prelude.Nothing
    }

-- | The configuration of the placeholder options in a text field control.
textFieldControlDisplayOptions_placeholderOptions :: Lens.Lens' TextFieldControlDisplayOptions (Prelude.Maybe TextControlPlaceholderOptions)
textFieldControlDisplayOptions_placeholderOptions = Lens.lens (\TextFieldControlDisplayOptions' {placeholderOptions} -> placeholderOptions) (\s@TextFieldControlDisplayOptions' {} a -> s {placeholderOptions = a} :: TextFieldControlDisplayOptions)

-- | The options to configure the title visibility, name, and font size.
textFieldControlDisplayOptions_titleOptions :: Lens.Lens' TextFieldControlDisplayOptions (Prelude.Maybe LabelOptions)
textFieldControlDisplayOptions_titleOptions = Lens.lens (\TextFieldControlDisplayOptions' {titleOptions} -> titleOptions) (\s@TextFieldControlDisplayOptions' {} a -> s {titleOptions = a} :: TextFieldControlDisplayOptions)

instance Data.FromJSON TextFieldControlDisplayOptions where
  parseJSON =
    Data.withObject
      "TextFieldControlDisplayOptions"
      ( \x ->
          TextFieldControlDisplayOptions'
            Prelude.<$> (x Data..:? "PlaceholderOptions")
            Prelude.<*> (x Data..:? "TitleOptions")
      )

instance
  Prelude.Hashable
    TextFieldControlDisplayOptions
  where
  hashWithSalt
    _salt
    TextFieldControlDisplayOptions' {..} =
      _salt
        `Prelude.hashWithSalt` placeholderOptions
        `Prelude.hashWithSalt` titleOptions

instance
  Prelude.NFData
    TextFieldControlDisplayOptions
  where
  rnf TextFieldControlDisplayOptions' {..} =
    Prelude.rnf placeholderOptions
      `Prelude.seq` Prelude.rnf titleOptions

instance Data.ToJSON TextFieldControlDisplayOptions where
  toJSON TextFieldControlDisplayOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PlaceholderOptions" Data..=)
              Prelude.<$> placeholderOptions,
            ("TitleOptions" Data..=) Prelude.<$> titleOptions
          ]
      )
