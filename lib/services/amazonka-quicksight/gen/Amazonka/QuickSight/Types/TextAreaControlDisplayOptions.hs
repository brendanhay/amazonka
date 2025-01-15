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
-- Module      : Amazonka.QuickSight.Types.TextAreaControlDisplayOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TextAreaControlDisplayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LabelOptions
import Amazonka.QuickSight.Types.TextControlPlaceholderOptions

-- | The display options of a control.
--
-- /See:/ 'newTextAreaControlDisplayOptions' smart constructor.
data TextAreaControlDisplayOptions = TextAreaControlDisplayOptions'
  { -- | The configuration of the placeholder options in a text area control.
    placeholderOptions :: Prelude.Maybe TextControlPlaceholderOptions,
    -- | The options to configure the title visibility, name, and font size.
    titleOptions :: Prelude.Maybe LabelOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextAreaControlDisplayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placeholderOptions', 'textAreaControlDisplayOptions_placeholderOptions' - The configuration of the placeholder options in a text area control.
--
-- 'titleOptions', 'textAreaControlDisplayOptions_titleOptions' - The options to configure the title visibility, name, and font size.
newTextAreaControlDisplayOptions ::
  TextAreaControlDisplayOptions
newTextAreaControlDisplayOptions =
  TextAreaControlDisplayOptions'
    { placeholderOptions =
        Prelude.Nothing,
      titleOptions = Prelude.Nothing
    }

-- | The configuration of the placeholder options in a text area control.
textAreaControlDisplayOptions_placeholderOptions :: Lens.Lens' TextAreaControlDisplayOptions (Prelude.Maybe TextControlPlaceholderOptions)
textAreaControlDisplayOptions_placeholderOptions = Lens.lens (\TextAreaControlDisplayOptions' {placeholderOptions} -> placeholderOptions) (\s@TextAreaControlDisplayOptions' {} a -> s {placeholderOptions = a} :: TextAreaControlDisplayOptions)

-- | The options to configure the title visibility, name, and font size.
textAreaControlDisplayOptions_titleOptions :: Lens.Lens' TextAreaControlDisplayOptions (Prelude.Maybe LabelOptions)
textAreaControlDisplayOptions_titleOptions = Lens.lens (\TextAreaControlDisplayOptions' {titleOptions} -> titleOptions) (\s@TextAreaControlDisplayOptions' {} a -> s {titleOptions = a} :: TextAreaControlDisplayOptions)

instance Data.FromJSON TextAreaControlDisplayOptions where
  parseJSON =
    Data.withObject
      "TextAreaControlDisplayOptions"
      ( \x ->
          TextAreaControlDisplayOptions'
            Prelude.<$> (x Data..:? "PlaceholderOptions")
            Prelude.<*> (x Data..:? "TitleOptions")
      )

instance
  Prelude.Hashable
    TextAreaControlDisplayOptions
  where
  hashWithSalt _salt TextAreaControlDisplayOptions' {..} =
    _salt
      `Prelude.hashWithSalt` placeholderOptions
      `Prelude.hashWithSalt` titleOptions

instance Prelude.NFData TextAreaControlDisplayOptions where
  rnf TextAreaControlDisplayOptions' {..} =
    Prelude.rnf placeholderOptions `Prelude.seq`
      Prelude.rnf titleOptions

instance Data.ToJSON TextAreaControlDisplayOptions where
  toJSON TextAreaControlDisplayOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PlaceholderOptions" Data..=)
              Prelude.<$> placeholderOptions,
            ("TitleOptions" Data..=) Prelude.<$> titleOptions
          ]
      )
