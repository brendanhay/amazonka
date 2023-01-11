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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormCTA
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormCTA where

import Amazonka.AmplifyUiBuilder.Types.FormButton
import Amazonka.AmplifyUiBuilder.Types.FormButtonsPosition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the call to action button configuration for the form.
--
-- /See:/ 'newFormCTA' smart constructor.
data FormCTA = FormCTA'
  { -- | Displays a cancel button.
    cancel :: Prelude.Maybe FormButton,
    -- | Displays a clear button.
    clear :: Prelude.Maybe FormButton,
    -- | The position of the button.
    position :: Prelude.Maybe FormButtonsPosition,
    -- | Displays a submit button.
    submit :: Prelude.Maybe FormButton
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormCTA' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancel', 'formCTA_cancel' - Displays a cancel button.
--
-- 'clear', 'formCTA_clear' - Displays a clear button.
--
-- 'position', 'formCTA_position' - The position of the button.
--
-- 'submit', 'formCTA_submit' - Displays a submit button.
newFormCTA ::
  FormCTA
newFormCTA =
  FormCTA'
    { cancel = Prelude.Nothing,
      clear = Prelude.Nothing,
      position = Prelude.Nothing,
      submit = Prelude.Nothing
    }

-- | Displays a cancel button.
formCTA_cancel :: Lens.Lens' FormCTA (Prelude.Maybe FormButton)
formCTA_cancel = Lens.lens (\FormCTA' {cancel} -> cancel) (\s@FormCTA' {} a -> s {cancel = a} :: FormCTA)

-- | Displays a clear button.
formCTA_clear :: Lens.Lens' FormCTA (Prelude.Maybe FormButton)
formCTA_clear = Lens.lens (\FormCTA' {clear} -> clear) (\s@FormCTA' {} a -> s {clear = a} :: FormCTA)

-- | The position of the button.
formCTA_position :: Lens.Lens' FormCTA (Prelude.Maybe FormButtonsPosition)
formCTA_position = Lens.lens (\FormCTA' {position} -> position) (\s@FormCTA' {} a -> s {position = a} :: FormCTA)

-- | Displays a submit button.
formCTA_submit :: Lens.Lens' FormCTA (Prelude.Maybe FormButton)
formCTA_submit = Lens.lens (\FormCTA' {submit} -> submit) (\s@FormCTA' {} a -> s {submit = a} :: FormCTA)

instance Data.FromJSON FormCTA where
  parseJSON =
    Data.withObject
      "FormCTA"
      ( \x ->
          FormCTA'
            Prelude.<$> (x Data..:? "cancel")
            Prelude.<*> (x Data..:? "clear")
            Prelude.<*> (x Data..:? "position")
            Prelude.<*> (x Data..:? "submit")
      )

instance Prelude.Hashable FormCTA where
  hashWithSalt _salt FormCTA' {..} =
    _salt `Prelude.hashWithSalt` cancel
      `Prelude.hashWithSalt` clear
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` submit

instance Prelude.NFData FormCTA where
  rnf FormCTA' {..} =
    Prelude.rnf cancel
      `Prelude.seq` Prelude.rnf clear
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf submit

instance Data.ToJSON FormCTA where
  toJSON FormCTA' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cancel" Data..=) Prelude.<$> cancel,
            ("clear" Data..=) Prelude.<$> clear,
            ("position" Data..=) Prelude.<$> position,
            ("submit" Data..=) Prelude.<$> submit
          ]
      )
