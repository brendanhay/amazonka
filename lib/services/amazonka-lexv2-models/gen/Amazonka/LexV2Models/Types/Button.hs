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
-- Module      : Amazonka.LexV2Models.Types.Button
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.Button where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a button to use on a response card used to gather slot values
-- from a user.
--
-- /See:/ 'newButton' smart constructor.
data Button = Button'
  { -- | The text that appears on the button. Use this to tell the user what
    -- value is returned when they choose this button.
    text :: Prelude.Text,
    -- | The value returned to Amazon Lex when the user chooses this button. This
    -- must be one of the slot values configured for the slot.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Button' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'button_text' - The text that appears on the button. Use this to tell the user what
-- value is returned when they choose this button.
--
-- 'value', 'button_value' - The value returned to Amazon Lex when the user chooses this button. This
-- must be one of the slot values configured for the slot.
newButton ::
  -- | 'text'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Button
newButton pText_ pValue_ =
  Button' {text = pText_, value = pValue_}

-- | The text that appears on the button. Use this to tell the user what
-- value is returned when they choose this button.
button_text :: Lens.Lens' Button Prelude.Text
button_text = Lens.lens (\Button' {text} -> text) (\s@Button' {} a -> s {text = a} :: Button)

-- | The value returned to Amazon Lex when the user chooses this button. This
-- must be one of the slot values configured for the slot.
button_value :: Lens.Lens' Button Prelude.Text
button_value = Lens.lens (\Button' {value} -> value) (\s@Button' {} a -> s {value = a} :: Button)

instance Data.FromJSON Button where
  parseJSON =
    Data.withObject
      "Button"
      ( \x ->
          Button'
            Prelude.<$> (x Data..: "text")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable Button where
  hashWithSalt _salt Button' {..} =
    _salt
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` value

instance Prelude.NFData Button where
  rnf Button' {..} =
    Prelude.rnf text `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Button where
  toJSON Button' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("text" Data..= text),
            Prelude.Just ("value" Data..= value)
          ]
      )
