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
-- Module      : Amazonka.LexRuntime.Types.Button
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.Button where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an option to be shown on the client platform (Facebook,
-- Slack, etc.)
--
-- /See:/ 'newButton' smart constructor.
data Button = Button'
  { -- | Text that is visible to the user on the button.
    text :: Prelude.Text,
    -- | The value sent to Amazon Lex when a user chooses the button. For
    -- example, consider button text \"NYC.\" When the user chooses the button,
    -- the value sent can be \"New York City.\"
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
-- 'text', 'button_text' - Text that is visible to the user on the button.
--
-- 'value', 'button_value' - The value sent to Amazon Lex when a user chooses the button. For
-- example, consider button text \"NYC.\" When the user chooses the button,
-- the value sent can be \"New York City.\"
newButton ::
  -- | 'text'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Button
newButton pText_ pValue_ =
  Button' {text = pText_, value = pValue_}

-- | Text that is visible to the user on the button.
button_text :: Lens.Lens' Button Prelude.Text
button_text = Lens.lens (\Button' {text} -> text) (\s@Button' {} a -> s {text = a} :: Button)

-- | The value sent to Amazon Lex when a user chooses the button. For
-- example, consider button text \"NYC.\" When the user chooses the button,
-- the value sent can be \"New York City.\"
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
