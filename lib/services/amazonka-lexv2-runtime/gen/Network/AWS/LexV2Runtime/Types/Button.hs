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
-- Module      : Network.AWS.LexV2Runtime.Types.Button
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.Button where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A button that appears on a response card show to the user.
--
-- /See:/ 'newButton' smart constructor.
data Button = Button'
  { -- | The text that is displayed on the button.
    text :: Prelude.Text,
    -- | The value returned to Amazon Lex V2 when a user chooses the button.
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
-- 'text', 'button_text' - The text that is displayed on the button.
--
-- 'value', 'button_value' - The value returned to Amazon Lex V2 when a user chooses the button.
newButton ::
  -- | 'text'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Button
newButton pText_ pValue_ =
  Button' {text = pText_, value = pValue_}

-- | The text that is displayed on the button.
button_text :: Lens.Lens' Button Prelude.Text
button_text = Lens.lens (\Button' {text} -> text) (\s@Button' {} a -> s {text = a} :: Button)

-- | The value returned to Amazon Lex V2 when a user chooses the button.
button_value :: Lens.Lens' Button Prelude.Text
button_value = Lens.lens (\Button' {value} -> value) (\s@Button' {} a -> s {value = a} :: Button)

instance Core.FromJSON Button where
  parseJSON =
    Core.withObject
      "Button"
      ( \x ->
          Button'
            Prelude.<$> (x Core..: "text") Prelude.<*> (x Core..: "value")
      )

instance Prelude.Hashable Button

instance Prelude.NFData Button

instance Core.ToJSON Button where
  toJSON Button' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("text" Core..= text),
            Prelude.Just ("value" Core..= value)
          ]
      )
