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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormButton
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormButton where

import Amazonka.AmplifyUiBuilder.Types.FieldPosition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for a button UI element that is a part of a
-- form.
--
-- /See:/ 'newFormButton' smart constructor.
data FormButton = FormButton'
  { -- | Describes the button\'s properties.
    children :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the button is visible on the form.
    excluded :: Prelude.Maybe Prelude.Bool,
    -- | The position of the button.
    position :: Prelude.Maybe FieldPosition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormButton' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'children', 'formButton_children' - Describes the button\'s properties.
--
-- 'excluded', 'formButton_excluded' - Specifies whether the button is visible on the form.
--
-- 'position', 'formButton_position' - The position of the button.
newFormButton ::
  FormButton
newFormButton =
  FormButton'
    { children = Prelude.Nothing,
      excluded = Prelude.Nothing,
      position = Prelude.Nothing
    }

-- | Describes the button\'s properties.
formButton_children :: Lens.Lens' FormButton (Prelude.Maybe Prelude.Text)
formButton_children = Lens.lens (\FormButton' {children} -> children) (\s@FormButton' {} a -> s {children = a} :: FormButton)

-- | Specifies whether the button is visible on the form.
formButton_excluded :: Lens.Lens' FormButton (Prelude.Maybe Prelude.Bool)
formButton_excluded = Lens.lens (\FormButton' {excluded} -> excluded) (\s@FormButton' {} a -> s {excluded = a} :: FormButton)

-- | The position of the button.
formButton_position :: Lens.Lens' FormButton (Prelude.Maybe FieldPosition)
formButton_position = Lens.lens (\FormButton' {position} -> position) (\s@FormButton' {} a -> s {position = a} :: FormButton)

instance Data.FromJSON FormButton where
  parseJSON =
    Data.withObject
      "FormButton"
      ( \x ->
          FormButton'
            Prelude.<$> (x Data..:? "children")
            Prelude.<*> (x Data..:? "excluded")
            Prelude.<*> (x Data..:? "position")
      )

instance Prelude.Hashable FormButton where
  hashWithSalt _salt FormButton' {..} =
    _salt `Prelude.hashWithSalt` children
      `Prelude.hashWithSalt` excluded
      `Prelude.hashWithSalt` position

instance Prelude.NFData FormButton where
  rnf FormButton' {..} =
    Prelude.rnf children
      `Prelude.seq` Prelude.rnf excluded
      `Prelude.seq` Prelude.rnf position

instance Data.ToJSON FormButton where
  toJSON FormButton' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("children" Data..=) Prelude.<$> children,
            ("excluded" Data..=) Prelude.<$> excluded,
            ("position" Data..=) Prelude.<$> position
          ]
      )
