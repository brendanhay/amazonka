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
-- Module      : Amazonka.Pinpoint.Types.DefaultButtonConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.DefaultButtonConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ButtonAction
import qualified Amazonka.Prelude as Prelude

-- | Default button configuration.
--
-- /See:/ 'newDefaultButtonConfiguration' smart constructor.
data DefaultButtonConfiguration = DefaultButtonConfiguration'
  { -- | The background color of the button.
    backgroundColor :: Prelude.Maybe Prelude.Text,
    -- | The border radius of the button.
    borderRadius :: Prelude.Maybe Prelude.Int,
    -- | Button destination.
    link :: Prelude.Maybe Prelude.Text,
    -- | The text color of the button.
    textColor :: Prelude.Maybe Prelude.Text,
    -- | Action triggered by the button.
    buttonAction :: ButtonAction,
    -- | Button text.
    text :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultButtonConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backgroundColor', 'defaultButtonConfiguration_backgroundColor' - The background color of the button.
--
-- 'borderRadius', 'defaultButtonConfiguration_borderRadius' - The border radius of the button.
--
-- 'link', 'defaultButtonConfiguration_link' - Button destination.
--
-- 'textColor', 'defaultButtonConfiguration_textColor' - The text color of the button.
--
-- 'buttonAction', 'defaultButtonConfiguration_buttonAction' - Action triggered by the button.
--
-- 'text', 'defaultButtonConfiguration_text' - Button text.
newDefaultButtonConfiguration ::
  -- | 'buttonAction'
  ButtonAction ->
  -- | 'text'
  Prelude.Text ->
  DefaultButtonConfiguration
newDefaultButtonConfiguration pButtonAction_ pText_ =
  DefaultButtonConfiguration'
    { backgroundColor =
        Prelude.Nothing,
      borderRadius = Prelude.Nothing,
      link = Prelude.Nothing,
      textColor = Prelude.Nothing,
      buttonAction = pButtonAction_,
      text = pText_
    }

-- | The background color of the button.
defaultButtonConfiguration_backgroundColor :: Lens.Lens' DefaultButtonConfiguration (Prelude.Maybe Prelude.Text)
defaultButtonConfiguration_backgroundColor = Lens.lens (\DefaultButtonConfiguration' {backgroundColor} -> backgroundColor) (\s@DefaultButtonConfiguration' {} a -> s {backgroundColor = a} :: DefaultButtonConfiguration)

-- | The border radius of the button.
defaultButtonConfiguration_borderRadius :: Lens.Lens' DefaultButtonConfiguration (Prelude.Maybe Prelude.Int)
defaultButtonConfiguration_borderRadius = Lens.lens (\DefaultButtonConfiguration' {borderRadius} -> borderRadius) (\s@DefaultButtonConfiguration' {} a -> s {borderRadius = a} :: DefaultButtonConfiguration)

-- | Button destination.
defaultButtonConfiguration_link :: Lens.Lens' DefaultButtonConfiguration (Prelude.Maybe Prelude.Text)
defaultButtonConfiguration_link = Lens.lens (\DefaultButtonConfiguration' {link} -> link) (\s@DefaultButtonConfiguration' {} a -> s {link = a} :: DefaultButtonConfiguration)

-- | The text color of the button.
defaultButtonConfiguration_textColor :: Lens.Lens' DefaultButtonConfiguration (Prelude.Maybe Prelude.Text)
defaultButtonConfiguration_textColor = Lens.lens (\DefaultButtonConfiguration' {textColor} -> textColor) (\s@DefaultButtonConfiguration' {} a -> s {textColor = a} :: DefaultButtonConfiguration)

-- | Action triggered by the button.
defaultButtonConfiguration_buttonAction :: Lens.Lens' DefaultButtonConfiguration ButtonAction
defaultButtonConfiguration_buttonAction = Lens.lens (\DefaultButtonConfiguration' {buttonAction} -> buttonAction) (\s@DefaultButtonConfiguration' {} a -> s {buttonAction = a} :: DefaultButtonConfiguration)

-- | Button text.
defaultButtonConfiguration_text :: Lens.Lens' DefaultButtonConfiguration Prelude.Text
defaultButtonConfiguration_text = Lens.lens (\DefaultButtonConfiguration' {text} -> text) (\s@DefaultButtonConfiguration' {} a -> s {text = a} :: DefaultButtonConfiguration)

instance Data.FromJSON DefaultButtonConfiguration where
  parseJSON =
    Data.withObject
      "DefaultButtonConfiguration"
      ( \x ->
          DefaultButtonConfiguration'
            Prelude.<$> (x Data..:? "BackgroundColor")
            Prelude.<*> (x Data..:? "BorderRadius")
            Prelude.<*> (x Data..:? "Link")
            Prelude.<*> (x Data..:? "TextColor")
            Prelude.<*> (x Data..: "ButtonAction")
            Prelude.<*> (x Data..: "Text")
      )

instance Prelude.Hashable DefaultButtonConfiguration where
  hashWithSalt _salt DefaultButtonConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` backgroundColor
      `Prelude.hashWithSalt` borderRadius
      `Prelude.hashWithSalt` link
      `Prelude.hashWithSalt` textColor
      `Prelude.hashWithSalt` buttonAction
      `Prelude.hashWithSalt` text

instance Prelude.NFData DefaultButtonConfiguration where
  rnf DefaultButtonConfiguration' {..} =
    Prelude.rnf backgroundColor
      `Prelude.seq` Prelude.rnf borderRadius
      `Prelude.seq` Prelude.rnf link
      `Prelude.seq` Prelude.rnf textColor
      `Prelude.seq` Prelude.rnf buttonAction
      `Prelude.seq` Prelude.rnf text

instance Data.ToJSON DefaultButtonConfiguration where
  toJSON DefaultButtonConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackgroundColor" Data..=)
              Prelude.<$> backgroundColor,
            ("BorderRadius" Data..=) Prelude.<$> borderRadius,
            ("Link" Data..=) Prelude.<$> link,
            ("TextColor" Data..=) Prelude.<$> textColor,
            Prelude.Just ("ButtonAction" Data..= buttonAction),
            Prelude.Just ("Text" Data..= text)
          ]
      )
