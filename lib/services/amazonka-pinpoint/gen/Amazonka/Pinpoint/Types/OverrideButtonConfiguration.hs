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
-- Module      : Amazonka.Pinpoint.Types.OverrideButtonConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.OverrideButtonConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ButtonAction
import qualified Amazonka.Prelude as Prelude

-- | Override button configuration.
--
-- /See:/ 'newOverrideButtonConfiguration' smart constructor.
data OverrideButtonConfiguration = OverrideButtonConfiguration'
  { -- | Button destination.
    link :: Prelude.Maybe Prelude.Text,
    -- | Action triggered by the button.
    buttonAction :: ButtonAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OverrideButtonConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'link', 'overrideButtonConfiguration_link' - Button destination.
--
-- 'buttonAction', 'overrideButtonConfiguration_buttonAction' - Action triggered by the button.
newOverrideButtonConfiguration ::
  -- | 'buttonAction'
  ButtonAction ->
  OverrideButtonConfiguration
newOverrideButtonConfiguration pButtonAction_ =
  OverrideButtonConfiguration'
    { link =
        Prelude.Nothing,
      buttonAction = pButtonAction_
    }

-- | Button destination.
overrideButtonConfiguration_link :: Lens.Lens' OverrideButtonConfiguration (Prelude.Maybe Prelude.Text)
overrideButtonConfiguration_link = Lens.lens (\OverrideButtonConfiguration' {link} -> link) (\s@OverrideButtonConfiguration' {} a -> s {link = a} :: OverrideButtonConfiguration)

-- | Action triggered by the button.
overrideButtonConfiguration_buttonAction :: Lens.Lens' OverrideButtonConfiguration ButtonAction
overrideButtonConfiguration_buttonAction = Lens.lens (\OverrideButtonConfiguration' {buttonAction} -> buttonAction) (\s@OverrideButtonConfiguration' {} a -> s {buttonAction = a} :: OverrideButtonConfiguration)

instance Data.FromJSON OverrideButtonConfiguration where
  parseJSON =
    Data.withObject
      "OverrideButtonConfiguration"
      ( \x ->
          OverrideButtonConfiguration'
            Prelude.<$> (x Data..:? "Link")
            Prelude.<*> (x Data..: "ButtonAction")
      )

instance Prelude.Hashable OverrideButtonConfiguration where
  hashWithSalt _salt OverrideButtonConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` link
      `Prelude.hashWithSalt` buttonAction

instance Prelude.NFData OverrideButtonConfiguration where
  rnf OverrideButtonConfiguration' {..} =
    Prelude.rnf link
      `Prelude.seq` Prelude.rnf buttonAction

instance Data.ToJSON OverrideButtonConfiguration where
  toJSON OverrideButtonConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Link" Data..=) Prelude.<$> link,
            Prelude.Just ("ButtonAction" Data..= buttonAction)
          ]
      )
