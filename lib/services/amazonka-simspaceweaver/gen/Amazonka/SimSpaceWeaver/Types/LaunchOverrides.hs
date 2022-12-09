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
-- Module      : Amazonka.SimSpaceWeaver.Types.LaunchOverrides
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.LaunchOverrides where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options that apply when the app starts. These optiAons override default
-- behavior.
--
-- /See:/ 'newLaunchOverrides' smart constructor.
data LaunchOverrides = LaunchOverrides'
  { -- | App launch commands and command line parameters that override the launch
    -- command configured in the simulation schema.
    launchCommands :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchCommands', 'launchOverrides_launchCommands' - App launch commands and command line parameters that override the launch
-- command configured in the simulation schema.
newLaunchOverrides ::
  LaunchOverrides
newLaunchOverrides =
  LaunchOverrides' {launchCommands = Prelude.Nothing}

-- | App launch commands and command line parameters that override the launch
-- command configured in the simulation schema.
launchOverrides_launchCommands :: Lens.Lens' LaunchOverrides (Prelude.Maybe [Prelude.Text])
launchOverrides_launchCommands = Lens.lens (\LaunchOverrides' {launchCommands} -> launchCommands) (\s@LaunchOverrides' {} a -> s {launchCommands = a} :: LaunchOverrides) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LaunchOverrides where
  parseJSON =
    Data.withObject
      "LaunchOverrides"
      ( \x ->
          LaunchOverrides'
            Prelude.<$> ( x Data..:? "LaunchCommands"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LaunchOverrides where
  hashWithSalt _salt LaunchOverrides' {..} =
    _salt `Prelude.hashWithSalt` launchCommands

instance Prelude.NFData LaunchOverrides where
  rnf LaunchOverrides' {..} = Prelude.rnf launchCommands

instance Data.ToJSON LaunchOverrides where
  toJSON LaunchOverrides' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LaunchCommands" Data..=)
              Prelude.<$> launchCommands
          ]
      )
