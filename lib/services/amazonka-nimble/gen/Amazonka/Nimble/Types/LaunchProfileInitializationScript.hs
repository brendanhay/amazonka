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
-- Module      : Amazonka.Nimble.Types.LaunchProfileInitializationScript
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfileInitializationScript where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newLaunchProfileInitializationScript' smart constructor.
data LaunchProfileInitializationScript = LaunchProfileInitializationScript'
  { -- | The initialization script.
    script :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a studio component resource.
    studioComponentId :: Prelude.Maybe Prelude.Text,
    -- | The name for the studio component.
    studioComponentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchProfileInitializationScript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'script', 'launchProfileInitializationScript_script' - The initialization script.
--
-- 'studioComponentId', 'launchProfileInitializationScript_studioComponentId' - The unique identifier for a studio component resource.
--
-- 'studioComponentName', 'launchProfileInitializationScript_studioComponentName' - The name for the studio component.
newLaunchProfileInitializationScript ::
  LaunchProfileInitializationScript
newLaunchProfileInitializationScript =
  LaunchProfileInitializationScript'
    { script =
        Prelude.Nothing,
      studioComponentId = Prelude.Nothing,
      studioComponentName = Prelude.Nothing
    }

-- | The initialization script.
launchProfileInitializationScript_script :: Lens.Lens' LaunchProfileInitializationScript (Prelude.Maybe Prelude.Text)
launchProfileInitializationScript_script = Lens.lens (\LaunchProfileInitializationScript' {script} -> script) (\s@LaunchProfileInitializationScript' {} a -> s {script = a} :: LaunchProfileInitializationScript)

-- | The unique identifier for a studio component resource.
launchProfileInitializationScript_studioComponentId :: Lens.Lens' LaunchProfileInitializationScript (Prelude.Maybe Prelude.Text)
launchProfileInitializationScript_studioComponentId = Lens.lens (\LaunchProfileInitializationScript' {studioComponentId} -> studioComponentId) (\s@LaunchProfileInitializationScript' {} a -> s {studioComponentId = a} :: LaunchProfileInitializationScript)

-- | The name for the studio component.
launchProfileInitializationScript_studioComponentName :: Lens.Lens' LaunchProfileInitializationScript (Prelude.Maybe Prelude.Text)
launchProfileInitializationScript_studioComponentName = Lens.lens (\LaunchProfileInitializationScript' {studioComponentName} -> studioComponentName) (\s@LaunchProfileInitializationScript' {} a -> s {studioComponentName = a} :: LaunchProfileInitializationScript)

instance
  Core.FromJSON
    LaunchProfileInitializationScript
  where
  parseJSON =
    Core.withObject
      "LaunchProfileInitializationScript"
      ( \x ->
          LaunchProfileInitializationScript'
            Prelude.<$> (x Core..:? "script")
            Prelude.<*> (x Core..:? "studioComponentId")
            Prelude.<*> (x Core..:? "studioComponentName")
      )

instance
  Prelude.Hashable
    LaunchProfileInitializationScript

instance
  Prelude.NFData
    LaunchProfileInitializationScript
