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
-- Module      : Amazonka.Nimble.Types.StudioComponentInitializationScript
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioComponentInitializationScript where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.LaunchProfilePlatform
import Amazonka.Nimble.Types.StudioComponentInitializationScriptRunContext
import qualified Amazonka.Prelude as Prelude

-- | Initialization scripts for studio components.
--
-- /See:/ 'newStudioComponentInitializationScript' smart constructor.
data StudioComponentInitializationScript = StudioComponentInitializationScript'
  { -- | The initialization script.
    script :: Prelude.Maybe Prelude.Text,
    -- | The platform of the initialization script, either WINDOWS or LINUX.
    platform :: Prelude.Maybe LaunchProfilePlatform,
    -- | The method to use when running the initialization script.
    runContext :: Prelude.Maybe StudioComponentInitializationScriptRunContext,
    -- | The version number of the protocol that is used by the launch profile.
    -- The only valid version is \"2021-03-31\".
    launchProfileProtocolVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StudioComponentInitializationScript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'script', 'studioComponentInitializationScript_script' - The initialization script.
--
-- 'platform', 'studioComponentInitializationScript_platform' - The platform of the initialization script, either WINDOWS or LINUX.
--
-- 'runContext', 'studioComponentInitializationScript_runContext' - The method to use when running the initialization script.
--
-- 'launchProfileProtocolVersion', 'studioComponentInitializationScript_launchProfileProtocolVersion' - The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
newStudioComponentInitializationScript ::
  StudioComponentInitializationScript
newStudioComponentInitializationScript =
  StudioComponentInitializationScript'
    { script =
        Prelude.Nothing,
      platform = Prelude.Nothing,
      runContext = Prelude.Nothing,
      launchProfileProtocolVersion =
        Prelude.Nothing
    }

-- | The initialization script.
studioComponentInitializationScript_script :: Lens.Lens' StudioComponentInitializationScript (Prelude.Maybe Prelude.Text)
studioComponentInitializationScript_script = Lens.lens (\StudioComponentInitializationScript' {script} -> script) (\s@StudioComponentInitializationScript' {} a -> s {script = a} :: StudioComponentInitializationScript)

-- | The platform of the initialization script, either WINDOWS or LINUX.
studioComponentInitializationScript_platform :: Lens.Lens' StudioComponentInitializationScript (Prelude.Maybe LaunchProfilePlatform)
studioComponentInitializationScript_platform = Lens.lens (\StudioComponentInitializationScript' {platform} -> platform) (\s@StudioComponentInitializationScript' {} a -> s {platform = a} :: StudioComponentInitializationScript)

-- | The method to use when running the initialization script.
studioComponentInitializationScript_runContext :: Lens.Lens' StudioComponentInitializationScript (Prelude.Maybe StudioComponentInitializationScriptRunContext)
studioComponentInitializationScript_runContext = Lens.lens (\StudioComponentInitializationScript' {runContext} -> runContext) (\s@StudioComponentInitializationScript' {} a -> s {runContext = a} :: StudioComponentInitializationScript)

-- | The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
studioComponentInitializationScript_launchProfileProtocolVersion :: Lens.Lens' StudioComponentInitializationScript (Prelude.Maybe Prelude.Text)
studioComponentInitializationScript_launchProfileProtocolVersion = Lens.lens (\StudioComponentInitializationScript' {launchProfileProtocolVersion} -> launchProfileProtocolVersion) (\s@StudioComponentInitializationScript' {} a -> s {launchProfileProtocolVersion = a} :: StudioComponentInitializationScript)

instance
  Core.FromJSON
    StudioComponentInitializationScript
  where
  parseJSON =
    Core.withObject
      "StudioComponentInitializationScript"
      ( \x ->
          StudioComponentInitializationScript'
            Prelude.<$> (x Core..:? "script")
            Prelude.<*> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "runContext")
            Prelude.<*> (x Core..:? "launchProfileProtocolVersion")
      )

instance
  Prelude.Hashable
    StudioComponentInitializationScript
  where
  hashWithSalt
    _salt
    StudioComponentInitializationScript' {..} =
      _salt `Prelude.hashWithSalt` script
        `Prelude.hashWithSalt` platform
        `Prelude.hashWithSalt` runContext
        `Prelude.hashWithSalt` launchProfileProtocolVersion

instance
  Prelude.NFData
    StudioComponentInitializationScript
  where
  rnf StudioComponentInitializationScript' {..} =
    Prelude.rnf script
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf runContext
      `Prelude.seq` Prelude.rnf launchProfileProtocolVersion

instance
  Core.ToJSON
    StudioComponentInitializationScript
  where
  toJSON StudioComponentInitializationScript' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("script" Core..=) Prelude.<$> script,
            ("platform" Core..=) Prelude.<$> platform,
            ("runContext" Core..=) Prelude.<$> runContext,
            ("launchProfileProtocolVersion" Core..=)
              Prelude.<$> launchProfileProtocolVersion
          ]
      )
