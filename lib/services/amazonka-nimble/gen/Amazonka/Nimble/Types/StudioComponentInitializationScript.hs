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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioComponentInitializationScript where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Nimble.Types.LaunchProfilePlatform
import Amazonka.Nimble.Types.StudioComponentInitializationScriptRunContext
import qualified Amazonka.Prelude as Prelude

-- | Initialization scripts for studio components.
--
-- /See:/ 'newStudioComponentInitializationScript' smart constructor.
data StudioComponentInitializationScript = StudioComponentInitializationScript'
  { -- | The platform of the initialization script, either WINDOWS or LINUX.
    platform :: Prelude.Maybe LaunchProfilePlatform,
    -- | The version number of the protocol that is used by the launch profile.
    -- The only valid version is \"2021-03-31\".
    launchProfileProtocolVersion :: Prelude.Maybe Prelude.Text,
    -- | The method to use when running the initialization script.
    runContext :: Prelude.Maybe StudioComponentInitializationScriptRunContext,
    -- | The initialization script.
    script :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StudioComponentInitializationScript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'studioComponentInitializationScript_platform' - The platform of the initialization script, either WINDOWS or LINUX.
--
-- 'launchProfileProtocolVersion', 'studioComponentInitializationScript_launchProfileProtocolVersion' - The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
--
-- 'runContext', 'studioComponentInitializationScript_runContext' - The method to use when running the initialization script.
--
-- 'script', 'studioComponentInitializationScript_script' - The initialization script.
newStudioComponentInitializationScript ::
  StudioComponentInitializationScript
newStudioComponentInitializationScript =
  StudioComponentInitializationScript'
    { platform =
        Prelude.Nothing,
      launchProfileProtocolVersion =
        Prelude.Nothing,
      runContext = Prelude.Nothing,
      script = Prelude.Nothing
    }

-- | The platform of the initialization script, either WINDOWS or LINUX.
studioComponentInitializationScript_platform :: Lens.Lens' StudioComponentInitializationScript (Prelude.Maybe LaunchProfilePlatform)
studioComponentInitializationScript_platform = Lens.lens (\StudioComponentInitializationScript' {platform} -> platform) (\s@StudioComponentInitializationScript' {} a -> s {platform = a} :: StudioComponentInitializationScript)

-- | The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
studioComponentInitializationScript_launchProfileProtocolVersion :: Lens.Lens' StudioComponentInitializationScript (Prelude.Maybe Prelude.Text)
studioComponentInitializationScript_launchProfileProtocolVersion = Lens.lens (\StudioComponentInitializationScript' {launchProfileProtocolVersion} -> launchProfileProtocolVersion) (\s@StudioComponentInitializationScript' {} a -> s {launchProfileProtocolVersion = a} :: StudioComponentInitializationScript)

-- | The method to use when running the initialization script.
studioComponentInitializationScript_runContext :: Lens.Lens' StudioComponentInitializationScript (Prelude.Maybe StudioComponentInitializationScriptRunContext)
studioComponentInitializationScript_runContext = Lens.lens (\StudioComponentInitializationScript' {runContext} -> runContext) (\s@StudioComponentInitializationScript' {} a -> s {runContext = a} :: StudioComponentInitializationScript)

-- | The initialization script.
studioComponentInitializationScript_script :: Lens.Lens' StudioComponentInitializationScript (Prelude.Maybe Prelude.Text)
studioComponentInitializationScript_script = Lens.lens (\StudioComponentInitializationScript' {script} -> script) (\s@StudioComponentInitializationScript' {} a -> s {script = a} :: StudioComponentInitializationScript) Prelude.. Lens.mapping Core._Sensitive

instance
  Core.FromJSON
    StudioComponentInitializationScript
  where
  parseJSON =
    Core.withObject
      "StudioComponentInitializationScript"
      ( \x ->
          StudioComponentInitializationScript'
            Prelude.<$> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "launchProfileProtocolVersion")
            Prelude.<*> (x Core..:? "runContext")
            Prelude.<*> (x Core..:? "script")
      )

instance
  Prelude.Hashable
    StudioComponentInitializationScript
  where
  hashWithSalt
    _salt
    StudioComponentInitializationScript' {..} =
      _salt `Prelude.hashWithSalt` platform
        `Prelude.hashWithSalt` launchProfileProtocolVersion
        `Prelude.hashWithSalt` runContext
        `Prelude.hashWithSalt` script

instance
  Prelude.NFData
    StudioComponentInitializationScript
  where
  rnf StudioComponentInitializationScript' {..} =
    Prelude.rnf platform
      `Prelude.seq` Prelude.rnf launchProfileProtocolVersion
      `Prelude.seq` Prelude.rnf runContext
      `Prelude.seq` Prelude.rnf script

instance
  Core.ToJSON
    StudioComponentInitializationScript
  where
  toJSON StudioComponentInitializationScript' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("platform" Core..=) Prelude.<$> platform,
            ("launchProfileProtocolVersion" Core..=)
              Prelude.<$> launchProfileProtocolVersion,
            ("runContext" Core..=) Prelude.<$> runContext,
            ("script" Core..=) Prelude.<$> script
          ]
      )
