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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfileInitializationScript where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Launch Profile Initialization Script is used when start streaming
-- session runs.
--
-- /See:/ 'newLaunchProfileInitializationScript' smart constructor.
data LaunchProfileInitializationScript = LaunchProfileInitializationScript'
  { -- | An IAM role attached to Studio Component when the system initialization
    -- script runs which give the studio component access to AWS resources when
    -- the system initialization script runs.
    secureInitializationRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a studio component resource.
    studioComponentId :: Prelude.Maybe Prelude.Text,
    -- | An IAM role attached to a Studio Component that gives the studio
    -- component access to AWS resources at anytime while the instance is
    -- running.
    runtimeRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name for the studio component.
    studioComponentName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The initialization script.
    script :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchProfileInitializationScript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secureInitializationRoleArn', 'launchProfileInitializationScript_secureInitializationRoleArn' - An IAM role attached to Studio Component when the system initialization
-- script runs which give the studio component access to AWS resources when
-- the system initialization script runs.
--
-- 'studioComponentId', 'launchProfileInitializationScript_studioComponentId' - The unique identifier for a studio component resource.
--
-- 'runtimeRoleArn', 'launchProfileInitializationScript_runtimeRoleArn' - An IAM role attached to a Studio Component that gives the studio
-- component access to AWS resources at anytime while the instance is
-- running.
--
-- 'studioComponentName', 'launchProfileInitializationScript_studioComponentName' - The name for the studio component.
--
-- 'script', 'launchProfileInitializationScript_script' - The initialization script.
newLaunchProfileInitializationScript ::
  LaunchProfileInitializationScript
newLaunchProfileInitializationScript =
  LaunchProfileInitializationScript'
    { secureInitializationRoleArn =
        Prelude.Nothing,
      studioComponentId = Prelude.Nothing,
      runtimeRoleArn = Prelude.Nothing,
      studioComponentName = Prelude.Nothing,
      script = Prelude.Nothing
    }

-- | An IAM role attached to Studio Component when the system initialization
-- script runs which give the studio component access to AWS resources when
-- the system initialization script runs.
launchProfileInitializationScript_secureInitializationRoleArn :: Lens.Lens' LaunchProfileInitializationScript (Prelude.Maybe Prelude.Text)
launchProfileInitializationScript_secureInitializationRoleArn = Lens.lens (\LaunchProfileInitializationScript' {secureInitializationRoleArn} -> secureInitializationRoleArn) (\s@LaunchProfileInitializationScript' {} a -> s {secureInitializationRoleArn = a} :: LaunchProfileInitializationScript)

-- | The unique identifier for a studio component resource.
launchProfileInitializationScript_studioComponentId :: Lens.Lens' LaunchProfileInitializationScript (Prelude.Maybe Prelude.Text)
launchProfileInitializationScript_studioComponentId = Lens.lens (\LaunchProfileInitializationScript' {studioComponentId} -> studioComponentId) (\s@LaunchProfileInitializationScript' {} a -> s {studioComponentId = a} :: LaunchProfileInitializationScript)

-- | An IAM role attached to a Studio Component that gives the studio
-- component access to AWS resources at anytime while the instance is
-- running.
launchProfileInitializationScript_runtimeRoleArn :: Lens.Lens' LaunchProfileInitializationScript (Prelude.Maybe Prelude.Text)
launchProfileInitializationScript_runtimeRoleArn = Lens.lens (\LaunchProfileInitializationScript' {runtimeRoleArn} -> runtimeRoleArn) (\s@LaunchProfileInitializationScript' {} a -> s {runtimeRoleArn = a} :: LaunchProfileInitializationScript)

-- | The name for the studio component.
launchProfileInitializationScript_studioComponentName :: Lens.Lens' LaunchProfileInitializationScript (Prelude.Maybe Prelude.Text)
launchProfileInitializationScript_studioComponentName = Lens.lens (\LaunchProfileInitializationScript' {studioComponentName} -> studioComponentName) (\s@LaunchProfileInitializationScript' {} a -> s {studioComponentName = a} :: LaunchProfileInitializationScript) Prelude.. Lens.mapping Data._Sensitive

-- | The initialization script.
launchProfileInitializationScript_script :: Lens.Lens' LaunchProfileInitializationScript (Prelude.Maybe Prelude.Text)
launchProfileInitializationScript_script = Lens.lens (\LaunchProfileInitializationScript' {script} -> script) (\s@LaunchProfileInitializationScript' {} a -> s {script = a} :: LaunchProfileInitializationScript) Prelude.. Lens.mapping Data._Sensitive

instance
  Data.FromJSON
    LaunchProfileInitializationScript
  where
  parseJSON =
    Data.withObject
      "LaunchProfileInitializationScript"
      ( \x ->
          LaunchProfileInitializationScript'
            Prelude.<$> (x Data..:? "secureInitializationRoleArn")
            Prelude.<*> (x Data..:? "studioComponentId")
            Prelude.<*> (x Data..:? "runtimeRoleArn")
            Prelude.<*> (x Data..:? "studioComponentName")
            Prelude.<*> (x Data..:? "script")
      )

instance
  Prelude.Hashable
    LaunchProfileInitializationScript
  where
  hashWithSalt
    _salt
    LaunchProfileInitializationScript' {..} =
      _salt
        `Prelude.hashWithSalt` secureInitializationRoleArn
        `Prelude.hashWithSalt` studioComponentId
        `Prelude.hashWithSalt` runtimeRoleArn
        `Prelude.hashWithSalt` studioComponentName
        `Prelude.hashWithSalt` script

instance
  Prelude.NFData
    LaunchProfileInitializationScript
  where
  rnf LaunchProfileInitializationScript' {..} =
    Prelude.rnf secureInitializationRoleArn
      `Prelude.seq` Prelude.rnf studioComponentId
      `Prelude.seq` Prelude.rnf runtimeRoleArn
      `Prelude.seq` Prelude.rnf studioComponentName
      `Prelude.seq` Prelude.rnf script
