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
-- Module      : Amazonka.OpsWorks.Types.AgentVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.AgentVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorks.Types.StackConfigurationManager
import qualified Amazonka.Prelude as Prelude

-- | Describes an agent version.
--
-- /See:/ 'newAgentVersion' smart constructor.
data AgentVersion = AgentVersion'
  { -- | The configuration manager.
    configurationManager :: Prelude.Maybe StackConfigurationManager,
    -- | The agent version.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationManager', 'agentVersion_configurationManager' - The configuration manager.
--
-- 'version', 'agentVersion_version' - The agent version.
newAgentVersion ::
  AgentVersion
newAgentVersion =
  AgentVersion'
    { configurationManager =
        Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The configuration manager.
agentVersion_configurationManager :: Lens.Lens' AgentVersion (Prelude.Maybe StackConfigurationManager)
agentVersion_configurationManager = Lens.lens (\AgentVersion' {configurationManager} -> configurationManager) (\s@AgentVersion' {} a -> s {configurationManager = a} :: AgentVersion)

-- | The agent version.
agentVersion_version :: Lens.Lens' AgentVersion (Prelude.Maybe Prelude.Text)
agentVersion_version = Lens.lens (\AgentVersion' {version} -> version) (\s@AgentVersion' {} a -> s {version = a} :: AgentVersion)

instance Core.FromJSON AgentVersion where
  parseJSON =
    Core.withObject
      "AgentVersion"
      ( \x ->
          AgentVersion'
            Prelude.<$> (x Core..:? "ConfigurationManager")
            Prelude.<*> (x Core..:? "Version")
      )

instance Prelude.Hashable AgentVersion where
  hashWithSalt _salt AgentVersion' {..} =
    _salt `Prelude.hashWithSalt` configurationManager
      `Prelude.hashWithSalt` version

instance Prelude.NFData AgentVersion where
  rnf AgentVersion' {..} =
    Prelude.rnf configurationManager
      `Prelude.seq` Prelude.rnf version
