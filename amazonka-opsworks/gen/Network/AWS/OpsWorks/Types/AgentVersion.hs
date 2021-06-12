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
-- Module      : Network.AWS.OpsWorks.Types.AgentVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.AgentVersion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.StackConfigurationManager

-- | Describes an agent version.
--
-- /See:/ 'newAgentVersion' smart constructor.
data AgentVersion = AgentVersion'
  { -- | The agent version.
    version :: Core.Maybe Core.Text,
    -- | The configuration manager.
    configurationManager :: Core.Maybe StackConfigurationManager
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AgentVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'agentVersion_version' - The agent version.
--
-- 'configurationManager', 'agentVersion_configurationManager' - The configuration manager.
newAgentVersion ::
  AgentVersion
newAgentVersion =
  AgentVersion'
    { version = Core.Nothing,
      configurationManager = Core.Nothing
    }

-- | The agent version.
agentVersion_version :: Lens.Lens' AgentVersion (Core.Maybe Core.Text)
agentVersion_version = Lens.lens (\AgentVersion' {version} -> version) (\s@AgentVersion' {} a -> s {version = a} :: AgentVersion)

-- | The configuration manager.
agentVersion_configurationManager :: Lens.Lens' AgentVersion (Core.Maybe StackConfigurationManager)
agentVersion_configurationManager = Lens.lens (\AgentVersion' {configurationManager} -> configurationManager) (\s@AgentVersion' {} a -> s {configurationManager = a} :: AgentVersion)

instance Core.FromJSON AgentVersion where
  parseJSON =
    Core.withObject
      "AgentVersion"
      ( \x ->
          AgentVersion'
            Core.<$> (x Core..:? "Version")
            Core.<*> (x Core..:? "ConfigurationManager")
      )

instance Core.Hashable AgentVersion

instance Core.NFData AgentVersion
