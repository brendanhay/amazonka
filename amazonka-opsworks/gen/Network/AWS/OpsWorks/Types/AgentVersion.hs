{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.StackConfigurationManager
import qualified Network.AWS.Prelude as Prelude

-- | Describes an agent version.
--
-- /See:/ 'newAgentVersion' smart constructor.
data AgentVersion = AgentVersion'
  { -- | The agent version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The configuration manager.
    configurationManager :: Prelude.Maybe StackConfigurationManager
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { version = Prelude.Nothing,
      configurationManager = Prelude.Nothing
    }

-- | The agent version.
agentVersion_version :: Lens.Lens' AgentVersion (Prelude.Maybe Prelude.Text)
agentVersion_version = Lens.lens (\AgentVersion' {version} -> version) (\s@AgentVersion' {} a -> s {version = a} :: AgentVersion)

-- | The configuration manager.
agentVersion_configurationManager :: Lens.Lens' AgentVersion (Prelude.Maybe StackConfigurationManager)
agentVersion_configurationManager = Lens.lens (\AgentVersion' {configurationManager} -> configurationManager) (\s@AgentVersion' {} a -> s {configurationManager = a} :: AgentVersion)

instance Prelude.FromJSON AgentVersion where
  parseJSON =
    Prelude.withObject
      "AgentVersion"
      ( \x ->
          AgentVersion'
            Prelude.<$> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "ConfigurationManager")
      )

instance Prelude.Hashable AgentVersion

instance Prelude.NFData AgentVersion
