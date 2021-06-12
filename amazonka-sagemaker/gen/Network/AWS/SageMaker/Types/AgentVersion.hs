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
-- Module      : Network.AWS.SageMaker.Types.AgentVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AgentVersion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Edge Manager agent version.
--
-- /See:/ 'newAgentVersion' smart constructor.
data AgentVersion = AgentVersion'
  { -- | Version of the agent.
    version :: Core.Text,
    -- | The number of Edge Manager agents.
    agentCount :: Core.Integer
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
-- 'version', 'agentVersion_version' - Version of the agent.
--
-- 'agentCount', 'agentVersion_agentCount' - The number of Edge Manager agents.
newAgentVersion ::
  -- | 'version'
  Core.Text ->
  -- | 'agentCount'
  Core.Integer ->
  AgentVersion
newAgentVersion pVersion_ pAgentCount_ =
  AgentVersion'
    { version = pVersion_,
      agentCount = pAgentCount_
    }

-- | Version of the agent.
agentVersion_version :: Lens.Lens' AgentVersion Core.Text
agentVersion_version = Lens.lens (\AgentVersion' {version} -> version) (\s@AgentVersion' {} a -> s {version = a} :: AgentVersion)

-- | The number of Edge Manager agents.
agentVersion_agentCount :: Lens.Lens' AgentVersion Core.Integer
agentVersion_agentCount = Lens.lens (\AgentVersion' {agentCount} -> agentCount) (\s@AgentVersion' {} a -> s {agentCount = a} :: AgentVersion)

instance Core.FromJSON AgentVersion where
  parseJSON =
    Core.withObject
      "AgentVersion"
      ( \x ->
          AgentVersion'
            Core.<$> (x Core..: "Version")
            Core.<*> (x Core..: "AgentCount")
      )

instance Core.Hashable AgentVersion

instance Core.NFData AgentVersion
