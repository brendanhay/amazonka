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
-- Module      : Amazonka.GroundStation.Types.AgentDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.AgentDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.ComponentVersion
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about the agent.
--
-- /See:/ 'newAgentDetails' smart constructor.
data AgentDetails = AgentDetails'
  { -- | List of CPU cores reserved for the agent.
    agentCpuCores :: Prelude.Maybe [Prelude.Int],
    -- | This field should not be used. Use agentCpuCores instead.
    --
    -- List of CPU cores reserved for processes other than the agent running on
    -- the EC2 instance.
    reservedCpuCores :: Prelude.Maybe [Prelude.Int],
    -- | Current agent version.
    agentVersion :: Prelude.Text,
    -- | List of versions being used by agent components.
    componentVersions :: Prelude.NonEmpty ComponentVersion,
    -- | ID of EC2 instance agent is running on.
    instanceId :: Prelude.Text,
    -- | Type of EC2 instance agent is running on.
    instanceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentCpuCores', 'agentDetails_agentCpuCores' - List of CPU cores reserved for the agent.
--
-- 'reservedCpuCores', 'agentDetails_reservedCpuCores' - This field should not be used. Use agentCpuCores instead.
--
-- List of CPU cores reserved for processes other than the agent running on
-- the EC2 instance.
--
-- 'agentVersion', 'agentDetails_agentVersion' - Current agent version.
--
-- 'componentVersions', 'agentDetails_componentVersions' - List of versions being used by agent components.
--
-- 'instanceId', 'agentDetails_instanceId' - ID of EC2 instance agent is running on.
--
-- 'instanceType', 'agentDetails_instanceType' - Type of EC2 instance agent is running on.
newAgentDetails ::
  -- | 'agentVersion'
  Prelude.Text ->
  -- | 'componentVersions'
  Prelude.NonEmpty ComponentVersion ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'instanceType'
  Prelude.Text ->
  AgentDetails
newAgentDetails
  pAgentVersion_
  pComponentVersions_
  pInstanceId_
  pInstanceType_ =
    AgentDetails'
      { agentCpuCores = Prelude.Nothing,
        reservedCpuCores = Prelude.Nothing,
        agentVersion = pAgentVersion_,
        componentVersions =
          Lens.coerced Lens.# pComponentVersions_,
        instanceId = pInstanceId_,
        instanceType = pInstanceType_
      }

-- | List of CPU cores reserved for the agent.
agentDetails_agentCpuCores :: Lens.Lens' AgentDetails (Prelude.Maybe [Prelude.Int])
agentDetails_agentCpuCores = Lens.lens (\AgentDetails' {agentCpuCores} -> agentCpuCores) (\s@AgentDetails' {} a -> s {agentCpuCores = a} :: AgentDetails) Prelude.. Lens.mapping Lens.coerced

-- | This field should not be used. Use agentCpuCores instead.
--
-- List of CPU cores reserved for processes other than the agent running on
-- the EC2 instance.
agentDetails_reservedCpuCores :: Lens.Lens' AgentDetails (Prelude.Maybe [Prelude.Int])
agentDetails_reservedCpuCores = Lens.lens (\AgentDetails' {reservedCpuCores} -> reservedCpuCores) (\s@AgentDetails' {} a -> s {reservedCpuCores = a} :: AgentDetails) Prelude.. Lens.mapping Lens.coerced

-- | Current agent version.
agentDetails_agentVersion :: Lens.Lens' AgentDetails Prelude.Text
agentDetails_agentVersion = Lens.lens (\AgentDetails' {agentVersion} -> agentVersion) (\s@AgentDetails' {} a -> s {agentVersion = a} :: AgentDetails)

-- | List of versions being used by agent components.
agentDetails_componentVersions :: Lens.Lens' AgentDetails (Prelude.NonEmpty ComponentVersion)
agentDetails_componentVersions = Lens.lens (\AgentDetails' {componentVersions} -> componentVersions) (\s@AgentDetails' {} a -> s {componentVersions = a} :: AgentDetails) Prelude.. Lens.coerced

-- | ID of EC2 instance agent is running on.
agentDetails_instanceId :: Lens.Lens' AgentDetails Prelude.Text
agentDetails_instanceId = Lens.lens (\AgentDetails' {instanceId} -> instanceId) (\s@AgentDetails' {} a -> s {instanceId = a} :: AgentDetails)

-- | Type of EC2 instance agent is running on.
agentDetails_instanceType :: Lens.Lens' AgentDetails Prelude.Text
agentDetails_instanceType = Lens.lens (\AgentDetails' {instanceType} -> instanceType) (\s@AgentDetails' {} a -> s {instanceType = a} :: AgentDetails)

instance Prelude.Hashable AgentDetails where
  hashWithSalt _salt AgentDetails' {..} =
    _salt
      `Prelude.hashWithSalt` agentCpuCores
      `Prelude.hashWithSalt` reservedCpuCores
      `Prelude.hashWithSalt` agentVersion
      `Prelude.hashWithSalt` componentVersions
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceType

instance Prelude.NFData AgentDetails where
  rnf AgentDetails' {..} =
    Prelude.rnf agentCpuCores
      `Prelude.seq` Prelude.rnf reservedCpuCores
      `Prelude.seq` Prelude.rnf agentVersion
      `Prelude.seq` Prelude.rnf componentVersions
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf instanceType

instance Data.ToJSON AgentDetails where
  toJSON AgentDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("agentCpuCores" Data..=) Prelude.<$> agentCpuCores,
            ("reservedCpuCores" Data..=)
              Prelude.<$> reservedCpuCores,
            Prelude.Just ("agentVersion" Data..= agentVersion),
            Prelude.Just
              ("componentVersions" Data..= componentVersions),
            Prelude.Just ("instanceId" Data..= instanceId),
            Prelude.Just ("instanceType" Data..= instanceType)
          ]
      )
