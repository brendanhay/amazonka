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
-- Module      : Network.AWS.Inspector.Types.AgentPreview
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AgentPreview where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types.AgentHealth
import qualified Network.AWS.Lens as Lens

-- | Used as a response element in the PreviewAgents action.
--
-- /See:/ 'newAgentPreview' smart constructor.
data AgentPreview = AgentPreview'
  { -- | The hostname of the EC2 instance on which the Amazon Inspector Agent is
    -- installed.
    hostname :: Core.Maybe Core.Text,
    -- | The version of the Amazon Inspector Agent.
    agentVersion :: Core.Maybe Core.Text,
    -- | The kernel version of the operating system running on the EC2 instance
    -- on which the Amazon Inspector Agent is installed.
    kernelVersion :: Core.Maybe Core.Text,
    -- | The operating system running on the EC2 instance on which the Amazon
    -- Inspector Agent is installed.
    operatingSystem :: Core.Maybe Core.Text,
    -- | The health status of the Amazon Inspector Agent.
    agentHealth :: Core.Maybe AgentHealth,
    -- | The Auto Scaling group for the EC2 instance where the agent is
    -- installed.
    autoScalingGroup :: Core.Maybe Core.Text,
    -- | The IP address of the EC2 instance on which the Amazon Inspector Agent
    -- is installed.
    ipv4Address :: Core.Maybe Core.Text,
    -- | The ID of the EC2 instance where the agent is installed.
    agentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AgentPreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'agentPreview_hostname' - The hostname of the EC2 instance on which the Amazon Inspector Agent is
-- installed.
--
-- 'agentVersion', 'agentPreview_agentVersion' - The version of the Amazon Inspector Agent.
--
-- 'kernelVersion', 'agentPreview_kernelVersion' - The kernel version of the operating system running on the EC2 instance
-- on which the Amazon Inspector Agent is installed.
--
-- 'operatingSystem', 'agentPreview_operatingSystem' - The operating system running on the EC2 instance on which the Amazon
-- Inspector Agent is installed.
--
-- 'agentHealth', 'agentPreview_agentHealth' - The health status of the Amazon Inspector Agent.
--
-- 'autoScalingGroup', 'agentPreview_autoScalingGroup' - The Auto Scaling group for the EC2 instance where the agent is
-- installed.
--
-- 'ipv4Address', 'agentPreview_ipv4Address' - The IP address of the EC2 instance on which the Amazon Inspector Agent
-- is installed.
--
-- 'agentId', 'agentPreview_agentId' - The ID of the EC2 instance where the agent is installed.
newAgentPreview ::
  -- | 'agentId'
  Core.Text ->
  AgentPreview
newAgentPreview pAgentId_ =
  AgentPreview'
    { hostname = Core.Nothing,
      agentVersion = Core.Nothing,
      kernelVersion = Core.Nothing,
      operatingSystem = Core.Nothing,
      agentHealth = Core.Nothing,
      autoScalingGroup = Core.Nothing,
      ipv4Address = Core.Nothing,
      agentId = pAgentId_
    }

-- | The hostname of the EC2 instance on which the Amazon Inspector Agent is
-- installed.
agentPreview_hostname :: Lens.Lens' AgentPreview (Core.Maybe Core.Text)
agentPreview_hostname = Lens.lens (\AgentPreview' {hostname} -> hostname) (\s@AgentPreview' {} a -> s {hostname = a} :: AgentPreview)

-- | The version of the Amazon Inspector Agent.
agentPreview_agentVersion :: Lens.Lens' AgentPreview (Core.Maybe Core.Text)
agentPreview_agentVersion = Lens.lens (\AgentPreview' {agentVersion} -> agentVersion) (\s@AgentPreview' {} a -> s {agentVersion = a} :: AgentPreview)

-- | The kernel version of the operating system running on the EC2 instance
-- on which the Amazon Inspector Agent is installed.
agentPreview_kernelVersion :: Lens.Lens' AgentPreview (Core.Maybe Core.Text)
agentPreview_kernelVersion = Lens.lens (\AgentPreview' {kernelVersion} -> kernelVersion) (\s@AgentPreview' {} a -> s {kernelVersion = a} :: AgentPreview)

-- | The operating system running on the EC2 instance on which the Amazon
-- Inspector Agent is installed.
agentPreview_operatingSystem :: Lens.Lens' AgentPreview (Core.Maybe Core.Text)
agentPreview_operatingSystem = Lens.lens (\AgentPreview' {operatingSystem} -> operatingSystem) (\s@AgentPreview' {} a -> s {operatingSystem = a} :: AgentPreview)

-- | The health status of the Amazon Inspector Agent.
agentPreview_agentHealth :: Lens.Lens' AgentPreview (Core.Maybe AgentHealth)
agentPreview_agentHealth = Lens.lens (\AgentPreview' {agentHealth} -> agentHealth) (\s@AgentPreview' {} a -> s {agentHealth = a} :: AgentPreview)

-- | The Auto Scaling group for the EC2 instance where the agent is
-- installed.
agentPreview_autoScalingGroup :: Lens.Lens' AgentPreview (Core.Maybe Core.Text)
agentPreview_autoScalingGroup = Lens.lens (\AgentPreview' {autoScalingGroup} -> autoScalingGroup) (\s@AgentPreview' {} a -> s {autoScalingGroup = a} :: AgentPreview)

-- | The IP address of the EC2 instance on which the Amazon Inspector Agent
-- is installed.
agentPreview_ipv4Address :: Lens.Lens' AgentPreview (Core.Maybe Core.Text)
agentPreview_ipv4Address = Lens.lens (\AgentPreview' {ipv4Address} -> ipv4Address) (\s@AgentPreview' {} a -> s {ipv4Address = a} :: AgentPreview)

-- | The ID of the EC2 instance where the agent is installed.
agentPreview_agentId :: Lens.Lens' AgentPreview Core.Text
agentPreview_agentId = Lens.lens (\AgentPreview' {agentId} -> agentId) (\s@AgentPreview' {} a -> s {agentId = a} :: AgentPreview)

instance Core.FromJSON AgentPreview where
  parseJSON =
    Core.withObject
      "AgentPreview"
      ( \x ->
          AgentPreview'
            Core.<$> (x Core..:? "hostname")
            Core.<*> (x Core..:? "agentVersion")
            Core.<*> (x Core..:? "kernelVersion")
            Core.<*> (x Core..:? "operatingSystem")
            Core.<*> (x Core..:? "agentHealth")
            Core.<*> (x Core..:? "autoScalingGroup")
            Core.<*> (x Core..:? "ipv4Address")
            Core.<*> (x Core..: "agentId")
      )

instance Core.Hashable AgentPreview

instance Core.NFData AgentPreview
