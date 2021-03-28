{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AgentPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AgentPreview
  ( AgentPreview (..)
  -- * Smart constructor
  , mkAgentPreview
  -- * Lenses
  , apAgentId
  , apAgentHealth
  , apAgentVersion
  , apAutoScalingGroup
  , apHostname
  , apIpv4Address
  , apKernelVersion
  , apOperatingSystem
  ) where

import qualified Network.AWS.Inspector.Types.AgentHealth as Types
import qualified Network.AWS.Inspector.Types.AgentId as Types
import qualified Network.AWS.Inspector.Types.AgentVersion as Types
import qualified Network.AWS.Inspector.Types.AutoScalingGroup as Types
import qualified Network.AWS.Inspector.Types.Hostname as Types
import qualified Network.AWS.Inspector.Types.Ipv4Address as Types
import qualified Network.AWS.Inspector.Types.KernelVersion as Types
import qualified Network.AWS.Inspector.Types.OperatingSystem as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used as a response element in the 'PreviewAgents' action.
--
-- /See:/ 'mkAgentPreview' smart constructor.
data AgentPreview = AgentPreview'
  { agentId :: Types.AgentId
    -- ^ The ID of the EC2 instance where the agent is installed.
  , agentHealth :: Core.Maybe Types.AgentHealth
    -- ^ The health status of the Amazon Inspector Agent.
  , agentVersion :: Core.Maybe Types.AgentVersion
    -- ^ The version of the Amazon Inspector Agent.
  , autoScalingGroup :: Core.Maybe Types.AutoScalingGroup
    -- ^ The Auto Scaling group for the EC2 instance where the agent is installed.
  , hostname :: Core.Maybe Types.Hostname
    -- ^ The hostname of the EC2 instance on which the Amazon Inspector Agent is installed.
  , ipv4Address :: Core.Maybe Types.Ipv4Address
    -- ^ The IP address of the EC2 instance on which the Amazon Inspector Agent is installed.
  , kernelVersion :: Core.Maybe Types.KernelVersion
    -- ^ The kernel version of the operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
  , operatingSystem :: Core.Maybe Types.OperatingSystem
    -- ^ The operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AgentPreview' value with any optional fields omitted.
mkAgentPreview
    :: Types.AgentId -- ^ 'agentId'
    -> AgentPreview
mkAgentPreview agentId
  = AgentPreview'{agentId, agentHealth = Core.Nothing,
                  agentVersion = Core.Nothing, autoScalingGroup = Core.Nothing,
                  hostname = Core.Nothing, ipv4Address = Core.Nothing,
                  kernelVersion = Core.Nothing, operatingSystem = Core.Nothing}

-- | The ID of the EC2 instance where the agent is installed.
--
-- /Note:/ Consider using 'agentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAgentId :: Lens.Lens' AgentPreview Types.AgentId
apAgentId = Lens.field @"agentId"
{-# INLINEABLE apAgentId #-}
{-# DEPRECATED agentId "Use generic-lens or generic-optics with 'agentId' instead"  #-}

-- | The health status of the Amazon Inspector Agent.
--
-- /Note:/ Consider using 'agentHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAgentHealth :: Lens.Lens' AgentPreview (Core.Maybe Types.AgentHealth)
apAgentHealth = Lens.field @"agentHealth"
{-# INLINEABLE apAgentHealth #-}
{-# DEPRECATED agentHealth "Use generic-lens or generic-optics with 'agentHealth' instead"  #-}

-- | The version of the Amazon Inspector Agent.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAgentVersion :: Lens.Lens' AgentPreview (Core.Maybe Types.AgentVersion)
apAgentVersion = Lens.field @"agentVersion"
{-# INLINEABLE apAgentVersion #-}
{-# DEPRECATED agentVersion "Use generic-lens or generic-optics with 'agentVersion' instead"  #-}

-- | The Auto Scaling group for the EC2 instance where the agent is installed.
--
-- /Note:/ Consider using 'autoScalingGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAutoScalingGroup :: Lens.Lens' AgentPreview (Core.Maybe Types.AutoScalingGroup)
apAutoScalingGroup = Lens.field @"autoScalingGroup"
{-# INLINEABLE apAutoScalingGroup #-}
{-# DEPRECATED autoScalingGroup "Use generic-lens or generic-optics with 'autoScalingGroup' instead"  #-}

-- | The hostname of the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apHostname :: Lens.Lens' AgentPreview (Core.Maybe Types.Hostname)
apHostname = Lens.field @"hostname"
{-# INLINEABLE apHostname #-}
{-# DEPRECATED hostname "Use generic-lens or generic-optics with 'hostname' instead"  #-}

-- | The IP address of the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- /Note:/ Consider using 'ipv4Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apIpv4Address :: Lens.Lens' AgentPreview (Core.Maybe Types.Ipv4Address)
apIpv4Address = Lens.field @"ipv4Address"
{-# INLINEABLE apIpv4Address #-}
{-# DEPRECATED ipv4Address "Use generic-lens or generic-optics with 'ipv4Address' instead"  #-}

-- | The kernel version of the operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- /Note:/ Consider using 'kernelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apKernelVersion :: Lens.Lens' AgentPreview (Core.Maybe Types.KernelVersion)
apKernelVersion = Lens.field @"kernelVersion"
{-# INLINEABLE apKernelVersion #-}
{-# DEPRECATED kernelVersion "Use generic-lens or generic-optics with 'kernelVersion' instead"  #-}

-- | The operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apOperatingSystem :: Lens.Lens' AgentPreview (Core.Maybe Types.OperatingSystem)
apOperatingSystem = Lens.field @"operatingSystem"
{-# INLINEABLE apOperatingSystem #-}
{-# DEPRECATED operatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead"  #-}

instance Core.FromJSON AgentPreview where
        parseJSON
          = Core.withObject "AgentPreview" Core.$
              \ x ->
                AgentPreview' Core.<$>
                  (x Core..: "agentId") Core.<*> x Core..:? "agentHealth" Core.<*>
                    x Core..:? "agentVersion"
                    Core.<*> x Core..:? "autoScalingGroup"
                    Core.<*> x Core..:? "hostname"
                    Core.<*> x Core..:? "ipv4Address"
                    Core.<*> x Core..:? "kernelVersion"
                    Core.<*> x Core..:? "operatingSystem"
