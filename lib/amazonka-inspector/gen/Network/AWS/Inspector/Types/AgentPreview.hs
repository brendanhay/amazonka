-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AgentPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AgentPreview
  ( AgentPreview (..),

    -- * Smart constructor
    mkAgentPreview,

    -- * Lenses
    apHostname,
    apAutoScalingGroup,
    apOperatingSystem,
    apAgentVersion,
    apKernelVersion,
    apAgentHealth,
    apIpv4Address,
    apAgentId,
  )
where

import Network.AWS.Inspector.Types.AgentHealth
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used as a response element in the 'PreviewAgents' action.
--
-- /See:/ 'mkAgentPreview' smart constructor.
data AgentPreview = AgentPreview'
  { hostname :: Lude.Maybe Lude.Text,
    autoScalingGroup :: Lude.Maybe Lude.Text,
    operatingSystem :: Lude.Maybe Lude.Text,
    agentVersion :: Lude.Maybe Lude.Text,
    kernelVersion :: Lude.Maybe Lude.Text,
    agentHealth :: Lude.Maybe AgentHealth,
    ipv4Address :: Lude.Maybe Lude.Text,
    agentId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AgentPreview' with the minimum fields required to make a request.
--
-- * 'agentHealth' - The health status of the Amazon Inspector Agent.
-- * 'agentId' - The ID of the EC2 instance where the agent is installed.
-- * 'agentVersion' - The version of the Amazon Inspector Agent.
-- * 'autoScalingGroup' - The Auto Scaling group for the EC2 instance where the agent is installed.
-- * 'hostname' - The hostname of the EC2 instance on which the Amazon Inspector Agent is installed.
-- * 'ipv4Address' - The IP address of the EC2 instance on which the Amazon Inspector Agent is installed.
-- * 'kernelVersion' - The kernel version of the operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
-- * 'operatingSystem' - The operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
mkAgentPreview ::
  -- | 'agentId'
  Lude.Text ->
  AgentPreview
mkAgentPreview pAgentId_ =
  AgentPreview'
    { hostname = Lude.Nothing,
      autoScalingGroup = Lude.Nothing,
      operatingSystem = Lude.Nothing,
      agentVersion = Lude.Nothing,
      kernelVersion = Lude.Nothing,
      agentHealth = Lude.Nothing,
      ipv4Address = Lude.Nothing,
      agentId = pAgentId_
    }

-- | The hostname of the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apHostname :: Lens.Lens' AgentPreview (Lude.Maybe Lude.Text)
apHostname = Lens.lens (hostname :: AgentPreview -> Lude.Maybe Lude.Text) (\s a -> s {hostname = a} :: AgentPreview)
{-# DEPRECATED apHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | The Auto Scaling group for the EC2 instance where the agent is installed.
--
-- /Note:/ Consider using 'autoScalingGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAutoScalingGroup :: Lens.Lens' AgentPreview (Lude.Maybe Lude.Text)
apAutoScalingGroup = Lens.lens (autoScalingGroup :: AgentPreview -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroup = a} :: AgentPreview)
{-# DEPRECATED apAutoScalingGroup "Use generic-lens or generic-optics with 'autoScalingGroup' instead." #-}

-- | The operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apOperatingSystem :: Lens.Lens' AgentPreview (Lude.Maybe Lude.Text)
apOperatingSystem = Lens.lens (operatingSystem :: AgentPreview -> Lude.Maybe Lude.Text) (\s a -> s {operatingSystem = a} :: AgentPreview)
{-# DEPRECATED apOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | The version of the Amazon Inspector Agent.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAgentVersion :: Lens.Lens' AgentPreview (Lude.Maybe Lude.Text)
apAgentVersion = Lens.lens (agentVersion :: AgentPreview -> Lude.Maybe Lude.Text) (\s a -> s {agentVersion = a} :: AgentPreview)
{-# DEPRECATED apAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | The kernel version of the operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- /Note:/ Consider using 'kernelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apKernelVersion :: Lens.Lens' AgentPreview (Lude.Maybe Lude.Text)
apKernelVersion = Lens.lens (kernelVersion :: AgentPreview -> Lude.Maybe Lude.Text) (\s a -> s {kernelVersion = a} :: AgentPreview)
{-# DEPRECATED apKernelVersion "Use generic-lens or generic-optics with 'kernelVersion' instead." #-}

-- | The health status of the Amazon Inspector Agent.
--
-- /Note:/ Consider using 'agentHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAgentHealth :: Lens.Lens' AgentPreview (Lude.Maybe AgentHealth)
apAgentHealth = Lens.lens (agentHealth :: AgentPreview -> Lude.Maybe AgentHealth) (\s a -> s {agentHealth = a} :: AgentPreview)
{-# DEPRECATED apAgentHealth "Use generic-lens or generic-optics with 'agentHealth' instead." #-}

-- | The IP address of the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- /Note:/ Consider using 'ipv4Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apIpv4Address :: Lens.Lens' AgentPreview (Lude.Maybe Lude.Text)
apIpv4Address = Lens.lens (ipv4Address :: AgentPreview -> Lude.Maybe Lude.Text) (\s a -> s {ipv4Address = a} :: AgentPreview)
{-# DEPRECATED apIpv4Address "Use generic-lens or generic-optics with 'ipv4Address' instead." #-}

-- | The ID of the EC2 instance where the agent is installed.
--
-- /Note:/ Consider using 'agentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAgentId :: Lens.Lens' AgentPreview Lude.Text
apAgentId = Lens.lens (agentId :: AgentPreview -> Lude.Text) (\s a -> s {agentId = a} :: AgentPreview)
{-# DEPRECATED apAgentId "Use generic-lens or generic-optics with 'agentId' instead." #-}

instance Lude.FromJSON AgentPreview where
  parseJSON =
    Lude.withObject
      "AgentPreview"
      ( \x ->
          AgentPreview'
            Lude.<$> (x Lude..:? "hostname")
            Lude.<*> (x Lude..:? "autoScalingGroup")
            Lude.<*> (x Lude..:? "operatingSystem")
            Lude.<*> (x Lude..:? "agentVersion")
            Lude.<*> (x Lude..:? "kernelVersion")
            Lude.<*> (x Lude..:? "agentHealth")
            Lude.<*> (x Lude..:? "ipv4Address")
            Lude.<*> (x Lude..: "agentId")
      )
