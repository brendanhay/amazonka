{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.AgentNetworkInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentNetworkInfo
  ( AgentNetworkInfo (..),

    -- * Smart constructor
    mkAgentNetworkInfo,

    -- * Lenses
    aniIpAddress,
    aniMacAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Network details about the host where the agent/connector resides.
--
-- /See:/ 'mkAgentNetworkInfo' smart constructor.
data AgentNetworkInfo = AgentNetworkInfo'
  { ipAddress ::
      Lude.Maybe Lude.Text,
    macAddress :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AgentNetworkInfo' with the minimum fields required to make a request.
--
-- * 'ipAddress' - The IP address for the host where the agent/connector resides.
-- * 'macAddress' - The MAC address for the host where the agent/connector resides.
mkAgentNetworkInfo ::
  AgentNetworkInfo
mkAgentNetworkInfo =
  AgentNetworkInfo'
    { ipAddress = Lude.Nothing,
      macAddress = Lude.Nothing
    }

-- | The IP address for the host where the agent/connector resides.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aniIpAddress :: Lens.Lens' AgentNetworkInfo (Lude.Maybe Lude.Text)
aniIpAddress = Lens.lens (ipAddress :: AgentNetworkInfo -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: AgentNetworkInfo)
{-# DEPRECATED aniIpAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The MAC address for the host where the agent/connector resides.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aniMacAddress :: Lens.Lens' AgentNetworkInfo (Lude.Maybe Lude.Text)
aniMacAddress = Lens.lens (macAddress :: AgentNetworkInfo -> Lude.Maybe Lude.Text) (\s a -> s {macAddress = a} :: AgentNetworkInfo)
{-# DEPRECATED aniMacAddress "Use generic-lens or generic-optics with 'macAddress' instead." #-}

instance Lude.FromJSON AgentNetworkInfo where
  parseJSON =
    Lude.withObject
      "AgentNetworkInfo"
      ( \x ->
          AgentNetworkInfo'
            Lude.<$> (x Lude..:? "ipAddress") Lude.<*> (x Lude..:? "macAddress")
      )
