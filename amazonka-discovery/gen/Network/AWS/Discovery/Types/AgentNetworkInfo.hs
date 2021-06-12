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
-- Module      : Network.AWS.Discovery.Types.AgentNetworkInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentNetworkInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Network details about the host where the agent\/connector resides.
--
-- /See:/ 'newAgentNetworkInfo' smart constructor.
data AgentNetworkInfo = AgentNetworkInfo'
  { -- | The MAC address for the host where the agent\/connector resides.
    macAddress :: Core.Maybe Core.Text,
    -- | The IP address for the host where the agent\/connector resides.
    ipAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AgentNetworkInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'macAddress', 'agentNetworkInfo_macAddress' - The MAC address for the host where the agent\/connector resides.
--
-- 'ipAddress', 'agentNetworkInfo_ipAddress' - The IP address for the host where the agent\/connector resides.
newAgentNetworkInfo ::
  AgentNetworkInfo
newAgentNetworkInfo =
  AgentNetworkInfo'
    { macAddress = Core.Nothing,
      ipAddress = Core.Nothing
    }

-- | The MAC address for the host where the agent\/connector resides.
agentNetworkInfo_macAddress :: Lens.Lens' AgentNetworkInfo (Core.Maybe Core.Text)
agentNetworkInfo_macAddress = Lens.lens (\AgentNetworkInfo' {macAddress} -> macAddress) (\s@AgentNetworkInfo' {} a -> s {macAddress = a} :: AgentNetworkInfo)

-- | The IP address for the host where the agent\/connector resides.
agentNetworkInfo_ipAddress :: Lens.Lens' AgentNetworkInfo (Core.Maybe Core.Text)
agentNetworkInfo_ipAddress = Lens.lens (\AgentNetworkInfo' {ipAddress} -> ipAddress) (\s@AgentNetworkInfo' {} a -> s {ipAddress = a} :: AgentNetworkInfo)

instance Core.FromJSON AgentNetworkInfo where
  parseJSON =
    Core.withObject
      "AgentNetworkInfo"
      ( \x ->
          AgentNetworkInfo'
            Core.<$> (x Core..:? "macAddress")
            Core.<*> (x Core..:? "ipAddress")
      )

instance Core.Hashable AgentNetworkInfo

instance Core.NFData AgentNetworkInfo
