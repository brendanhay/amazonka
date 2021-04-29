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
-- Module      : Network.AWS.Discovery.Types.AgentNetworkInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentNetworkInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Network details about the host where the agent\/connector resides.
--
-- /See:/ 'newAgentNetworkInfo' smart constructor.
data AgentNetworkInfo = AgentNetworkInfo'
  { -- | The MAC address for the host where the agent\/connector resides.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The IP address for the host where the agent\/connector resides.
    ipAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { macAddress = Prelude.Nothing,
      ipAddress = Prelude.Nothing
    }

-- | The MAC address for the host where the agent\/connector resides.
agentNetworkInfo_macAddress :: Lens.Lens' AgentNetworkInfo (Prelude.Maybe Prelude.Text)
agentNetworkInfo_macAddress = Lens.lens (\AgentNetworkInfo' {macAddress} -> macAddress) (\s@AgentNetworkInfo' {} a -> s {macAddress = a} :: AgentNetworkInfo)

-- | The IP address for the host where the agent\/connector resides.
agentNetworkInfo_ipAddress :: Lens.Lens' AgentNetworkInfo (Prelude.Maybe Prelude.Text)
agentNetworkInfo_ipAddress = Lens.lens (\AgentNetworkInfo' {ipAddress} -> ipAddress) (\s@AgentNetworkInfo' {} a -> s {ipAddress = a} :: AgentNetworkInfo)

instance Prelude.FromJSON AgentNetworkInfo where
  parseJSON =
    Prelude.withObject
      "AgentNetworkInfo"
      ( \x ->
          AgentNetworkInfo'
            Prelude.<$> (x Prelude..:? "macAddress")
            Prelude.<*> (x Prelude..:? "ipAddress")
      )

instance Prelude.Hashable AgentNetworkInfo

instance Prelude.NFData AgentNetworkInfo
