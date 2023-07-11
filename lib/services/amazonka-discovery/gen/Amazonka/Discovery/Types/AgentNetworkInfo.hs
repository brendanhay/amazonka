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
-- Module      : Amazonka.Discovery.Types.AgentNetworkInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.AgentNetworkInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Network details about the host where the agent\/connector resides.
--
-- /See:/ 'newAgentNetworkInfo' smart constructor.
data AgentNetworkInfo = AgentNetworkInfo'
  { -- | The IP address for the host where the agent\/connector resides.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The MAC address for the host where the agent\/connector resides.
    macAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentNetworkInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddress', 'agentNetworkInfo_ipAddress' - The IP address for the host where the agent\/connector resides.
--
-- 'macAddress', 'agentNetworkInfo_macAddress' - The MAC address for the host where the agent\/connector resides.
newAgentNetworkInfo ::
  AgentNetworkInfo
newAgentNetworkInfo =
  AgentNetworkInfo'
    { ipAddress = Prelude.Nothing,
      macAddress = Prelude.Nothing
    }

-- | The IP address for the host where the agent\/connector resides.
agentNetworkInfo_ipAddress :: Lens.Lens' AgentNetworkInfo (Prelude.Maybe Prelude.Text)
agentNetworkInfo_ipAddress = Lens.lens (\AgentNetworkInfo' {ipAddress} -> ipAddress) (\s@AgentNetworkInfo' {} a -> s {ipAddress = a} :: AgentNetworkInfo)

-- | The MAC address for the host where the agent\/connector resides.
agentNetworkInfo_macAddress :: Lens.Lens' AgentNetworkInfo (Prelude.Maybe Prelude.Text)
agentNetworkInfo_macAddress = Lens.lens (\AgentNetworkInfo' {macAddress} -> macAddress) (\s@AgentNetworkInfo' {} a -> s {macAddress = a} :: AgentNetworkInfo)

instance Data.FromJSON AgentNetworkInfo where
  parseJSON =
    Data.withObject
      "AgentNetworkInfo"
      ( \x ->
          AgentNetworkInfo'
            Prelude.<$> (x Data..:? "ipAddress")
            Prelude.<*> (x Data..:? "macAddress")
      )

instance Prelude.Hashable AgentNetworkInfo where
  hashWithSalt _salt AgentNetworkInfo' {..} =
    _salt
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` macAddress

instance Prelude.NFData AgentNetworkInfo where
  rnf AgentNetworkInfo' {..} =
    Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf macAddress
