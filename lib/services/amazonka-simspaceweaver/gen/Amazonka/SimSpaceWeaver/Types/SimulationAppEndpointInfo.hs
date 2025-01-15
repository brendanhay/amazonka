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
-- Module      : Amazonka.SimSpaceWeaver.Types.SimulationAppEndpointInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.SimulationAppEndpointInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SimSpaceWeaver.Types.SimulationAppPortMapping

-- | Information about the network endpoint that you can use to connect to
-- your custom or service app.
--
-- /See:/ 'newSimulationAppEndpointInfo' smart constructor.
data SimulationAppEndpointInfo = SimulationAppEndpointInfo'
  { -- | The IP address of the app. SimSpace Weaver dynamically assigns this IP
    -- address when the app starts.
    address :: Prelude.Maybe Prelude.Text,
    -- | The inbound TCP\/UDP port numbers of the app. The combination of an IP
    -- address and a port number form a network endpoint.
    ingressPortMappings :: Prelude.Maybe [SimulationAppPortMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulationAppEndpointInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'simulationAppEndpointInfo_address' - The IP address of the app. SimSpace Weaver dynamically assigns this IP
-- address when the app starts.
--
-- 'ingressPortMappings', 'simulationAppEndpointInfo_ingressPortMappings' - The inbound TCP\/UDP port numbers of the app. The combination of an IP
-- address and a port number form a network endpoint.
newSimulationAppEndpointInfo ::
  SimulationAppEndpointInfo
newSimulationAppEndpointInfo =
  SimulationAppEndpointInfo'
    { address =
        Prelude.Nothing,
      ingressPortMappings = Prelude.Nothing
    }

-- | The IP address of the app. SimSpace Weaver dynamically assigns this IP
-- address when the app starts.
simulationAppEndpointInfo_address :: Lens.Lens' SimulationAppEndpointInfo (Prelude.Maybe Prelude.Text)
simulationAppEndpointInfo_address = Lens.lens (\SimulationAppEndpointInfo' {address} -> address) (\s@SimulationAppEndpointInfo' {} a -> s {address = a} :: SimulationAppEndpointInfo)

-- | The inbound TCP\/UDP port numbers of the app. The combination of an IP
-- address and a port number form a network endpoint.
simulationAppEndpointInfo_ingressPortMappings :: Lens.Lens' SimulationAppEndpointInfo (Prelude.Maybe [SimulationAppPortMapping])
simulationAppEndpointInfo_ingressPortMappings = Lens.lens (\SimulationAppEndpointInfo' {ingressPortMappings} -> ingressPortMappings) (\s@SimulationAppEndpointInfo' {} a -> s {ingressPortMappings = a} :: SimulationAppEndpointInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SimulationAppEndpointInfo where
  parseJSON =
    Data.withObject
      "SimulationAppEndpointInfo"
      ( \x ->
          SimulationAppEndpointInfo'
            Prelude.<$> (x Data..:? "Address")
            Prelude.<*> ( x
                            Data..:? "IngressPortMappings"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SimulationAppEndpointInfo where
  hashWithSalt _salt SimulationAppEndpointInfo' {..} =
    _salt
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` ingressPortMappings

instance Prelude.NFData SimulationAppEndpointInfo where
  rnf SimulationAppEndpointInfo' {..} =
    Prelude.rnf address `Prelude.seq`
      Prelude.rnf ingressPortMappings
