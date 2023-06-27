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
-- Module      : Amazonka.InternetMonitor.Types.NetworkImpairment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.NetworkImpairment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types.Network
import Amazonka.InternetMonitor.Types.TriangulationEventType
import qualified Amazonka.Prelude as Prelude

-- | Information about the network impairment for a specific network measured
-- by Amazon CloudWatch Internet Monitor.
--
-- /See:/ 'newNetworkImpairment' smart constructor.
data NetworkImpairment = NetworkImpairment'
  { -- | The networks that could be impacted by a network impairment event.
    networks :: [Network],
    -- | The combination of the Autonomous System Number (ASN) of the network and
    -- the name of the network.
    asPath :: [Network],
    -- | Type of network impairment.
    networkEventType :: TriangulationEventType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkImpairment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networks', 'networkImpairment_networks' - The networks that could be impacted by a network impairment event.
--
-- 'asPath', 'networkImpairment_asPath' - The combination of the Autonomous System Number (ASN) of the network and
-- the name of the network.
--
-- 'networkEventType', 'networkImpairment_networkEventType' - Type of network impairment.
newNetworkImpairment ::
  -- | 'networkEventType'
  TriangulationEventType ->
  NetworkImpairment
newNetworkImpairment pNetworkEventType_ =
  NetworkImpairment'
    { networks = Prelude.mempty,
      asPath = Prelude.mempty,
      networkEventType = pNetworkEventType_
    }

-- | The networks that could be impacted by a network impairment event.
networkImpairment_networks :: Lens.Lens' NetworkImpairment [Network]
networkImpairment_networks = Lens.lens (\NetworkImpairment' {networks} -> networks) (\s@NetworkImpairment' {} a -> s {networks = a} :: NetworkImpairment) Prelude.. Lens.coerced

-- | The combination of the Autonomous System Number (ASN) of the network and
-- the name of the network.
networkImpairment_asPath :: Lens.Lens' NetworkImpairment [Network]
networkImpairment_asPath = Lens.lens (\NetworkImpairment' {asPath} -> asPath) (\s@NetworkImpairment' {} a -> s {asPath = a} :: NetworkImpairment) Prelude.. Lens.coerced

-- | Type of network impairment.
networkImpairment_networkEventType :: Lens.Lens' NetworkImpairment TriangulationEventType
networkImpairment_networkEventType = Lens.lens (\NetworkImpairment' {networkEventType} -> networkEventType) (\s@NetworkImpairment' {} a -> s {networkEventType = a} :: NetworkImpairment)

instance Data.FromJSON NetworkImpairment where
  parseJSON =
    Data.withObject
      "NetworkImpairment"
      ( \x ->
          NetworkImpairment'
            Prelude.<$> (x Data..:? "Networks" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AsPath" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "NetworkEventType")
      )

instance Prelude.Hashable NetworkImpairment where
  hashWithSalt _salt NetworkImpairment' {..} =
    _salt
      `Prelude.hashWithSalt` networks
      `Prelude.hashWithSalt` asPath
      `Prelude.hashWithSalt` networkEventType

instance Prelude.NFData NetworkImpairment where
  rnf NetworkImpairment' {..} =
    Prelude.rnf networks
      `Prelude.seq` Prelude.rnf asPath
      `Prelude.seq` Prelude.rnf networkEventType
