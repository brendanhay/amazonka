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
-- Module      : Amazonka.GroundStation.Types.DataflowEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.DataflowEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.EndpointStatus
import Amazonka.GroundStation.Types.SocketAddress
import qualified Amazonka.Prelude as Prelude

-- | Information about a dataflow endpoint.
--
-- /See:/ 'newDataflowEndpoint' smart constructor.
data DataflowEndpoint = DataflowEndpoint'
  { -- | Socket address of a dataflow endpoint.
    address :: Prelude.Maybe SocketAddress,
    -- | Maximum transmission unit (MTU) size in bytes of a dataflow endpoint.
    mtu :: Prelude.Maybe Prelude.Natural,
    -- | Name of a dataflow endpoint.
    name :: Prelude.Maybe Prelude.Text,
    -- | Status of a dataflow endpoint.
    status :: Prelude.Maybe EndpointStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataflowEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'dataflowEndpoint_address' - Socket address of a dataflow endpoint.
--
-- 'mtu', 'dataflowEndpoint_mtu' - Maximum transmission unit (MTU) size in bytes of a dataflow endpoint.
--
-- 'name', 'dataflowEndpoint_name' - Name of a dataflow endpoint.
--
-- 'status', 'dataflowEndpoint_status' - Status of a dataflow endpoint.
newDataflowEndpoint ::
  DataflowEndpoint
newDataflowEndpoint =
  DataflowEndpoint'
    { address = Prelude.Nothing,
      mtu = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Socket address of a dataflow endpoint.
dataflowEndpoint_address :: Lens.Lens' DataflowEndpoint (Prelude.Maybe SocketAddress)
dataflowEndpoint_address = Lens.lens (\DataflowEndpoint' {address} -> address) (\s@DataflowEndpoint' {} a -> s {address = a} :: DataflowEndpoint)

-- | Maximum transmission unit (MTU) size in bytes of a dataflow endpoint.
dataflowEndpoint_mtu :: Lens.Lens' DataflowEndpoint (Prelude.Maybe Prelude.Natural)
dataflowEndpoint_mtu = Lens.lens (\DataflowEndpoint' {mtu} -> mtu) (\s@DataflowEndpoint' {} a -> s {mtu = a} :: DataflowEndpoint)

-- | Name of a dataflow endpoint.
dataflowEndpoint_name :: Lens.Lens' DataflowEndpoint (Prelude.Maybe Prelude.Text)
dataflowEndpoint_name = Lens.lens (\DataflowEndpoint' {name} -> name) (\s@DataflowEndpoint' {} a -> s {name = a} :: DataflowEndpoint)

-- | Status of a dataflow endpoint.
dataflowEndpoint_status :: Lens.Lens' DataflowEndpoint (Prelude.Maybe EndpointStatus)
dataflowEndpoint_status = Lens.lens (\DataflowEndpoint' {status} -> status) (\s@DataflowEndpoint' {} a -> s {status = a} :: DataflowEndpoint)

instance Data.FromJSON DataflowEndpoint where
  parseJSON =
    Data.withObject
      "DataflowEndpoint"
      ( \x ->
          DataflowEndpoint'
            Prelude.<$> (x Data..:? "address")
            Prelude.<*> (x Data..:? "mtu")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable DataflowEndpoint where
  hashWithSalt _salt DataflowEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` mtu
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData DataflowEndpoint where
  rnf DataflowEndpoint' {..} =
    Prelude.rnf address `Prelude.seq`
      Prelude.rnf mtu `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf status

instance Data.ToJSON DataflowEndpoint where
  toJSON DataflowEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("address" Data..=) Prelude.<$> address,
            ("mtu" Data..=) Prelude.<$> mtu,
            ("name" Data..=) Prelude.<$> name,
            ("status" Data..=) Prelude.<$> status
          ]
      )
