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
-- Module      : Amazonka.SecurityHub.Types.NetworkPathComponentDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.NetworkPathComponentDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.PortRange

-- | Information about the destination of the next component in the network
-- path.
--
-- /See:/ 'newNetworkPathComponentDetails' smart constructor.
data NetworkPathComponentDetails = NetworkPathComponentDetails'
  { -- | The IP addresses of the destination.
    address :: Prelude.Maybe [Prelude.Text],
    -- | A list of port ranges for the destination.
    portRanges :: Prelude.Maybe [PortRange]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkPathComponentDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'networkPathComponentDetails_address' - The IP addresses of the destination.
--
-- 'portRanges', 'networkPathComponentDetails_portRanges' - A list of port ranges for the destination.
newNetworkPathComponentDetails ::
  NetworkPathComponentDetails
newNetworkPathComponentDetails =
  NetworkPathComponentDetails'
    { address =
        Prelude.Nothing,
      portRanges = Prelude.Nothing
    }

-- | The IP addresses of the destination.
networkPathComponentDetails_address :: Lens.Lens' NetworkPathComponentDetails (Prelude.Maybe [Prelude.Text])
networkPathComponentDetails_address = Lens.lens (\NetworkPathComponentDetails' {address} -> address) (\s@NetworkPathComponentDetails' {} a -> s {address = a} :: NetworkPathComponentDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of port ranges for the destination.
networkPathComponentDetails_portRanges :: Lens.Lens' NetworkPathComponentDetails (Prelude.Maybe [PortRange])
networkPathComponentDetails_portRanges = Lens.lens (\NetworkPathComponentDetails' {portRanges} -> portRanges) (\s@NetworkPathComponentDetails' {} a -> s {portRanges = a} :: NetworkPathComponentDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON NetworkPathComponentDetails where
  parseJSON =
    Data.withObject
      "NetworkPathComponentDetails"
      ( \x ->
          NetworkPathComponentDetails'
            Prelude.<$> (x Data..:? "Address" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PortRanges" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable NetworkPathComponentDetails where
  hashWithSalt _salt NetworkPathComponentDetails' {..} =
    _salt `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` portRanges

instance Prelude.NFData NetworkPathComponentDetails where
  rnf NetworkPathComponentDetails' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf portRanges

instance Data.ToJSON NetworkPathComponentDetails where
  toJSON NetworkPathComponentDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Address" Data..=) Prelude.<$> address,
            ("PortRanges" Data..=) Prelude.<$> portRanges
          ]
      )
