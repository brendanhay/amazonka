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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.NetworkPathComponentDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.PortRange

-- | Information about the destination of the next component in the network
-- path.
--
-- /See:/ 'newNetworkPathComponentDetails' smart constructor.
data NetworkPathComponentDetails = NetworkPathComponentDetails'
  { -- | A list of port ranges for the destination.
    portRanges :: Prelude.Maybe [PortRange],
    -- | The IP addresses of the destination.
    address :: Prelude.Maybe [Prelude.Text]
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
-- 'portRanges', 'networkPathComponentDetails_portRanges' - A list of port ranges for the destination.
--
-- 'address', 'networkPathComponentDetails_address' - The IP addresses of the destination.
newNetworkPathComponentDetails ::
  NetworkPathComponentDetails
newNetworkPathComponentDetails =
  NetworkPathComponentDetails'
    { portRanges =
        Prelude.Nothing,
      address = Prelude.Nothing
    }

-- | A list of port ranges for the destination.
networkPathComponentDetails_portRanges :: Lens.Lens' NetworkPathComponentDetails (Prelude.Maybe [PortRange])
networkPathComponentDetails_portRanges = Lens.lens (\NetworkPathComponentDetails' {portRanges} -> portRanges) (\s@NetworkPathComponentDetails' {} a -> s {portRanges = a} :: NetworkPathComponentDetails) Prelude.. Lens.mapping Lens.coerced

-- | The IP addresses of the destination.
networkPathComponentDetails_address :: Lens.Lens' NetworkPathComponentDetails (Prelude.Maybe [Prelude.Text])
networkPathComponentDetails_address = Lens.lens (\NetworkPathComponentDetails' {address} -> address) (\s@NetworkPathComponentDetails' {} a -> s {address = a} :: NetworkPathComponentDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON NetworkPathComponentDetails where
  parseJSON =
    Core.withObject
      "NetworkPathComponentDetails"
      ( \x ->
          NetworkPathComponentDetails'
            Prelude.<$> (x Core..:? "PortRanges" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Address" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable NetworkPathComponentDetails where
  hashWithSalt salt' NetworkPathComponentDetails' {..} =
    salt' `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` portRanges

instance Prelude.NFData NetworkPathComponentDetails where
  rnf NetworkPathComponentDetails' {..} =
    Prelude.rnf portRanges
      `Prelude.seq` Prelude.rnf address

instance Core.ToJSON NetworkPathComponentDetails where
  toJSON NetworkPathComponentDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PortRanges" Core..=) Prelude.<$> portRanges,
            ("Address" Core..=) Prelude.<$> address
          ]
      )
