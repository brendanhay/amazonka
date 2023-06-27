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
-- Module      : Amazonka.GroundStation.Types.RangedConnectionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.RangedConnectionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.RangedSocketAddress
import qualified Amazonka.Prelude as Prelude

-- | Ingress address of AgentEndpoint with a port range and an optional mtu.
--
-- /See:/ 'newRangedConnectionDetails' smart constructor.
data RangedConnectionDetails = RangedConnectionDetails'
  { -- | Maximum transmission unit (MTU) size in bytes of a dataflow endpoint.
    mtu :: Prelude.Maybe Prelude.Natural,
    -- | A ranged socket address.
    socketAddress :: RangedSocketAddress
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RangedConnectionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mtu', 'rangedConnectionDetails_mtu' - Maximum transmission unit (MTU) size in bytes of a dataflow endpoint.
--
-- 'socketAddress', 'rangedConnectionDetails_socketAddress' - A ranged socket address.
newRangedConnectionDetails ::
  -- | 'socketAddress'
  RangedSocketAddress ->
  RangedConnectionDetails
newRangedConnectionDetails pSocketAddress_ =
  RangedConnectionDetails'
    { mtu = Prelude.Nothing,
      socketAddress = pSocketAddress_
    }

-- | Maximum transmission unit (MTU) size in bytes of a dataflow endpoint.
rangedConnectionDetails_mtu :: Lens.Lens' RangedConnectionDetails (Prelude.Maybe Prelude.Natural)
rangedConnectionDetails_mtu = Lens.lens (\RangedConnectionDetails' {mtu} -> mtu) (\s@RangedConnectionDetails' {} a -> s {mtu = a} :: RangedConnectionDetails)

-- | A ranged socket address.
rangedConnectionDetails_socketAddress :: Lens.Lens' RangedConnectionDetails RangedSocketAddress
rangedConnectionDetails_socketAddress = Lens.lens (\RangedConnectionDetails' {socketAddress} -> socketAddress) (\s@RangedConnectionDetails' {} a -> s {socketAddress = a} :: RangedConnectionDetails)

instance Data.FromJSON RangedConnectionDetails where
  parseJSON =
    Data.withObject
      "RangedConnectionDetails"
      ( \x ->
          RangedConnectionDetails'
            Prelude.<$> (x Data..:? "mtu")
            Prelude.<*> (x Data..: "socketAddress")
      )

instance Prelude.Hashable RangedConnectionDetails where
  hashWithSalt _salt RangedConnectionDetails' {..} =
    _salt
      `Prelude.hashWithSalt` mtu
      `Prelude.hashWithSalt` socketAddress

instance Prelude.NFData RangedConnectionDetails where
  rnf RangedConnectionDetails' {..} =
    Prelude.rnf mtu
      `Prelude.seq` Prelude.rnf socketAddress

instance Data.ToJSON RangedConnectionDetails where
  toJSON RangedConnectionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("mtu" Data..=) Prelude.<$> mtu,
            Prelude.Just
              ("socketAddress" Data..= socketAddress)
          ]
      )
