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
-- Module      : Amazonka.AutoScaling.Types.NetworkInterfaceCountRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.NetworkInterfaceCountRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the minimum and maximum for the @NetworkInterfaceCount@ object
-- when you specify InstanceRequirements for an Auto Scaling group.
--
-- /See:/ 'newNetworkInterfaceCountRequest' smart constructor.
data NetworkInterfaceCountRequest = NetworkInterfaceCountRequest'
  { -- | The maximum number of network interfaces.
    max :: Prelude.Maybe Prelude.Natural,
    -- | The minimum number of network interfaces.
    min :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterfaceCountRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'networkInterfaceCountRequest_max' - The maximum number of network interfaces.
--
-- 'min', 'networkInterfaceCountRequest_min' - The minimum number of network interfaces.
newNetworkInterfaceCountRequest ::
  NetworkInterfaceCountRequest
newNetworkInterfaceCountRequest =
  NetworkInterfaceCountRequest'
    { max =
        Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum number of network interfaces.
networkInterfaceCountRequest_max :: Lens.Lens' NetworkInterfaceCountRequest (Prelude.Maybe Prelude.Natural)
networkInterfaceCountRequest_max = Lens.lens (\NetworkInterfaceCountRequest' {max} -> max) (\s@NetworkInterfaceCountRequest' {} a -> s {max = a} :: NetworkInterfaceCountRequest)

-- | The minimum number of network interfaces.
networkInterfaceCountRequest_min :: Lens.Lens' NetworkInterfaceCountRequest (Prelude.Maybe Prelude.Natural)
networkInterfaceCountRequest_min = Lens.lens (\NetworkInterfaceCountRequest' {min} -> min) (\s@NetworkInterfaceCountRequest' {} a -> s {min = a} :: NetworkInterfaceCountRequest)

instance Data.FromXML NetworkInterfaceCountRequest where
  parseXML x =
    NetworkInterfaceCountRequest'
      Prelude.<$> (x Data..@? "Max") Prelude.<*> (x Data..@? "Min")

instance
  Prelude.Hashable
    NetworkInterfaceCountRequest
  where
  hashWithSalt _salt NetworkInterfaceCountRequest' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData NetworkInterfaceCountRequest where
  rnf NetworkInterfaceCountRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery NetworkInterfaceCountRequest where
  toQuery NetworkInterfaceCountRequest' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
