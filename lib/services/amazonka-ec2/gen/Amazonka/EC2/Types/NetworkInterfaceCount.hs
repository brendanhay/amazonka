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
-- Module      : Amazonka.EC2.Types.NetworkInterfaceCount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInterfaceCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum number of network interfaces.
--
-- /See:/ 'newNetworkInterfaceCount' smart constructor.
data NetworkInterfaceCount = NetworkInterfaceCount'
  { -- | The maximum number of network interfaces. If this parameter is not
    -- specified, there is no maximum limit.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum number of network interfaces. If this parameter is not
    -- specified, there is no minimum limit.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterfaceCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'networkInterfaceCount_max' - The maximum number of network interfaces. If this parameter is not
-- specified, there is no maximum limit.
--
-- 'min', 'networkInterfaceCount_min' - The minimum number of network interfaces. If this parameter is not
-- specified, there is no minimum limit.
newNetworkInterfaceCount ::
  NetworkInterfaceCount
newNetworkInterfaceCount =
  NetworkInterfaceCount'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum number of network interfaces. If this parameter is not
-- specified, there is no maximum limit.
networkInterfaceCount_max :: Lens.Lens' NetworkInterfaceCount (Prelude.Maybe Prelude.Int)
networkInterfaceCount_max = Lens.lens (\NetworkInterfaceCount' {max} -> max) (\s@NetworkInterfaceCount' {} a -> s {max = a} :: NetworkInterfaceCount)

-- | The minimum number of network interfaces. If this parameter is not
-- specified, there is no minimum limit.
networkInterfaceCount_min :: Lens.Lens' NetworkInterfaceCount (Prelude.Maybe Prelude.Int)
networkInterfaceCount_min = Lens.lens (\NetworkInterfaceCount' {min} -> min) (\s@NetworkInterfaceCount' {} a -> s {min = a} :: NetworkInterfaceCount)

instance Data.FromXML NetworkInterfaceCount where
  parseXML x =
    NetworkInterfaceCount'
      Prelude.<$> (x Data..@? "max") Prelude.<*> (x Data..@? "min")

instance Prelude.Hashable NetworkInterfaceCount where
  hashWithSalt _salt NetworkInterfaceCount' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData NetworkInterfaceCount where
  rnf NetworkInterfaceCount' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery NetworkInterfaceCount where
  toQuery NetworkInterfaceCount' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
