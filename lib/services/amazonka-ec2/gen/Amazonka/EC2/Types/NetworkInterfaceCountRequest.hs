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
-- Module      : Amazonka.EC2.Types.NetworkInterfaceCountRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInterfaceCountRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum number of network interfaces.
--
-- /See:/ 'newNetworkInterfaceCountRequest' smart constructor.
data NetworkInterfaceCountRequest = NetworkInterfaceCountRequest'
  { -- | The maximum number of network interfaces. To specify no maximum limit,
    -- omit this parameter.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum number of network interfaces. To specify no minimum limit,
    -- omit this parameter.
    min :: Prelude.Maybe Prelude.Int
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
-- 'max', 'networkInterfaceCountRequest_max' - The maximum number of network interfaces. To specify no maximum limit,
-- omit this parameter.
--
-- 'min', 'networkInterfaceCountRequest_min' - The minimum number of network interfaces. To specify no minimum limit,
-- omit this parameter.
newNetworkInterfaceCountRequest ::
  NetworkInterfaceCountRequest
newNetworkInterfaceCountRequest =
  NetworkInterfaceCountRequest'
    { max =
        Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum number of network interfaces. To specify no maximum limit,
-- omit this parameter.
networkInterfaceCountRequest_max :: Lens.Lens' NetworkInterfaceCountRequest (Prelude.Maybe Prelude.Int)
networkInterfaceCountRequest_max = Lens.lens (\NetworkInterfaceCountRequest' {max} -> max) (\s@NetworkInterfaceCountRequest' {} a -> s {max = a} :: NetworkInterfaceCountRequest)

-- | The minimum number of network interfaces. To specify no minimum limit,
-- omit this parameter.
networkInterfaceCountRequest_min :: Lens.Lens' NetworkInterfaceCountRequest (Prelude.Maybe Prelude.Int)
networkInterfaceCountRequest_min = Lens.lens (\NetworkInterfaceCountRequest' {min} -> min) (\s@NetworkInterfaceCountRequest' {} a -> s {min = a} :: NetworkInterfaceCountRequest)

instance
  Prelude.Hashable
    NetworkInterfaceCountRequest
  where
  hashWithSalt _salt NetworkInterfaceCountRequest' {..} =
    _salt
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData NetworkInterfaceCountRequest where
  rnf NetworkInterfaceCountRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery NetworkInterfaceCountRequest where
  toQuery NetworkInterfaceCountRequest' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
