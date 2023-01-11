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
-- Module      : Amazonka.EC2.Types.NetworkBandwidthGbps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkBandwidthGbps where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum amount of network bandwidth, in gigabits per
-- second (Gbps).
--
-- Setting the minimum bandwidth does not guarantee that your instance will
-- achieve the minimum bandwidth. Amazon EC2 will identify instance types
-- that support the specified minimum bandwidth, but the actual bandwidth
-- of your instance might go below the specified minimum at times. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-network-bandwidth.html#available-instance-bandwidth Available instance bandwidth>
-- in the /Amazon EC2 User Guide/.
--
-- /See:/ 'newNetworkBandwidthGbps' smart constructor.
data NetworkBandwidthGbps = NetworkBandwidthGbps'
  { -- | The maximum amount of network bandwidth, in Gbps. If this parameter is
    -- not specified, there is no maximum limit.
    max :: Prelude.Maybe Prelude.Double,
    -- | The minimum amount of network bandwidth, in Gbps. If this parameter is
    -- not specified, there is no minimum limit.
    min :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkBandwidthGbps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'networkBandwidthGbps_max' - The maximum amount of network bandwidth, in Gbps. If this parameter is
-- not specified, there is no maximum limit.
--
-- 'min', 'networkBandwidthGbps_min' - The minimum amount of network bandwidth, in Gbps. If this parameter is
-- not specified, there is no minimum limit.
newNetworkBandwidthGbps ::
  NetworkBandwidthGbps
newNetworkBandwidthGbps =
  NetworkBandwidthGbps'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum amount of network bandwidth, in Gbps. If this parameter is
-- not specified, there is no maximum limit.
networkBandwidthGbps_max :: Lens.Lens' NetworkBandwidthGbps (Prelude.Maybe Prelude.Double)
networkBandwidthGbps_max = Lens.lens (\NetworkBandwidthGbps' {max} -> max) (\s@NetworkBandwidthGbps' {} a -> s {max = a} :: NetworkBandwidthGbps)

-- | The minimum amount of network bandwidth, in Gbps. If this parameter is
-- not specified, there is no minimum limit.
networkBandwidthGbps_min :: Lens.Lens' NetworkBandwidthGbps (Prelude.Maybe Prelude.Double)
networkBandwidthGbps_min = Lens.lens (\NetworkBandwidthGbps' {min} -> min) (\s@NetworkBandwidthGbps' {} a -> s {min = a} :: NetworkBandwidthGbps)

instance Data.FromXML NetworkBandwidthGbps where
  parseXML x =
    NetworkBandwidthGbps'
      Prelude.<$> (x Data..@? "max") Prelude.<*> (x Data..@? "min")

instance Prelude.Hashable NetworkBandwidthGbps where
  hashWithSalt _salt NetworkBandwidthGbps' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData NetworkBandwidthGbps where
  rnf NetworkBandwidthGbps' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery NetworkBandwidthGbps where
  toQuery NetworkBandwidthGbps' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
