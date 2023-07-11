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
-- Module      : Amazonka.AutoScaling.Types.NetworkBandwidthGbpsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.NetworkBandwidthGbpsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the minimum and maximum for the @NetworkBandwidthGbps@ object
-- when you specify InstanceRequirements for an Auto Scaling group.
--
-- Setting the minimum bandwidth does not guarantee that your instance will
-- achieve the minimum bandwidth. Amazon EC2 will identify instance types
-- that support the specified minimum bandwidth, but the actual bandwidth
-- of your instance might go below the specified minimum at times. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-network-bandwidth.html#available-instance-bandwidth Available instance bandwidth>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- /See:/ 'newNetworkBandwidthGbpsRequest' smart constructor.
data NetworkBandwidthGbpsRequest = NetworkBandwidthGbpsRequest'
  { -- | The maximum amount of network bandwidth, in gigabits per second (Gbps).
    max :: Prelude.Maybe Prelude.Double,
    -- | The minimum amount of network bandwidth, in gigabits per second (Gbps).
    min :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkBandwidthGbpsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'networkBandwidthGbpsRequest_max' - The maximum amount of network bandwidth, in gigabits per second (Gbps).
--
-- 'min', 'networkBandwidthGbpsRequest_min' - The minimum amount of network bandwidth, in gigabits per second (Gbps).
newNetworkBandwidthGbpsRequest ::
  NetworkBandwidthGbpsRequest
newNetworkBandwidthGbpsRequest =
  NetworkBandwidthGbpsRequest'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum amount of network bandwidth, in gigabits per second (Gbps).
networkBandwidthGbpsRequest_max :: Lens.Lens' NetworkBandwidthGbpsRequest (Prelude.Maybe Prelude.Double)
networkBandwidthGbpsRequest_max = Lens.lens (\NetworkBandwidthGbpsRequest' {max} -> max) (\s@NetworkBandwidthGbpsRequest' {} a -> s {max = a} :: NetworkBandwidthGbpsRequest)

-- | The minimum amount of network bandwidth, in gigabits per second (Gbps).
networkBandwidthGbpsRequest_min :: Lens.Lens' NetworkBandwidthGbpsRequest (Prelude.Maybe Prelude.Double)
networkBandwidthGbpsRequest_min = Lens.lens (\NetworkBandwidthGbpsRequest' {min} -> min) (\s@NetworkBandwidthGbpsRequest' {} a -> s {min = a} :: NetworkBandwidthGbpsRequest)

instance Data.FromXML NetworkBandwidthGbpsRequest where
  parseXML x =
    NetworkBandwidthGbpsRequest'
      Prelude.<$> (x Data..@? "Max")
      Prelude.<*> (x Data..@? "Min")

instance Prelude.Hashable NetworkBandwidthGbpsRequest where
  hashWithSalt _salt NetworkBandwidthGbpsRequest' {..} =
    _salt
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData NetworkBandwidthGbpsRequest where
  rnf NetworkBandwidthGbpsRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery NetworkBandwidthGbpsRequest where
  toQuery NetworkBandwidthGbpsRequest' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
