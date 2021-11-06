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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerHealthCheck
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerHealthCheck where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the health checks that are conducted on the
-- load balancer.
--
-- /See:/ 'newAwsElbLoadBalancerHealthCheck' smart constructor.
data AwsElbLoadBalancerHealthCheck = AwsElbLoadBalancerHealthCheck'
  { -- | The number of consecutive health check successes required before the
    -- instance is moved to the Healthy state.
    healthyThreshold :: Prelude.Maybe Prelude.Int,
    -- | The approximate interval, in seconds, between health checks of an
    -- individual instance.
    interval :: Prelude.Maybe Prelude.Int,
    -- | The amount of time, in seconds, during which no response means a failed
    -- health check.
    timeout :: Prelude.Maybe Prelude.Int,
    -- | The number of consecutive health check failures that must occur before
    -- the instance is moved to the Unhealthy state.
    unhealthyThreshold :: Prelude.Maybe Prelude.Int,
    -- | The instance that is being checked. The target specifies the protocol
    -- and port. The available protocols are TCP, SSL, HTTP, and HTTPS. The
    -- range of valid ports is 1 through 65535.
    --
    -- For the HTTP and HTTPS protocols, the target also specifies the ping
    -- path.
    --
    -- For the TCP protocol, the target is specified as @TCP: \<port> @.
    --
    -- For the SSL protocol, the target is specified as @SSL.\<port> @.
    --
    -- For the HTTP and HTTPS protocols, the target is specified as
    -- @ \<protocol>:\<port>\/\<path to ping> @.
    target :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerHealthCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthyThreshold', 'awsElbLoadBalancerHealthCheck_healthyThreshold' - The number of consecutive health check successes required before the
-- instance is moved to the Healthy state.
--
-- 'interval', 'awsElbLoadBalancerHealthCheck_interval' - The approximate interval, in seconds, between health checks of an
-- individual instance.
--
-- 'timeout', 'awsElbLoadBalancerHealthCheck_timeout' - The amount of time, in seconds, during which no response means a failed
-- health check.
--
-- 'unhealthyThreshold', 'awsElbLoadBalancerHealthCheck_unhealthyThreshold' - The number of consecutive health check failures that must occur before
-- the instance is moved to the Unhealthy state.
--
-- 'target', 'awsElbLoadBalancerHealthCheck_target' - The instance that is being checked. The target specifies the protocol
-- and port. The available protocols are TCP, SSL, HTTP, and HTTPS. The
-- range of valid ports is 1 through 65535.
--
-- For the HTTP and HTTPS protocols, the target also specifies the ping
-- path.
--
-- For the TCP protocol, the target is specified as @TCP: \<port> @.
--
-- For the SSL protocol, the target is specified as @SSL.\<port> @.
--
-- For the HTTP and HTTPS protocols, the target is specified as
-- @ \<protocol>:\<port>\/\<path to ping> @.
newAwsElbLoadBalancerHealthCheck ::
  AwsElbLoadBalancerHealthCheck
newAwsElbLoadBalancerHealthCheck =
  AwsElbLoadBalancerHealthCheck'
    { healthyThreshold =
        Prelude.Nothing,
      interval = Prelude.Nothing,
      timeout = Prelude.Nothing,
      unhealthyThreshold = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | The number of consecutive health check successes required before the
-- instance is moved to the Healthy state.
awsElbLoadBalancerHealthCheck_healthyThreshold :: Lens.Lens' AwsElbLoadBalancerHealthCheck (Prelude.Maybe Prelude.Int)
awsElbLoadBalancerHealthCheck_healthyThreshold = Lens.lens (\AwsElbLoadBalancerHealthCheck' {healthyThreshold} -> healthyThreshold) (\s@AwsElbLoadBalancerHealthCheck' {} a -> s {healthyThreshold = a} :: AwsElbLoadBalancerHealthCheck)

-- | The approximate interval, in seconds, between health checks of an
-- individual instance.
awsElbLoadBalancerHealthCheck_interval :: Lens.Lens' AwsElbLoadBalancerHealthCheck (Prelude.Maybe Prelude.Int)
awsElbLoadBalancerHealthCheck_interval = Lens.lens (\AwsElbLoadBalancerHealthCheck' {interval} -> interval) (\s@AwsElbLoadBalancerHealthCheck' {} a -> s {interval = a} :: AwsElbLoadBalancerHealthCheck)

-- | The amount of time, in seconds, during which no response means a failed
-- health check.
awsElbLoadBalancerHealthCheck_timeout :: Lens.Lens' AwsElbLoadBalancerHealthCheck (Prelude.Maybe Prelude.Int)
awsElbLoadBalancerHealthCheck_timeout = Lens.lens (\AwsElbLoadBalancerHealthCheck' {timeout} -> timeout) (\s@AwsElbLoadBalancerHealthCheck' {} a -> s {timeout = a} :: AwsElbLoadBalancerHealthCheck)

-- | The number of consecutive health check failures that must occur before
-- the instance is moved to the Unhealthy state.
awsElbLoadBalancerHealthCheck_unhealthyThreshold :: Lens.Lens' AwsElbLoadBalancerHealthCheck (Prelude.Maybe Prelude.Int)
awsElbLoadBalancerHealthCheck_unhealthyThreshold = Lens.lens (\AwsElbLoadBalancerHealthCheck' {unhealthyThreshold} -> unhealthyThreshold) (\s@AwsElbLoadBalancerHealthCheck' {} a -> s {unhealthyThreshold = a} :: AwsElbLoadBalancerHealthCheck)

-- | The instance that is being checked. The target specifies the protocol
-- and port. The available protocols are TCP, SSL, HTTP, and HTTPS. The
-- range of valid ports is 1 through 65535.
--
-- For the HTTP and HTTPS protocols, the target also specifies the ping
-- path.
--
-- For the TCP protocol, the target is specified as @TCP: \<port> @.
--
-- For the SSL protocol, the target is specified as @SSL.\<port> @.
--
-- For the HTTP and HTTPS protocols, the target is specified as
-- @ \<protocol>:\<port>\/\<path to ping> @.
awsElbLoadBalancerHealthCheck_target :: Lens.Lens' AwsElbLoadBalancerHealthCheck (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerHealthCheck_target = Lens.lens (\AwsElbLoadBalancerHealthCheck' {target} -> target) (\s@AwsElbLoadBalancerHealthCheck' {} a -> s {target = a} :: AwsElbLoadBalancerHealthCheck)

instance Core.FromJSON AwsElbLoadBalancerHealthCheck where
  parseJSON =
    Core.withObject
      "AwsElbLoadBalancerHealthCheck"
      ( \x ->
          AwsElbLoadBalancerHealthCheck'
            Prelude.<$> (x Core..:? "HealthyThreshold")
            Prelude.<*> (x Core..:? "Interval")
            Prelude.<*> (x Core..:? "Timeout")
            Prelude.<*> (x Core..:? "UnhealthyThreshold")
            Prelude.<*> (x Core..:? "Target")
      )

instance
  Prelude.Hashable
    AwsElbLoadBalancerHealthCheck

instance Prelude.NFData AwsElbLoadBalancerHealthCheck

instance Core.ToJSON AwsElbLoadBalancerHealthCheck where
  toJSON AwsElbLoadBalancerHealthCheck' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HealthyThreshold" Core..=)
              Prelude.<$> healthyThreshold,
            ("Interval" Core..=) Prelude.<$> interval,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("UnhealthyThreshold" Core..=)
              Prelude.<$> unhealthyThreshold,
            ("Target" Core..=) Prelude.<$> target
          ]
      )
