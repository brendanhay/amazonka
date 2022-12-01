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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerConnectionDraining
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerConnectionDraining where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the connection draining configuration for the
-- load balancer.
--
-- /See:/ 'newAwsElbLoadBalancerConnectionDraining' smart constructor.
data AwsElbLoadBalancerConnectionDraining = AwsElbLoadBalancerConnectionDraining'
  { -- | The maximum time, in seconds, to keep the existing connections open
    -- before deregistering the instances.
    timeout :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether connection draining is enabled for the load balancer.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerConnectionDraining' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeout', 'awsElbLoadBalancerConnectionDraining_timeout' - The maximum time, in seconds, to keep the existing connections open
-- before deregistering the instances.
--
-- 'enabled', 'awsElbLoadBalancerConnectionDraining_enabled' - Indicates whether connection draining is enabled for the load balancer.
newAwsElbLoadBalancerConnectionDraining ::
  AwsElbLoadBalancerConnectionDraining
newAwsElbLoadBalancerConnectionDraining =
  AwsElbLoadBalancerConnectionDraining'
    { timeout =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | The maximum time, in seconds, to keep the existing connections open
-- before deregistering the instances.
awsElbLoadBalancerConnectionDraining_timeout :: Lens.Lens' AwsElbLoadBalancerConnectionDraining (Prelude.Maybe Prelude.Int)
awsElbLoadBalancerConnectionDraining_timeout = Lens.lens (\AwsElbLoadBalancerConnectionDraining' {timeout} -> timeout) (\s@AwsElbLoadBalancerConnectionDraining' {} a -> s {timeout = a} :: AwsElbLoadBalancerConnectionDraining)

-- | Indicates whether connection draining is enabled for the load balancer.
awsElbLoadBalancerConnectionDraining_enabled :: Lens.Lens' AwsElbLoadBalancerConnectionDraining (Prelude.Maybe Prelude.Bool)
awsElbLoadBalancerConnectionDraining_enabled = Lens.lens (\AwsElbLoadBalancerConnectionDraining' {enabled} -> enabled) (\s@AwsElbLoadBalancerConnectionDraining' {} a -> s {enabled = a} :: AwsElbLoadBalancerConnectionDraining)

instance
  Core.FromJSON
    AwsElbLoadBalancerConnectionDraining
  where
  parseJSON =
    Core.withObject
      "AwsElbLoadBalancerConnectionDraining"
      ( \x ->
          AwsElbLoadBalancerConnectionDraining'
            Prelude.<$> (x Core..:? "Timeout")
            Prelude.<*> (x Core..:? "Enabled")
      )

instance
  Prelude.Hashable
    AwsElbLoadBalancerConnectionDraining
  where
  hashWithSalt
    _salt
    AwsElbLoadBalancerConnectionDraining' {..} =
      _salt `Prelude.hashWithSalt` timeout
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    AwsElbLoadBalancerConnectionDraining
  where
  rnf AwsElbLoadBalancerConnectionDraining' {..} =
    Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf enabled

instance
  Core.ToJSON
    AwsElbLoadBalancerConnectionDraining
  where
  toJSON AwsElbLoadBalancerConnectionDraining' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Timeout" Core..=) Prelude.<$> timeout,
            ("Enabled" Core..=) Prelude.<$> enabled
          ]
      )
