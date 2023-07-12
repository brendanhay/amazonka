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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerCrossZoneLoadBalancing
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerCrossZoneLoadBalancing where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains cross-zone load balancing settings for the load balancer.
--
-- /See:/ 'newAwsElbLoadBalancerCrossZoneLoadBalancing' smart constructor.
data AwsElbLoadBalancerCrossZoneLoadBalancing = AwsElbLoadBalancerCrossZoneLoadBalancing'
  { -- | Indicates whether cross-zone load balancing is enabled for the load
    -- balancer.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerCrossZoneLoadBalancing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'awsElbLoadBalancerCrossZoneLoadBalancing_enabled' - Indicates whether cross-zone load balancing is enabled for the load
-- balancer.
newAwsElbLoadBalancerCrossZoneLoadBalancing ::
  AwsElbLoadBalancerCrossZoneLoadBalancing
newAwsElbLoadBalancerCrossZoneLoadBalancing =
  AwsElbLoadBalancerCrossZoneLoadBalancing'
    { enabled =
        Prelude.Nothing
    }

-- | Indicates whether cross-zone load balancing is enabled for the load
-- balancer.
awsElbLoadBalancerCrossZoneLoadBalancing_enabled :: Lens.Lens' AwsElbLoadBalancerCrossZoneLoadBalancing (Prelude.Maybe Prelude.Bool)
awsElbLoadBalancerCrossZoneLoadBalancing_enabled = Lens.lens (\AwsElbLoadBalancerCrossZoneLoadBalancing' {enabled} -> enabled) (\s@AwsElbLoadBalancerCrossZoneLoadBalancing' {} a -> s {enabled = a} :: AwsElbLoadBalancerCrossZoneLoadBalancing)

instance
  Data.FromJSON
    AwsElbLoadBalancerCrossZoneLoadBalancing
  where
  parseJSON =
    Data.withObject
      "AwsElbLoadBalancerCrossZoneLoadBalancing"
      ( \x ->
          AwsElbLoadBalancerCrossZoneLoadBalancing'
            Prelude.<$> (x Data..:? "Enabled")
      )

instance
  Prelude.Hashable
    AwsElbLoadBalancerCrossZoneLoadBalancing
  where
  hashWithSalt
    _salt
    AwsElbLoadBalancerCrossZoneLoadBalancing' {..} =
      _salt `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    AwsElbLoadBalancerCrossZoneLoadBalancing
  where
  rnf AwsElbLoadBalancerCrossZoneLoadBalancing' {..} =
    Prelude.rnf enabled

instance
  Data.ToJSON
    AwsElbLoadBalancerCrossZoneLoadBalancing
  where
  toJSON AwsElbLoadBalancerCrossZoneLoadBalancing' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Enabled" Data..=) Prelude.<$> enabled]
      )
