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
-- Module      : Amazonka.OpenSearch.Types.ZoneAwarenessConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ZoneAwarenessConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The zone awareness configuration for an Amazon OpenSearch Service
-- domain.
--
-- /See:/ 'newZoneAwarenessConfig' smart constructor.
data ZoneAwarenessConfig = ZoneAwarenessConfig'
  { -- | If you enabled multiple Availability Zones, this value is the number of
    -- zones that you want the domain to use. Valid values are @2@ and @3@. If
    -- your domain is provisioned within a VPC, this value be equal to number
    -- of subnets.
    availabilityZoneCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZoneAwarenessConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneCount', 'zoneAwarenessConfig_availabilityZoneCount' - If you enabled multiple Availability Zones, this value is the number of
-- zones that you want the domain to use. Valid values are @2@ and @3@. If
-- your domain is provisioned within a VPC, this value be equal to number
-- of subnets.
newZoneAwarenessConfig ::
  ZoneAwarenessConfig
newZoneAwarenessConfig =
  ZoneAwarenessConfig'
    { availabilityZoneCount =
        Prelude.Nothing
    }

-- | If you enabled multiple Availability Zones, this value is the number of
-- zones that you want the domain to use. Valid values are @2@ and @3@. If
-- your domain is provisioned within a VPC, this value be equal to number
-- of subnets.
zoneAwarenessConfig_availabilityZoneCount :: Lens.Lens' ZoneAwarenessConfig (Prelude.Maybe Prelude.Int)
zoneAwarenessConfig_availabilityZoneCount = Lens.lens (\ZoneAwarenessConfig' {availabilityZoneCount} -> availabilityZoneCount) (\s@ZoneAwarenessConfig' {} a -> s {availabilityZoneCount = a} :: ZoneAwarenessConfig)

instance Core.FromJSON ZoneAwarenessConfig where
  parseJSON =
    Core.withObject
      "ZoneAwarenessConfig"
      ( \x ->
          ZoneAwarenessConfig'
            Prelude.<$> (x Core..:? "AvailabilityZoneCount")
      )

instance Prelude.Hashable ZoneAwarenessConfig where
  hashWithSalt _salt ZoneAwarenessConfig' {..} =
    _salt `Prelude.hashWithSalt` availabilityZoneCount

instance Prelude.NFData ZoneAwarenessConfig where
  rnf ZoneAwarenessConfig' {..} =
    Prelude.rnf availabilityZoneCount

instance Core.ToJSON ZoneAwarenessConfig where
  toJSON ZoneAwarenessConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AvailabilityZoneCount" Core..=)
              Prelude.<$> availabilityZoneCount
          ]
      )
