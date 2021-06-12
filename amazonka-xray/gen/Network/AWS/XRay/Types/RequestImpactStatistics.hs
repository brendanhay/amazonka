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
-- Module      : Network.AWS.XRay.Types.RequestImpactStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.RequestImpactStatistics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Statistics that describe how the incident has impacted a service.
--
-- /See:/ 'newRequestImpactStatistics' smart constructor.
data RequestImpactStatistics = RequestImpactStatistics'
  { -- | The number of successful requests.
    okCount :: Core.Maybe Core.Integer,
    -- | The number of requests that have resulted in a fault,
    faultCount :: Core.Maybe Core.Integer,
    -- | The total number of requests to the service.
    totalCount :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RequestImpactStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'okCount', 'requestImpactStatistics_okCount' - The number of successful requests.
--
-- 'faultCount', 'requestImpactStatistics_faultCount' - The number of requests that have resulted in a fault,
--
-- 'totalCount', 'requestImpactStatistics_totalCount' - The total number of requests to the service.
newRequestImpactStatistics ::
  RequestImpactStatistics
newRequestImpactStatistics =
  RequestImpactStatistics'
    { okCount = Core.Nothing,
      faultCount = Core.Nothing,
      totalCount = Core.Nothing
    }

-- | The number of successful requests.
requestImpactStatistics_okCount :: Lens.Lens' RequestImpactStatistics (Core.Maybe Core.Integer)
requestImpactStatistics_okCount = Lens.lens (\RequestImpactStatistics' {okCount} -> okCount) (\s@RequestImpactStatistics' {} a -> s {okCount = a} :: RequestImpactStatistics)

-- | The number of requests that have resulted in a fault,
requestImpactStatistics_faultCount :: Lens.Lens' RequestImpactStatistics (Core.Maybe Core.Integer)
requestImpactStatistics_faultCount = Lens.lens (\RequestImpactStatistics' {faultCount} -> faultCount) (\s@RequestImpactStatistics' {} a -> s {faultCount = a} :: RequestImpactStatistics)

-- | The total number of requests to the service.
requestImpactStatistics_totalCount :: Lens.Lens' RequestImpactStatistics (Core.Maybe Core.Integer)
requestImpactStatistics_totalCount = Lens.lens (\RequestImpactStatistics' {totalCount} -> totalCount) (\s@RequestImpactStatistics' {} a -> s {totalCount = a} :: RequestImpactStatistics)

instance Core.FromJSON RequestImpactStatistics where
  parseJSON =
    Core.withObject
      "RequestImpactStatistics"
      ( \x ->
          RequestImpactStatistics'
            Core.<$> (x Core..:? "OkCount")
            Core.<*> (x Core..:? "FaultCount")
            Core.<*> (x Core..:? "TotalCount")
      )

instance Core.Hashable RequestImpactStatistics

instance Core.NFData RequestImpactStatistics
