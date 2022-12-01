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
-- Module      : Amazonka.XRay.Types.RequestImpactStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.RequestImpactStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Statistics that describe how the incident has impacted a service.
--
-- /See:/ 'newRequestImpactStatistics' smart constructor.
data RequestImpactStatistics = RequestImpactStatistics'
  { -- | The number of successful requests.
    okCount :: Prelude.Maybe Prelude.Integer,
    -- | The total number of requests to the service.
    totalCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of requests that have resulted in a fault,
    faultCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'totalCount', 'requestImpactStatistics_totalCount' - The total number of requests to the service.
--
-- 'faultCount', 'requestImpactStatistics_faultCount' - The number of requests that have resulted in a fault,
newRequestImpactStatistics ::
  RequestImpactStatistics
newRequestImpactStatistics =
  RequestImpactStatistics'
    { okCount = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      faultCount = Prelude.Nothing
    }

-- | The number of successful requests.
requestImpactStatistics_okCount :: Lens.Lens' RequestImpactStatistics (Prelude.Maybe Prelude.Integer)
requestImpactStatistics_okCount = Lens.lens (\RequestImpactStatistics' {okCount} -> okCount) (\s@RequestImpactStatistics' {} a -> s {okCount = a} :: RequestImpactStatistics)

-- | The total number of requests to the service.
requestImpactStatistics_totalCount :: Lens.Lens' RequestImpactStatistics (Prelude.Maybe Prelude.Integer)
requestImpactStatistics_totalCount = Lens.lens (\RequestImpactStatistics' {totalCount} -> totalCount) (\s@RequestImpactStatistics' {} a -> s {totalCount = a} :: RequestImpactStatistics)

-- | The number of requests that have resulted in a fault,
requestImpactStatistics_faultCount :: Lens.Lens' RequestImpactStatistics (Prelude.Maybe Prelude.Integer)
requestImpactStatistics_faultCount = Lens.lens (\RequestImpactStatistics' {faultCount} -> faultCount) (\s@RequestImpactStatistics' {} a -> s {faultCount = a} :: RequestImpactStatistics)

instance Core.FromJSON RequestImpactStatistics where
  parseJSON =
    Core.withObject
      "RequestImpactStatistics"
      ( \x ->
          RequestImpactStatistics'
            Prelude.<$> (x Core..:? "OkCount")
            Prelude.<*> (x Core..:? "TotalCount")
            Prelude.<*> (x Core..:? "FaultCount")
      )

instance Prelude.Hashable RequestImpactStatistics where
  hashWithSalt _salt RequestImpactStatistics' {..} =
    _salt `Prelude.hashWithSalt` okCount
      `Prelude.hashWithSalt` totalCount
      `Prelude.hashWithSalt` faultCount

instance Prelude.NFData RequestImpactStatistics where
  rnf RequestImpactStatistics' {..} =
    Prelude.rnf okCount
      `Prelude.seq` Prelude.rnf totalCount
      `Prelude.seq` Prelude.rnf faultCount
