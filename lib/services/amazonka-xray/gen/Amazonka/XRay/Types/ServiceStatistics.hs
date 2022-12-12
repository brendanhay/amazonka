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
-- Module      : Amazonka.XRay.Types.ServiceStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ServiceStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.ErrorStatistics
import Amazonka.XRay.Types.FaultStatistics

-- | Response statistics for a service.
--
-- /See:/ 'newServiceStatistics' smart constructor.
data ServiceStatistics = ServiceStatistics'
  { -- | Information about requests that failed with a 4xx Client Error status
    -- code.
    errorStatistics :: Prelude.Maybe ErrorStatistics,
    -- | Information about requests that failed with a 5xx Server Error status
    -- code.
    faultStatistics :: Prelude.Maybe FaultStatistics,
    -- | The number of requests that completed with a 2xx Success status code.
    okCount :: Prelude.Maybe Prelude.Integer,
    -- | The total number of completed requests.
    totalCount :: Prelude.Maybe Prelude.Integer,
    -- | The aggregate response time of completed requests.
    totalResponseTime :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorStatistics', 'serviceStatistics_errorStatistics' - Information about requests that failed with a 4xx Client Error status
-- code.
--
-- 'faultStatistics', 'serviceStatistics_faultStatistics' - Information about requests that failed with a 5xx Server Error status
-- code.
--
-- 'okCount', 'serviceStatistics_okCount' - The number of requests that completed with a 2xx Success status code.
--
-- 'totalCount', 'serviceStatistics_totalCount' - The total number of completed requests.
--
-- 'totalResponseTime', 'serviceStatistics_totalResponseTime' - The aggregate response time of completed requests.
newServiceStatistics ::
  ServiceStatistics
newServiceStatistics =
  ServiceStatistics'
    { errorStatistics =
        Prelude.Nothing,
      faultStatistics = Prelude.Nothing,
      okCount = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      totalResponseTime = Prelude.Nothing
    }

-- | Information about requests that failed with a 4xx Client Error status
-- code.
serviceStatistics_errorStatistics :: Lens.Lens' ServiceStatistics (Prelude.Maybe ErrorStatistics)
serviceStatistics_errorStatistics = Lens.lens (\ServiceStatistics' {errorStatistics} -> errorStatistics) (\s@ServiceStatistics' {} a -> s {errorStatistics = a} :: ServiceStatistics)

-- | Information about requests that failed with a 5xx Server Error status
-- code.
serviceStatistics_faultStatistics :: Lens.Lens' ServiceStatistics (Prelude.Maybe FaultStatistics)
serviceStatistics_faultStatistics = Lens.lens (\ServiceStatistics' {faultStatistics} -> faultStatistics) (\s@ServiceStatistics' {} a -> s {faultStatistics = a} :: ServiceStatistics)

-- | The number of requests that completed with a 2xx Success status code.
serviceStatistics_okCount :: Lens.Lens' ServiceStatistics (Prelude.Maybe Prelude.Integer)
serviceStatistics_okCount = Lens.lens (\ServiceStatistics' {okCount} -> okCount) (\s@ServiceStatistics' {} a -> s {okCount = a} :: ServiceStatistics)

-- | The total number of completed requests.
serviceStatistics_totalCount :: Lens.Lens' ServiceStatistics (Prelude.Maybe Prelude.Integer)
serviceStatistics_totalCount = Lens.lens (\ServiceStatistics' {totalCount} -> totalCount) (\s@ServiceStatistics' {} a -> s {totalCount = a} :: ServiceStatistics)

-- | The aggregate response time of completed requests.
serviceStatistics_totalResponseTime :: Lens.Lens' ServiceStatistics (Prelude.Maybe Prelude.Double)
serviceStatistics_totalResponseTime = Lens.lens (\ServiceStatistics' {totalResponseTime} -> totalResponseTime) (\s@ServiceStatistics' {} a -> s {totalResponseTime = a} :: ServiceStatistics)

instance Data.FromJSON ServiceStatistics where
  parseJSON =
    Data.withObject
      "ServiceStatistics"
      ( \x ->
          ServiceStatistics'
            Prelude.<$> (x Data..:? "ErrorStatistics")
            Prelude.<*> (x Data..:? "FaultStatistics")
            Prelude.<*> (x Data..:? "OkCount")
            Prelude.<*> (x Data..:? "TotalCount")
            Prelude.<*> (x Data..:? "TotalResponseTime")
      )

instance Prelude.Hashable ServiceStatistics where
  hashWithSalt _salt ServiceStatistics' {..} =
    _salt `Prelude.hashWithSalt` errorStatistics
      `Prelude.hashWithSalt` faultStatistics
      `Prelude.hashWithSalt` okCount
      `Prelude.hashWithSalt` totalCount
      `Prelude.hashWithSalt` totalResponseTime

instance Prelude.NFData ServiceStatistics where
  rnf ServiceStatistics' {..} =
    Prelude.rnf errorStatistics
      `Prelude.seq` Prelude.rnf faultStatistics
      `Prelude.seq` Prelude.rnf okCount
      `Prelude.seq` Prelude.rnf totalCount
      `Prelude.seq` Prelude.rnf totalResponseTime
