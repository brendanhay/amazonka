{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.XRay.Types.ServiceStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ServiceStatistics where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.XRay.Types.ErrorStatistics
import Network.AWS.XRay.Types.FaultStatistics

-- | Response statistics for a service.
--
-- /See:/ 'newServiceStatistics' smart constructor.
data ServiceStatistics = ServiceStatistics'
  { -- | The aggregate response time of completed requests.
    totalResponseTime :: Prelude.Maybe Prelude.Double,
    -- | The number of requests that completed with a 2xx Success status code.
    okCount :: Prelude.Maybe Prelude.Integer,
    -- | Information about requests that failed with a 5xx Server Error status
    -- code.
    faultStatistics :: Prelude.Maybe FaultStatistics,
    -- | The total number of completed requests.
    totalCount :: Prelude.Maybe Prelude.Integer,
    -- | Information about requests that failed with a 4xx Client Error status
    -- code.
    errorStatistics :: Prelude.Maybe ErrorStatistics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalResponseTime', 'serviceStatistics_totalResponseTime' - The aggregate response time of completed requests.
--
-- 'okCount', 'serviceStatistics_okCount' - The number of requests that completed with a 2xx Success status code.
--
-- 'faultStatistics', 'serviceStatistics_faultStatistics' - Information about requests that failed with a 5xx Server Error status
-- code.
--
-- 'totalCount', 'serviceStatistics_totalCount' - The total number of completed requests.
--
-- 'errorStatistics', 'serviceStatistics_errorStatistics' - Information about requests that failed with a 4xx Client Error status
-- code.
newServiceStatistics ::
  ServiceStatistics
newServiceStatistics =
  ServiceStatistics'
    { totalResponseTime =
        Prelude.Nothing,
      okCount = Prelude.Nothing,
      faultStatistics = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      errorStatistics = Prelude.Nothing
    }

-- | The aggregate response time of completed requests.
serviceStatistics_totalResponseTime :: Lens.Lens' ServiceStatistics (Prelude.Maybe Prelude.Double)
serviceStatistics_totalResponseTime = Lens.lens (\ServiceStatistics' {totalResponseTime} -> totalResponseTime) (\s@ServiceStatistics' {} a -> s {totalResponseTime = a} :: ServiceStatistics)

-- | The number of requests that completed with a 2xx Success status code.
serviceStatistics_okCount :: Lens.Lens' ServiceStatistics (Prelude.Maybe Prelude.Integer)
serviceStatistics_okCount = Lens.lens (\ServiceStatistics' {okCount} -> okCount) (\s@ServiceStatistics' {} a -> s {okCount = a} :: ServiceStatistics)

-- | Information about requests that failed with a 5xx Server Error status
-- code.
serviceStatistics_faultStatistics :: Lens.Lens' ServiceStatistics (Prelude.Maybe FaultStatistics)
serviceStatistics_faultStatistics = Lens.lens (\ServiceStatistics' {faultStatistics} -> faultStatistics) (\s@ServiceStatistics' {} a -> s {faultStatistics = a} :: ServiceStatistics)

-- | The total number of completed requests.
serviceStatistics_totalCount :: Lens.Lens' ServiceStatistics (Prelude.Maybe Prelude.Integer)
serviceStatistics_totalCount = Lens.lens (\ServiceStatistics' {totalCount} -> totalCount) (\s@ServiceStatistics' {} a -> s {totalCount = a} :: ServiceStatistics)

-- | Information about requests that failed with a 4xx Client Error status
-- code.
serviceStatistics_errorStatistics :: Lens.Lens' ServiceStatistics (Prelude.Maybe ErrorStatistics)
serviceStatistics_errorStatistics = Lens.lens (\ServiceStatistics' {errorStatistics} -> errorStatistics) (\s@ServiceStatistics' {} a -> s {errorStatistics = a} :: ServiceStatistics)

instance Prelude.FromJSON ServiceStatistics where
  parseJSON =
    Prelude.withObject
      "ServiceStatistics"
      ( \x ->
          ServiceStatistics'
            Prelude.<$> (x Prelude..:? "TotalResponseTime")
            Prelude.<*> (x Prelude..:? "OkCount")
            Prelude.<*> (x Prelude..:? "FaultStatistics")
            Prelude.<*> (x Prelude..:? "TotalCount")
            Prelude.<*> (x Prelude..:? "ErrorStatistics")
      )

instance Prelude.Hashable ServiceStatistics

instance Prelude.NFData ServiceStatistics
